# H2o Modeling ----

# Time Series ML 
library(tidymodels)
library(modeltime.h2o)
library(h2o)
library(fs)
library(stringr)
library(forcats)
library(cowplot)
library(glue)

# Core
library(readxl)
library(tidyverse)
library(lubridate)
library(timetk)

Q <- "Provider Support"

# Functions
sys.source("./00_Scripts/functions.R", envir = env <- new.env())

# Load Data ----
## Phone_Data

daily_tbl <- env[["amazon_phone"]]()

## Cx Data
cx <- env[["customer_count_data"]]("") 
 
cx_up <- env[["cx_up_daily"]](1.3)

## Holiday tbl  
Holidays_tbl <- env[["holidays_tbl"]](2022:2024)


# Data Transformation ----
daily_cleaned_tbl <- env[["daily_cleaned_tbl"]](daily_tbl, Q)

## Chart Cleaned Tbl
daily_cleaned_tbl %>% 
  select(Date, NCO_trans, NCO_trans_clean, NCO_trans_clean_drop) %>% 
  pivot_longer(-Date) %>% 
  plot_time_series(.date_var = Date, .value = value, .color_var = name, .smooth = F)


#Prepare Data ----
## Forecasting Horizon and Lag ----
horizon    <- as.numeric(as.Date('2024-12-31')-as.Date(max(daily_cleaned_tbl$Date)))
lag_period <- as.numeric(as.Date('2024-12-31')-as.Date(max(daily_cleaned_tbl$Date)))

## Table Prepare
data_prepared_full_tbl <- env[["data_prepared_full_tbl"]](daily_cleaned_tbl, cx_up)

## Table View
data_prepared_full_tbl %>%
  plot_time_series(Date, NCO_trans_clean_drop, Queue, .smooth = FALSE)


## Historical to Current tbl and Fcst Tbl ----
data_prepared_current_tbl <- data_prepared_full_tbl %>%
  filter(!is.na(NCO_trans_clean_drop)) %>% 
  select(-Queue,-`Offered Calls`, -NCO_trans,-NCO_trans_clean)

forecast_tbl <- data_prepared_full_tbl %>%
  filter(is.na(NCO_trans_clean_drop)) %>% 
  select(-Queue, -NCO_trans,-NCO_trans_clean)

## Tbl Split for Training ----
splits <- time_series_split(data_prepared_current_tbl, assess = "3 months", cumulative = TRUE)

## View Split
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, NCO_trans_clean_drop)

## Recipe for Forecast modeling ----- 
recipe_act_spec_base <- recipes::recipe(NCO_trans_clean_drop ~ ., data = training(splits)) %>%

  # Time Series Signature
  step_timeseries_signature(Date) %>%
  recipes::step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  
  # Standardization
  recipes::step_normalize(matches("(index.num)|(year)|(yday)"))
  
  # Dummy Encoding (One Hot Encoding)
  # step_dummy(all_nominal(), one_hot = TRUE)
?step_dummy
# Interaction
# step_interact(~ matches("week2") * matches("wday.lbl")) %>%

# Fourier
# step_fourier(date_rounded, period = c(7, 14, 30, 90, 365), K = 2)

recipe_act_spec_base %>% prep() %>% recipes::juice() %>% glimpse()

# H2O Modeling ----
h2o.init()

model_spec_h2o <-  automl_reg()%>% 
  set_engine(
    engine            = 'h2o',
    max_runtime_secs  = 30,
    max_runtime_secs_per_model = 10,
    max_models =30,
    nfolds = 10,
    exclude_algos = c("DeepLearning"),
    verbosity = NULL,
    seed =   1234
  )


workflow_fit_h2o <- workflow() %>%
  add_model(model_spec_h2o) %>%
  add_recipe(recipe_act_spec_base) %>%
  fit(training(splits))


workflow_fit_h2o %>% automl_leaderboard() %>% view()

## Models results ----
calibration_tbl <- modeltime_table(
  workflow_fit_h2o
) %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
  modeltime_accuracy() %>% table_modeltime_accuracy()

calibration_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data_prepared_current_tbl,
    keep_data = T
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = F)

## Refit Data ----
refit_tbl <- calibration_tbl %>% 
  modeltime_refit(data_prepared_current_tbl)

refit_tbl %>% 
  modeltime_forecast(new_data = forecast_tbl, actual_data = data_prepared_current_tbl, keep_data = T) %>% 
  plot_modeltime_forecast(.conf_interval_show = F)

Models_Forecast <- refit_tbl %>%
  modeltime_forecast(
    new_data = forecast_tbl,
    actual_data = data_prepared_current_tbl, keep_data = T
  ) %>% 
  group_by(Queue) %>%
  # Invert Transformation
  mutate(across(.value:.conf_hi, .fns = ~ expm1(.))) 


# h2o.shutdown()

# Export Data ----
current_datetime <- format(Sys.time(), "%Y-%m-%d_%H:%M")
filename <- paste("h20_Provider_Phone", current_datetime, ".csv", sep = "_")
output_directory <- "./00_Data/Provider_Forecasts/Phone/"
file_path <- paste0(output_directory, filename)

write.csv(Models_Forecast, file = file_path)

