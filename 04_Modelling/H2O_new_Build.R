sys.source("./00_Scripts/functions.R", envir = env <- new.env())
library(dplyr)
library(h2o)
library(modeltime.h2o)

# Paramaters:
        CS <- "Customer Support"
        PS <- "Provider Support"

## Load Data ----
### Phone 
daily_phone_tbl <- env[["amazon_phone"]]()

daily_customer_count_tbl <- env[["daily_customer_count_tbl"]](path = "G:\\Shared drives\\WFM\\Customer_Count\\LS Customer Count 7.18.23.xlsx")

Holidays_tbl <- env[["Holidays_tbl"]](2022:2024)

# Data Transformation ----

## Clean files and add log transformation to smooth any anomalies 

CS_cleaned_tbl <- env[["cleaned_tbl"]](holiday = Holidays_tbl) %>% 
  dplyr::filter(`Queue` == CS) %>% 
  dplyr::select(-c(`Queue`,`Offered Calls`)) %>% 
  tidyr::pivot_longer(-Date)

PS_cleaned_tbl <- env[["cleaned_tbl"]](holiday = Holidays_tbl) %>% 
  dplyr::filter(`Queue` == PS) %>% 
  dplyr::select(-c(`Queue`,`Offered Calls`)) %>% 
  tidyr::pivot_longer(-Date)

## View in Charts

CS_Chart_Cleaned <- CS_cleaned_tbl %>% 
  timetk::plot_time_series(.date_var = Date, 
                           .value = value,
                           .color_var = name, 
                           .smooth = F)

PS_Chart_Cleaned <- PS_cleaned_tbl %>% 
  timetk::plot_time_series(.date_var = Date,
                           .value = value, 
                           .color_var = name, 
                           .smooth = F)

plotly::subplot(CS_Chart_Cleaned,PS_Chart_Cleaned, nrows = 2)


        horizon    <- as.numeric(as.Date('2024-12-31')-as.Date(max(CS_cleaned_tbl$Date)))
        lag_period <- as.numeric(as.Date('2024-12-31')-as.Date(max(CS_cleaned_tbl$Date)))

# Prep Tables----
CS_prepared <- env[["prepared_tbl"]](data = CS_cleaned_tbl,
                                     holiday = Holidays_tbl,
                                     customer = daily_customer_count_tbl)

PS_prepared <- env[["prepared_tbl"]](data = PS_cleaned_tbl,
                                     holiday = Holidays_tbl,
                                     customer = daily_customer_count_tbl)


## Historical to Current tbl and Fcst Tbl ----
CS_current_tbl <- CS_prepared %>%
  filter(!is.na(NCO_trans_clean_drop)) %>% 
  select(-NCO_trans,-NCO_trans_clean)

PS_current_tbl <- PS_prepared %>%
  filter(!is.na(NCO_trans_clean_drop)) %>% 
  select(-NCO_trans,-NCO_trans_clean)

CS_forecast_tbl <- CS_prepared %>%
  filter(is.na(NCO_trans_clean_drop)) %>% 
  select(-NCO_trans,-NCO_trans_clean)

PS_forecast_tbl <- PS_prepared %>%
  filter(is.na(NCO_trans_clean_drop)) %>% 
  select(-NCO_trans,-NCO_trans_clean)

## Tbl Split for Training ----
CS_Splits <- timetk::time_series_split(CS_current_tbl, assess = "11 weeks", cumulative = TRUE)
PS_Splits <- timetk::time_series_split(PS_current_tbl, assess = "11 weeks", cumulative = TRUE)


## View Split
CS_Chart_Splits <- CS_Splits %>%
  timetk::tk_time_series_cv_plan() %>%
  timetk::plot_time_series_cv_plan(Date, NCO_trans_clean_drop)

PS_Chart_Splits <- PS_Splits %>%
  timetk::tk_time_series_cv_plan() %>%
  timetk::plot_time_series_cv_plan(Date, NCO_trans_clean_drop)

plotly::subplot(CS_Chart_Splits,PS_Chart_Splits, nrows = 2)


## Recipe for Forecast modeling ----- 

CS_recipe <- recipes::recipe(NCO_trans_clean_drop ~ ., data = rsample::training(CS_Splits)) %>%
    
    # Time Series Signature
    timetk::step_timeseries_signature(Date) %>%
    recipes::step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
    
    # Standardization
    recipes::step_normalize(matches("(index.num)|(year)|(yday)"))

PS_recipe <- recipes::recipe(NCO_trans_clean_drop ~ ., data = rsample::training(PS_Splits)) %>%
  
  # Time Series Signature
  timetk::step_timeseries_signature(Date) %>%
  recipes::step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  
  # Standardization
  recipes::step_normalize(matches("(index.num)|(year)|(yday)"))

CS_recipe %>% recipes::prep() %>% recipes::juice() %>% glimpse()

# H2O Modeling ----

h2o.init()

model_spec_h2o <-  modeltime.h2o::automl_reg()%>% 
  parsnip::set_engine(
    engine            = 'h2o',
    max_runtime_secs  = 30,
    max_runtime_secs_per_model = 10,
    max_models =30,
    nfolds = 10,
    exclude_algos = c("DeepLearning"),
    verbosity = NULL,
    seed =   1234
  )

## Models ----
CS_workflow <- workflows::workflow() %>%
  workflows::add_model(model_spec_h2o) %>%
  workflows::add_recipe(CS_recipe) %>%
  parsnip::fit(rsample::training(CS_Splits))

PS_workflow <- workflows::workflow() %>%
  workflows::add_model(model_spec_h2o) %>%
  workflows::add_recipe(PS_recipe) %>%
  parsnip::fit(rsample::training(PS_Splits))

?workflow

## Models results

CS_accuarcy <- env[["accuracy_tbl"]](CS_workflow,CS_Splits)

PS_accuarcy <- env[["accuracy_tbl"]](PS_workflow,PS_Splits)

bind_rows(
  CS_accuarcy %>% dplyr::mutate(Model = "CS"),
  PS_accuarcy %>% dplyr::mutate(Model = "PS")
) %>%  modeltime::table_modeltime_accuracy()

## Chart Results 

plotly::subplot(
  env[["accuracy_chart"]](workflow = CS_workflow, splits = CS_Splits, current_tbl = CS_current_tbl),
  env[["accuracy_chart"]](workflow = PS_workflow, splits = PS_Splits, current_tbl = PS_current_tbl),
  nrows = 2
)


## Refit Data ----

CS_refit_tbl <- env[["refit_tbl"]](workflow = CS_workflow,splits = CS_Splits,current_tbl = CS_current_tbl)

PS_refit_tbl <- env[["refit_tbl"]](workflow = PS_workflow,splits = PS_Splits,current_tbl = PS_current_tbl)

env[["refit_chart"]](refit = CS_refit_tbl,current_tbl = CS_current_tbl, forecast_tbl = CS_forecast_tbl)

env[["refit_chart"]](refit = PS_refit_tbl,current_tbl = PS_current_tbl, forecast_tbl = PS_forecast_tbl)

# Forecast Complete ----

CS_forecast_complete <- env[["Invert_forecast_tbl"]](refit = CS_refit_tbl,current_tbl = CS_current_tbl,forecast_tbl = CS_forecast_tbl)

CS_forecast_complete %>% modeltime::plot_modeltime_forecast(.conf_interval_show = F)


PS_forecast_complete <- env[["Invert_forecast_tbl"]](refit = PS_refit_tbl,current_tbl = PS_current_tbl,forecast_tbl = PS_forecast_tbl)

PS_forecast_complete %>% modeltime::plot_modeltime_forecast(.conf_interval_show = F)

# Export ----

## CS
current_datetime <- format(Sys.time(), "%Y-%m-%d_%H_%M")
CS_filename <- paste("h20_Customer_Phone", current_datetime, ".csv", sep = "_")
CS_output_directory <- "./00_Data/Customer_Forecasts/Phone/"
CS_file_path <- paste0(CS_output_directory, CS_filename)

write.csv(PS_forecast_complete, file = CS_file_path)

## Provider
PS_filename <- paste("h20_Provider_Phone", current_datetime, ".csv", sep = "_")
PS_output_directory <- "./00_Data/Provider_Forecasts/Phone/"
PS_file_path <- paste0(PS_output_directory, PS_filename)



write.csv(PS_forecast_complete, file = PS_file_path)












