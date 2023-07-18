sys.source("./00_Scripts/functions.R", envir = env <- new.env())
library(dplyr)



# Paramaters:
        CS <- "Customer Support"
        PS <- "Provider Support"

## Load Data ----
### Phone 
daily_phone_tbl <- env[["amazon_phone"]]()

daily_customer_count_tbl <- env[["daily_customer_count_tbl"]]()

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

