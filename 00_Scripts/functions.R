
# Misc----
Holidays_tbl <-function(year) {
  timetk::tk_get_holidays_by_year(year) %>%
  filter(holiday_name %>% stringr::str_detect("US_")) %>%
  filter(holiday_name %>% stringr::str_detect("US_CPulaskisBirthday|US_InaugurationDay|US_ElectionDay|US_DecorationMemorialDay", negate = T)) %>%
  dplyr::select(-locale)}


# customer count ----
  
customer_count_data <- function(path = "", sheet = "", col_types = "", ...) {
  if (!require("dplyr")) {
    stop("dplyr package not found.")
  }
  
  path <- dplyr::case_when(
    path == "" ~ "G:\\Shared drives\\WFM\\Customer_Count\\LS Customer Count 7.11.23.xlsx",
    TRUE ~ path
  )
  
  sheet <- dplyr::case_when(
    sheet == "" ~ "Customer_Count",
    TRUE ~ sheet
  )
  
  col_types <- dplyr::case_when(
    col_types == "" ~ c("date", "numeric", "numeric"),
    TRUE ~ col_types
  )
  
  data <- readxl::read_excel(path = path, sheet = sheet, col_types = col_types, ...)
  
  data <- data %>%
    mutate(Month = lubridate::month(Date, label = T))
  
  return(data)
}

daily_customer_count_tbl <- function(){
  # Load Data
  env[["customer_count_data"]]() %>% 
    
    # Expand to Dialy
    dplyr::group_by(Year) %>% 
    tidyr::expand(Date = seq(min(Date),
                             max(Date)+lubridate::days(30),
                             by = "day")) %>%
    dplyr::mutate(Month = format(Date, "%b"), 
                  DIM = lubridate::days_in_month(Date)) %>%
    
    # Bring in Customer Count data and divid by day to get daily  
    left_join(env[["customer_count_data"]]() %>% dplyr::select(-Date), 
              by = c("Year"="Year", 
                     "Month"="Month")) %>%
    dplyr::mutate(Customer_Count = round(Customer_Count/DIM))
}

year_update <- function(data, year, percentage) {
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    message("Package 'tidyr' not found. Loading 'tidyr' package...")
    library(tidyr)
  }
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    message("Package 'stringr' not found. Loading 'stringr' package...")
    library(stringr)
  }
  
  updated_data <- data %>%
    mutate(Date = month(Date)) %>%
    group_by(Date) %>%
    tidyr::pivot_wider(names_from = Year, values_from = Customer_Count, values_fill = 0) %>%
    mutate(!!paste0("Updated_", year) := !!sym(paste0(year)) * (1 + percentage/100), Day = 01) %>%
    tidyr::pivot_longer(cols = c(-Date, -Day), names_to = "Group", values_to = "Customer_Count") %>%
    mutate(Year = case_when(
      str_detect(Group, paste0("Updated_", year)) ~ year,
      TRUE ~ as.numeric(Group))
    ) %>%
    mutate(Date = as.Date(paste0(Year, "-", Date, "-", Day))) %>%
    select(Date, Group, Customer_Count)
  
  return(updated_data)
}

cx_up <- function(data,value) {
  data |> 
    mutate(Date = month(Date, label = T)) |> 
    group_by(Date) |> 
    pivot_wider(names_from = Year, values_from = Customer_Count, values_fill = 0) %>%
    mutate(Updated = round(`2024`/colSums(.[, c(4)], na.rm=TRUE)*(colSums(.[, c(2)], na.rm=TRUE)*value)))  |> 
    mutate(across(everything(), ~format(.x, big.mark = ","))) %>%
    rename_with(~gsub("Year_", "", .), starts_with("Year_"))
}

cx_chart_up <- function(data,value) {
  data %>%
    mutate(Date = lubridate::month(Date)) %>%
    group_by(Date) %>%
    tidyr::pivot_wider(names_from = Year, values_from = Customer_Count, values_fill = 0) %>%
    mutate(Updated = round(`2024`/colSums(.[, c(4)], na.rm=TRUE)*(colSums(.[, c(2)], na.rm=TRUE)*value)),Day = 1)  %>%
    tidyr::pivot_longer(cols = c(-Date,-Day), names_to = "Group", values_to = "Customer_Count") %>%
    mutate(Year = case_when(
      Group == "Updated" ~ 2024,
      TRUE ~ as.numeric(Group))
    ) %>%
    mutate(Date = as.Date(paste0(Year, "-", Date, "-", Day))) |> 
    select(Date, Group, Customer_Count) |> 
    mutate(Month = lubridate::month(Date, label = TRUE))
}



# H20 ----

amazon_phone <- function(path = "", sheet = "", col_types = "", ...) {
  if (!require("dplyr")) {
    stop("dplyr package not found.")
  }
  
  path <- dplyr::case_when(
    path == "" ~ "G:\\Shared drives\\WFM\\LS_Daily_Metrics_Build.xlsm",
    TRUE ~ path
  )
  
  sheet <- dplyr::case_when(
    sheet == "" ~ "Raw_Data",
    TRUE ~ sheet
  )
  
  col_types <- dplyr::case_when(
    col_types == "" ~ c("text", "date", "numeric"),
    TRUE ~ col_types
  )
  
  readxl::read_excel(path = path, sheet = sheet, col_types = col_types, ...)
}

cx_up_daily <- function(value){
  env[["cx_chart_up"]](cx,value = value) %>% 
    filter(Group != 2024) %>%   # Adding the multiplier to the forecasted CX column 
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    group_by(Group) %>%
    expand(Date = seq(min(Date), max(Date)+days(30), by = "day")) %>% 
    mutate(Month = format(Date, "%b"), DIM = days_in_month(Date)) %>%
    left_join(env[["cx_chart_up"]](cx,value = 1.3), by = c("Group"="Group", "Month"="Month")) %>%
    mutate(Customer_Count = round(Customer_Count/DIM)) %>% 
    select(1,Date = 2,3,6)
}

cleaned_tbl <- function(holiday) {
  env[["amazon_phone"]]() %>%
    dplyr::mutate(NCO_trans = log1p(`Offered Calls`)) %>%
    dplyr::left_join(holiday, by = c("Date" = "date")) %>% 
    dplyr::mutate(NCO_trans_clean = timetk::ts_clean_vec(NCO_trans, period = 7)) %>% 
    dplyr::mutate(NCO_trans_clean_drop = case_when(
      (Date >= lubridate::floor_date(Date, "week")) ~ NCO_trans,
      (!is.na(holiday_name)) ~ NCO_trans,
      TRUE ~ NCO_trans_clean)) %>% 
    dplyr::select(-holiday_name)
}



prepared_tbl <- function(data,holiday,customer) {
  
  data  <- data
  
  horizon    <- as.numeric(as.Date('2024-12-31')-as.Date(max(data$Date)))  
  lag_period <- as.numeric(as.Date('2024-12-31')-as.Date(max(data$Date)))
  max <- max(data$Date)
  min <- min(data$Date)
  
  data %>%
    
    tidyr::pivot_wider(names_from = name,values_from = value) %>% 
    
    # Add future window
    bind_rows(
      timetk::future_frame(.data = ., .date_var = Date, .length_out = horizon)
    ) %>% arrange(Date) %>% 
    
    # Add Events
    left_join(holiday, by = c("Date" = "date")) %>% 
    dplyr::mutate(holiday_name = ifelse(is.na(holiday_name), 0, holiday_name)) %>%
    
    # Add Untis/adds
    left_join(customer %>% filter(Date >= as.Date(min), Date <= as.Date(max)), by = c("Date" = "Date")) %>%
    
    # Format Columns
    rename(holiday_name_event = holiday_name) %>% 
    dplyr::mutate(Date = as.Date(Date))
}



forecast_Chart <-function(data,u) {
  data %>% 
  ggplot(aes(x = Date, y = Forecast, group = .key, color = .key)) +
  geom_line() +
  geom_text_repel(aes(label = holiday_name_event), hjust = 0, vjust = 1, nudge_x = 0.1, size = 3) +
  geom_point(data = data[!is.na(data$holiday_name_event), ], aes(shape = holiday_name_event), size = 4) +
  scale_shape_manual(values = 1:length(u)) +
  labs(title = "Forecast by Group",
       x = "Date",
       y = "Forecast",
       color = "Group") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, NA))
}





