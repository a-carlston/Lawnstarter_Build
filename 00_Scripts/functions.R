
#customer count ----
  
customer_count_data <- function(path = "", sheet = "", col_types = "", ...) {
  if (!require("dplyr")) {
    stop("dplyr package not found.")
  }
  
  path <- dplyr::case_when(
    path == "" ~ "G:\\Shared drives\\WFM\\LS Customer Count 7.11.23.xlsx",
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
  
  readxl::read_excel(path = path, sheet = sheet, col_types = col_types, ...)
}
# customer_count_data("", sheet = "", col_types = "")



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
    mutate(Date = month(Date)) %>%
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