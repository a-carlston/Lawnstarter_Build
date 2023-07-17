library(ggplot2)
library(lubridate)
library(stringr)
-------------------------------------------------------
# call fucntion_file 
#^ sys.source(function_file, envir = env <- new.env())

# Call the specific function from the environment
#^ result <- env[[function_name]](arg1, arg2)
-------------------------------------------------------

sys.source("./00_Scripts/functions.R", envir = env <- new.env())

data <- env[["customer_count_data"]]() 

data_up <- env[["year_update"]](data,2023,30)

Pivot_tbl <- data %>%
    mutate(Date = format(Date, "%b")) %>%
    group_by(Date) %>%
    tidyr::pivot_wider(names_from = Year, values_from = Customer_Count, names_prefix = "Year_", values_fill = 0) %>%
    reactable::reactable(defaultPageSize = 50, height = "450px")
  

chart <-   data %>%
  mutate(Month = lubridate::month(Date, label = TRUE)) %>%
  ggplot(aes(x = Month, y = Customer_Count, color = as.factor(Year), group = as.factor(Year))) +
  geom_line() +
  labs(x = "Month", y = "Customer Count", color = "Year") +
  scale_color_discrete(name = "Year") +
  theme_minimal()

plotly::ggplotly(chart)

data_up <- data %>%
  mutate(Date = month(Date)) %>%
  group_by(Date) %>%
  tidyr::pivot_wider(names_from = Year, values_from = Customer_Count, values_fill = 0) %>%
  mutate(Updated = `2022`/colSums(.[, c(2)], na.rm=TRUE)*(colSums(.[, c(2)], na.rm=TRUE)*1.3)) |> 
  tidyr::pivot_longer(cols = c(-Date,-Day), names_to = "Group", values_to = "Customer_Count") %>%
  mutate(Year = case_when(
    Group == "Updated" ~ 2024,
    TRUE ~ as.numeric(Group))
  ) %>%
  mutate(Date = as.Date(paste0(Year, "-", Date, "-", Day))) |> 
  select(Date, Group, Customer_Count)


chart_up <- data_up %>%
  mutate(Month = lubridate::month(Date, label = TRUE)) %>%
  ggplot(aes(x = Month, y = Customer_Count, color = as.factor(Group), group = as.factor(Group), linetype = as.factor(Group))) +
  geom_line() +
  labs(x = "Month", y = "Customer Count", color = "Group") +
  scale_color_manual(values = c("orange", "blue", "green", "red")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted")) +
  theme_minimal()

plotly::ggplotly(chart_up)



