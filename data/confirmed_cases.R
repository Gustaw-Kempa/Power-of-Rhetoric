rm(list = ls())
library(tidyverse)
library(lubridate)


global_data <- read_delim('/Users/gustawkempa/Desktop/Studia/Master/data/deaths/global_deaths_john_hopkins.csv'
                 )[-1,]


colnames(global_data) <- c('date', colnames(global_data)[-1])
csddcxz <- tibble(global_data$`Country/Region`, global_data$Poland)
global_data$date <- mdy(global_data$date)

global_data$week <- strftime(global_data$date, format = "%y%V")


global_data[, -1] <- lapply(global_data[-1], as.numeric)
global_data <- global_data[,-1]


weekly_global_data <- global_data %>% group_by(week) %>% summarise_all(sum, na.omi = TRUE)



write_csv(weekly_global_data, '/Users/gustawkempa/Desktop/Studia/Master/data/deaths/global_confirmed_cases.csv')
