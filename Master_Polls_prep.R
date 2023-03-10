rm(list = ls())
library(jsonlite)
library(dplyr)
library(lubridate) 

# for now for/against boris johnson can be changed
UK <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/GB-leadership-approval")$polls
USA <- read.csv("https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv")
DE <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/DE-parliament")$polls
# for france we only have presidential data
FR <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/FR-presidential-approval")$polls
IT <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/IT-parliament")$polls
ES <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/ES-parliament")$polls
NL <- fromJSON("https://www.politico.eu/wp-json/politico/v1/poll-of-polls/NL-parliament")$polls


UK <- tibble(UK[,2][1],UK[c(1,3,5)])
UK$date <- ymd(UK$date)
UK$week <- strftime(UK$date, format = "%y%V")
UK <- UK %>% na.omit()
write_csv(UK, "UK_polls.csv")
head(UK)
USA <- tibble(USA[,c(5,6,7,12)])
USA$enddate <- mdy(USA$enddate)
USA$week <- strftime(USA$enddate, format = "%y%V")
write_csv(USA, "USA_polls.csv")

DE <-  tibble(DE[,2][1],DE[c(1,3,5)])

ES <- tibble(ES[,2][2],ES[c(1,3,5)])
ES$date <- ymd(ES$date)
ES$week <- strftime(ES$date, format = "%y%V")
ES <- ES %>% na.omit()
write_csv(ES, "ES_polls.csv")


FR <-tibble(FR[,2][1],FR[c(1,3,5)])
FR$date <- ymd(FR$date)
FR$week <- strftime(FR$date, format = "%y%V")
FR <- FR %>% na.omit()
write_csv(FR, "FR_polls.csv")

