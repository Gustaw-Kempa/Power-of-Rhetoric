rm(list = ls())
library(jsonlite) #to read poll data
library(tidyverse) #general data wrangling
library(lubridate) #making the dates easier
library(BBmisc) # data scaling and normalizing
library(reshape2) # melting data for plots


# adding deaths data
# adding death data to the model
deaths_data <- read_csv('/Users/gustawkempa/Desktop/Studia/Master/data/deaths/global_confirmed_cases.csv')


# UK ####


UK_speeches  <-
 read_delim("/Users/gustawkempa/Desktop/Studia/Master/data/speeches/UK.csv")
#fixing the dates in dataset
UK_speeches$dates <- dmy(UK_speeches$dates)
#count the number of words in each speech - would be better to count tokens instead - will be implemented :)

df <- tibble()
fragm <- c()
for (i in 1:nrow(UK_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(UK_speeches$speeches[i], "(?<=[[:punct:]])\\s(?=[A-Z])", perl = T))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  } 
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = UK_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
UK_speeches <- df


# binding the results
UK_res <-
  tibble(
    sentiment = read.csv(
      "/Users/gustawkempa/Desktop/Studia/Master/data/emotions/UK/sent_temp.csv"
    )[, 2],
    happiness = read.csv(
      '/Users/gustawkempa/Desktop/Studia/Master/data/emotions/UK/hap_temp.csv'
    )[, 2],
    persuasion = read.csv(
      '/Users/gustawkempa/Desktop/Studia/Master/data/emotions/UK/pers_temp.csv'
    )[, 2],
    anger = read.csv(
      '/Users/gustawkempa/Desktop/Studia/Master/data/emotions/UK/ang_temp.csv'
    )[, 2],
    fear = read.csv(
      '/Users/gustawkempa/Desktop/Studia/Master/data/emotions/UK/fear_temp.csv'
    )[, 2],
    surprise = read.csv(
      '/Users/gustawkempa/Desktop/Studia/Master/data/emotions/UK/surp_temp.csv'
    )[, 2],
    informativeness = read.csv(
      '/Users/gustawkempa/Desktop/Studia/Master/data/emotions/UK/inf_temp.csv'
    )[, 2]
   
  )

UK_final <- tibble(UK_speeches, UK_res) %>% group_by(date) %>% summarise(
  mean_sentiment = mean(sentiment),
  mean_informativeness = mean(informativeness),
  mean_persuasion = mean(persuasion), mean_anger = mean(anger), mean_surprise = mean(surprise), 
  mean_happiness = mean(happiness), mean_fear = mean(fear),
  sd_sentiment = sd(sentiment),
  sd_informativeness = sd(informativeness),  sd_fear = sd(fear),
  sd_persuasion = sd(persuasion), sd_anger = sd(anger), sd_surprise = sd(surprise), 
  sd_happiness = sd(happiness))

# adding the week number (format year, week no)
UK_final$week <- strftime(UK_final$date, format = "%y%V")

# reading the polls data

UK_polls <- read_csv('/Users/gustawkempa/Desktop/Studia/Master/data/polls/UK_polls.csv')
UK_polls <- UK_polls %>% group_by(week) %>% summarise(support = mean(approve_johnson))
UK_polls <- UK_polls %>% mutate(diff1 = lead(support) - support, 
                               diff2 = lead(support, n=2) - lead(support, n=1))
UK_final$week <- as.numeric(UK_final$week)
# joining the two dataframes


UK_final <- UK_final %>% left_join (UK_polls, by= 'week') %>% na.omit()
uk_deaths <- deaths_data$`United Kingdom...266` + deaths_data$`United Kingdom...267` + deaths_data$`United Kingdom...268` + 
  deaths_data$`United Kingdom...269` + deaths_data$`United Kingdom...270` + deaths_data$`United Kingdom...271` + 
  deaths_data$`United Kingdom...272` + deaths_data$`United Kingdom...273` + deaths_data$`United Kingdom...274` + 
  deaths_data$`United Kingdom...275` + deaths_data$`United Kingdom...276` + deaths_data$`United Kingdom...277` + 
  deaths_data$`United Kingdom...278` + deaths_data$`United Kingdom...279` + deaths_data$`United Kingdom...280`


UK_final <- UK_final %>% left_join(tibble(week = deaths_data$week, cases = uk_deaths), by='week')

UK_final$country <- "UK"



rm(list = setdiff(ls(),c( "UK_final","deaths_data")))


# USA ####

USA_speeches  <-
  read_delim("/Users/gustawkempa/Desktop/Studia/Master/data/speeches/USA.tsv")
#fixing the dates in dataset
USA_speeches$dates <- mdy(USA_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(USA_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(USA_speeches$speeches[i], "(?<=[[:punct:]])\\s(?=[A-Z])", perl = T))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  } 
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = USA_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
USA_speeches <- df


# binding the results
USA_res <- read_csv("/Users/gustawkempa/Desktop/Studia/Master/data/emotions/USA/USA_emotions.csv")
 
USA_final <- tibble(USA_speeches, USA_res) %>% group_by(date) %>% summarise(
  mean_sentiment = mean(sentiment),
  mean_informativeness = mean(informativeness),
  mean_persuasion = mean(persuasion), mean_anger = mean(anger), mean_surprise = mean(surprise), 
  mean_happiness = mean(happiness),  mean_fear = mean(fear),
  sd_sentiment = sd(sentiment),
  sd_informativeness = sd(informativeness), sd_fear = sd(fear),
  sd_persuasion = sd(persuasion), sd_anger = sd(anger), sd_surprise = sd(surprise), 
  sd_happiness = sd(happiness))

# adding the week number (format year, week no)
USA_final$week <- strftime(USA_final$date, format = "%y%V")

# reading the polls data

USA_polls <- read_csv('/Users/gustawkempa/Desktop/Studia/Master/data/polls/USA_polls.csv')
USA_polls <- USA_polls %>% group_by(week) %>% summarise(support = mean(approve))
USA_polls <- USA_polls %>%  mutate(diff1 = lead(support) - support, 
                                   diff2 = lead(support, n=2) - lead(support, n=1))
USA_final$week <- as.numeric(USA_final$week)
# joining the two dataframes

USA_final <- USA_final %>% left_join (USA_polls, by= 'week') %>% na.omit()

USA_final <- USA_final %>% left_join(tibble(week = deaths_data$week, cases = deaths_data$US), by='week')

USA_final$country <- "USA"

rm(list = setdiff(ls(),c( "USA_final", "UK_final","deaths_data")))

# ES ####


ES_speeches  <-
  read_delim("/Users/gustawkempa/Desktop/Studia/Master/data/speeches/speeches_ES_translated")
#fixing the dates in dataset
colnames(ES_speeches) = c('id','links', 'topics', 'date','speeches_ES', 'len', 'topics_EN', 'speeches_EN')
ES_speeches$date <- ymd(ES_speeches$date)



df <- tibble()
fragm <- c()
for (i in 1:nrow(ES_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(ES_speeches$speeches_EN[i], "(?<=[[:punct:]])\\s(?=[A-Z])", perl = T))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  } 
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = ES_speeches$date[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
ES_speeches <- df


# binding the results
ES_res <- read_csv("/Users/gustawkempa/Desktop/Studia/Master/data/emotions/ES/ES_emotions.csv")

ES_final <-tibble(ES_speeches, ES_res) %>% na.omit()


ES_final <- ES_final %>% 
  group_by(date) %>% 
  summarise(
    mean_sentiment = mean(sentiment), 
    mean_informativeness = mean(informativeness),
    mean_persuasion = mean(persuasion), 
    mean_anger = mean(anger), 
    mean_surprise = mean(surprise), 
    mean_happiness = mean(happiness),  
    mean_fear = mean(fear),
    sd_sentiment = sd(sentiment),
    sd_informativeness = sd(informativeness), 
    sd_fear = sd(fear),
    sd_persuasion = sd(persuasion), 
    sd_anger = sd(anger), 
    sd_surprise = sd(surprise), 
    sd_happiness = sd(happiness),
    .groups = 'keep'
  )







# adding the week number (format year, week no)
ES_final$week <- strftime(ES_final$date, format = "%y%V")

# reading the polls data

ES_polls <- read_csv('/Users/gustawkempa/Desktop/Studia/Master/data/polls/ES_polls.csv')
ES_polls <- ES_polls %>% group_by(week) %>% summarise(support = mean(PSOE))
ES_polls <- ES_polls %>%  mutate(diff1 = lead(support) - support, 
                                 diff2 = lead(support, n=2) - lead(support, n=1))
ES_final$week <- as.numeric(ES_final$week)
# joining the two dataframes

ES_final <- ES_final %>% left_join (ES_polls, by= 'week') %>% na.omit()

ES_final <- ES_final %>% left_join(tibble(week = deaths_data$week, cases = deaths_data$Spain), by='week')

ES_final$country <- "ES"

rm(list = setdiff(ls(),c( "USA_final", "UK_final", "ES_final","deaths_data")))


# FR ####

FR_speeches  <-
  data <- read_delim("/Users/gustawkempa/Desktop/Studia/Master/data/speeches/france.csv")
#fixing the dates in dataset
FR_speeches$dates <- ymd(FR_speeches$dates)

df <- tibble()
fragm <- c()
for (i in 1:nrow(FR_speeches)) {
  #we need to check how many groups to create
  
  speech_fragment <-
    unlist(strsplit(FR_speeches$speeches_EN[i], "(?<=[[:punct:]])\\s(?=[A-Z])", perl = T))
  fragm <- c()
  for (j in 1:floor(length(speech_fragment) / 5)) {
    fragm[j] <-
      paste(
        speech_fragment[j * 5 - 4],
        speech_fragment[j * 5 - 3],
        speech_fragment[j * 5 - 2],
        speech_fragment[j * 5 - 1],
        speech_fragment[j * 5]
      )
    fragm[j] <- gsub('NA', '', fragm[j])
    fragm[j] <- gsub('\n', '', fragm[j])
  } 
  df_temp <-
    data.frame(
      fragment = fragm,
      fragment_no = c(1:length(fragm)),
      date = FR_speeches$dates[i]
    )
  df <- rbind.data.frame(df, df_temp)
}
FR_speeches <- df


# binding the results
FR_res <- read_csv("/Users/gustawkempa/Desktop/Studia/Master/data/emotions/FR/FR_emotions.csv")

FR_final <- tibble(FR_speeches, FR_res) %>% group_by(date) %>% summarise(
  mean_sentiment = mean(sentiment),
  mean_informativeness = mean(informativeness),
  mean_persuasion = mean(persuasion), mean_anger = mean(anger), mean_surprise = mean(surprise), 
  mean_happiness = mean(happiness), mean_fear = mean(fear),
  sd_sentiment = sd(sentiment),
  sd_informativeness = sd(informativeness), sd_fear = sd(fear),
  sd_persuasion = sd(persuasion), sd_anger = sd(anger), sd_surprise = sd(surprise), 
  sd_happiness = sd(happiness))

# adding the week number (format year, week no)
FR_final$week <- strftime(FR_final$date, format = "%y%V")

# reading the polls data

FR_polls <- read_csv('/Users/gustawkempa/Desktop/Studia/Master/data/polls/FR_polls.csv')
FR_polls <- FR_polls %>% group_by(week) %>% summarise(support = mean(macron_approve))
FR_polls <- FR_polls %>%  mutate(diff1 = lead(support) - support, 
                                 diff2 = lead(support, n=2) - lead(support, n=1))
FR_final$week <- as.numeric(FR_final$week)
# joining the two dataframes

FR_final <- FR_final %>% left_join (FR_polls, by= 'week') %>% na.omit()

# data needs to be aggregated
fr_deaths <- deaths_data$France...122+ deaths_data$France...123+ deaths_data$France...124+deaths_data$France...125+
                 deaths_data$France...126+deaths_data$France...127+deaths_data$France...128+ deaths_data$France...129+
                 deaths_data$France...130+ deaths_data$France...131+deaths_data$France...132+ deaths_data$France...133
                 

FR_final <- FR_final %>% left_join(tibble(week = deaths_data$week, cases = fr_deaths), by='week')

FR_final$country <- "FR"

rm(list = setdiff(ls(),c( "USA_final", "UK_final", "ES_final", "FR_final","deaths_data")))


model_data <- rbind(USA_final, UK_final, ES_final, FR_final)





write_csv(model_data, "/Users/gustawkempa/Desktop/Studia/Master/data/model_data.csv")



# TODO: implement sd instead of mean and see if there are any changes 
# TODO: track changes between polls/ weeks more than changes between speeches
# TODO: Include t-1 and maybe t-2

