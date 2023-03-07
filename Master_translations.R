library(deeplr)
library(lubridate)
library(stringr)
library(googleLanguageR)
gl_auth("/Users/gustawkempa/Desktop/Studia/Master/Master.Data/master-377713-d6f77365d167.json")
key_deepl <- "5a7e90e2-ddfb-8858-a3da-1610ab3190ee:fx"
key_google <- "AIzaSyDy660FyuscFxBITTui1xvqeV18XfQxLk8"
data_FR <- read.csv("/Users/gustawkempa/Desktop/Studia/Master/data/speeches/france.csv")
# fixing the dates
data_FR$dates <- dmy(data_FR$dates)
# getting rid of the newline signs (and things coming after them)
data_FR$speeches <- str_extract(data_FR$speeches, "^[^\n]+")
data_FR$topics <- gsub("\n", "", data_FR$topics)
sum(sapply(data_FR$speeches, nchar))



# for (i in 1:nrow(data_FR))
#   data_FR$topics_EN <- toEnglish2(
#     text = data_FR$topics[i], auth_key = key_deepl)

topics_EN <- gl_translate(t_string = data_FR$topics, target = "en",
                                  format = "text", model = "nmt")[1]
speeches_EN <- gl_translate(t_string = data_FR$speeches, target = "en",
                                   format = "text", model = "nmt")[1]


