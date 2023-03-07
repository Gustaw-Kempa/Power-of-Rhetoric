library(rvest)
library(dplyr)


links <- c('https://pm.gc.ca/en/news/speeches/2020/03/11/prime-ministers-remarks-canadas-response-covid-19',
           )


# webpage to be updated 
html_beggining <- "https://pm.gc.ca/en/news/speeches?page="
# timeframe of interest
html_nums <- c(2:24)


for (i in html_nums) {
  html <- paste(html_beggining, i, sep = "")
  page <- read_html(html)
  links <- page %>% html_nodes('.title a') %>% html_attr("href") %>%
    paste("https://presidenti.quirinale.it/", ., sep="")
  topics <- page %>% html_nodes('.title') %>% html_text()
  dates <-  page %>% html_nodes('time') %>% html_text()
}

remotes::install_github("irudnyts/openai", ref = "create_chat_completion")



topics <- page %>% html_nodes('.inline-date') %>% html_text()
topics <- topics[c(FALSE,TRUE)]
dates <-  page %>% html_nodes('.card-date') %>% html_text()
links <- page %>% html_nodes('#archive a') %>% html_attr("href") %>%
  paste("https://presidenti.quirinale.it/", ., sep="")

get_text = function(link) {
  
  subpage <- read_html(link)
  speech <- subpage %>% html_nodes(".content-main-wrap p") %>% html_text() %>% paste(collapse =" " )
}

speeches = sapply(links, get_text)

it2 <- data_frame(topics, dates, speeches, links)
