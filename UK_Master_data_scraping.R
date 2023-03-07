library(rvest)
library(dplyr)


#1st page
# html <- "https://presidenti.quirinale.it/Presidente/12/ricerca/discorsi?p=1&fp=True"
# 2nd page
html <- "https://presidenti.quirinale.it/Presidente/12/ricerca/discorsi?p=2&fp=True"
#3rd page
#html <- "https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-oldest&page=3&people%5B%5D=boris-johnson"
# 4th page
# html <- "https://www.gov.uk/search/news-and-communications?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-oldest&page=4&people%5B%5D=boris-johnson"
page <- read_html(html)



topics <- page %>% html_nodes('#archive a') %>% html_text()
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
