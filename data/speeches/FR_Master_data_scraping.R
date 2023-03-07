library(rvest)
library(dplyr)


#page address
html <- "https://www.gouvernement.fr/discours-et-rapports?page=5&thematic=%2Fapi%2Fcontent_thematics%2F87&content_type%5B%5D=speech"
  #"https://www.gouvernement.fr/discours-et-rapports?page=4&thematic=%2Fapi%2Fcontent_thematics%2F87&content_type%5B%5D=speech"
  #"https://www.gouvernement.fr/discours-et-rapports?page=3&thematic=%2Fapi%2Fcontent_thematics%2F87&content_type%5B%5D=speech"
  #"https://www.gouvernement.fr/discours-et-rapports?page=2&thematic=%2Fapi%2Fcontent_thematics%2F87&content_type%5B%5D=speech"
page <- read_html(html)

topics <- page %>% html_nodes('.fr-card__link') %>% html_text()
dates <-  page %>% html_nodes('.views-field-field-docs-start-date-time-value.text-nowrap') %>% html_text()
links <- page %>% html_nodes('.fr-card__link') %>% html_attr("href") %>%
  paste("https://www.gouvernement.fr", ., sep="")

get_text = function(link) {
  
  subpage <- read_html(link)
  speech <- subpage %>% html_nodes(".fr-my-1w .fr-col") %>% html_text() %>% paste(collapse =" " )

}
get_date = function(link) {
  
  subpage <- read_html(link)
  date <- subpage %>% html_nodes(".fr-text--sm.fr-mb-5w") %>% html_text()
  return(date)
}

speeches = sapply(links, get_text)
dates = sapply(links, get_date)

fr4 <- data_frame(topics, dates, speeches, links)

france <- rbind(fr1,fr2,fr3,fr4)

dates[[1]]

conferences <- speeches
france <- rbind(address, conferences, readouts)
write.csv(france, 'france.csv')
