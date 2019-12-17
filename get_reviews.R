library(rvest)
library(stringr)
library(tidyverse)
library(ggplot2)
library(dplyr)


## Собираем отзывы к каждому фильму и сериалу

get_reviews <- function(html_link){
  
  webpage <- read_html(html_link)
  main <- webpage %>% html_nodes('.review-container')
  
  vals <- list(rate=c(), spoiler=c(), date=c(), user=c(), title=c(),
               link=c(), text=c(), help_plus=c(), help_all=c())
  
  for (i in main){
    
    rate <- i %>% html_nodes("span") %>% html_text(trim = TRUE)
    if (grepl("/", rate[1])){rate <- as.numeric(rate[2])} 
    else{rate <- NA}
    vals$rate <- c(vals$rate, rate)
    
    spoiler <- i %>% html_nodes(".spoiler-warning")
    if (length(spoiler) > 0){spoiler <- 1}
    else {spoiler <- 0}
    vals$spoiler <- c(vals$spoiler, spoiler)
    
    date <- i %>% html_nodes(".review-date") %>% html_text(trim = TRUE)
    vals$date <- c(vals$date, date)
    
    user <- i %>% html_nodes(".display-name-link") %>% html_nodes("a") %>% html_attr("href")
    vals$user <- c(vals$user, str_match(user, "user/(.*?)/")[,2])
    
    title <- i %>% html_nodes(".title")
    link <- str_match(title %>% html_attr("href"), "review/(.*?)/")[,2]
    vals$link <- c(vals$link, link)
    
    title <- title %>% html_text(trim = TRUE)
    vals$title <- c(vals$title, title)
    
    text <- i %>% html_nodes(".text.show-more__control") %>% html_text(trim = TRUE)
    vals$text <- c(vals$text, text)
    
    help <- i %>% html_nodes(".actions.text-muted") %>% html_text(trim = TRUE)
    help <- str_match(help, "([0-9]+(?: [0-9]+)?) out of ([0-9]+(?: [0-9]+)?)")
    
    vals$help_plus <- c(vals$help_plus, as.numeric(help[,2]))
    vals$help_all <- c(vals$help_all, as.numeric(help[,3]))
  }
  return(vals)
}

##---------------------------------------------------------------------------------

review_link <- "https://www.imdb.com/title/%s/reviews?ref_=tt_ov_rt"
data_path <- "tops2"

filenames <- list.files(data_path, pattern="*.csv", full.names=TRUE)

all_review_data <- list()
movid_file <- list()


## Собираем в одном файле id фильма и чарт, к которому он относится

for (filename in filenames){
  
  mydata <- read.csv(filename, header = TRUE)
  name <- str_match(filename, 'tops2/(.*?)csv')[,2]
  rn_rows <- sample(1:nrow(mydata), 100)
  
  for (id in rn_rows){
    movid <- toString(mydata[id,]$ids)
    movid_file[[movid]] <- toString(c(movid_file[[movid]], name))
  }
}


list_values <- sapply(1:length(movid_file), function(i) movid_file[[i]][[1]])
df <- tibble(movieid = attributes(movid_file)$names, top_name=list_values)
write_csv(df, "id_list6.csv")


##---------------------------------------------------------------------------------
## Скачиваем отзывы

start <- 1

for (ind in start:length(movid_file)){
  
  movieid <- attributes(movid_file)$names[ind]
  
  print(str_c("Started: ", toString(ind), " | ", movieid))

  url <- sprintf(review_link, movieid)
  vals <- get_reviews(url)
  n_rows <- length(vals[[1]])
    
  all_review_data$movie_id <- c(all_review_data$movie_id, rep(movieid, n_rows))
  
  for (attr in attributes(vals)[[1]]){
    all_review_data[[attr]] <- c(all_review_data[[attr]], vals[[attr]])}
  
  if (ind %% 20 == 0){Sys.sleep(70)}
  else{Sys.sleep(4)}
}

start <- ind


df4 <- as_tibble(all_review_data)
write_delim(df4, "all_review_data100_3.csv", delim="\t")

