
library(rvest)
library(stringr)
library(tidyverse)


imbd_links <- c()

imbd_links["top_rated_movies_link"] <- 'https://www.imdb.com/chart/top/?ref_=nv_mv_250'
imbd_links["low_rated_movies_link"] <- 'https://www.imdb.com/chart/bottom?sort=rk,desc&mode=simple&page=1'

imbd_links["top_rated_tv_link"] <- 'https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250' 
imbd_links["low_rated_tv_link"] <- 'https://www.imdb.com/list/ls062374418/?sort=list_order,asc&st_dt=&mode=detail&page='

#imbd_links["most_pop_movies_link"] <- 'https://www.imdb.com/chart/moviemeter/?sort=rk,asc&mode=simple&page='
#imbd_links["most_pop_tv_link"] <- 'https://www.imdb.com/chart/tvmeter/?ref_=nv_tvv_mptv'


##---------------------------------------------------------------------------------

# Есть два типа данных рейтигов, из которых собирались списки фильмов: 3 созданных
# imdb и 1 созданный пользователем. 
# Они имеют разную разметку, поэтому для каждого из них, были написаны отдельные функции



# функция достает информацию про каждый сериал из чарта imdb 

get_movie_id <- function(data_length, results_title, results_rating){
  
  vals <- list(movies=c(), years = c(), ids = c(), links = c(), rates = c())
  
  for (index in 1:data_length ){
    
    line1 <- results_title[index]
    line2 <- results_rating[index]
    
    movie_name <- line1 %>% html_nodes("a")
    vals$movies <- c(vals$movies, movie_name %>% html_text(trim = TRUE))
    
    movie_link <- movie_name %>% html_attr("href")
    vals$links <- c(vals$links, movie_link)
    
    id <- str_match(movie_link, "title/(.*?)/")[,2]
    vals$ids <- c(vals$ids, id)
    
    movie_year <- line1 %>% html_nodes("span") %>% html_text(trim = TRUE)
    
    if (length(movie_year) > 1){movie_year <- movie_year[1]}
    
    vals$years <- c(vals$years, substr(movie_year, 2, 5))
    
    vals$rates <- c(vals$rates, line2 %>% html_text(trim = TRUE))}
  
  df <- tibble(vals$movies = movies, vals$years = years,
               vals$ids = ids, vals$links = links, vals$rates = rates)
  return(df)}


##---------------------------------------------------------------------------------
# функция парсит чарт с фильмами и сериалами, сделанный imdb

parse_imdb_lists <- function(webpage){
  
  results_title <- webpage %>% html_nodes(".titleColumn")
  results_rating <- webpage %>% html_nodes(".ratingColumn.imdbRating")
  
  data_length <- length(results_title)
  
  df <- get_movie_id(data_length, results_title, results_rating)
  return(df)}


##---------------------------------------------------------------------------------
# функция достает информацию про каждый сериал из чарта пользователя 

get_movie_id_users_list <- function(webpage, vals){
  
  results_title <- webpage %>% html_nodes(".lister-item-image.ribbonize")
  results_date <- webpage %>% html_nodes(".lister-item-header")
  results_rate <- webpage %>% html_nodes(".ipl-rating-star.small")
  
  data_length <- length(results_title)
  
  for (indx in 1:data_length){
    
    line1 <- results_title[indx]
    line2 <- results_date[indx]
    
    id <- line1 %>% html_attr("data-tconst")
    vals$ids <- c(vals$ids, id)
    
    link <- line1 %>% html_nodes("a") 
    vals$links <- c(vals$links, link %>% html_attr("href"))
    
    name <- link %>% html_nodes("img")  %>%  html_attr("alt")
    vals$names <- c(vals$names, name)
    
    date <- line2 %>% html_nodes("span") %>% html_text(trim = TRUE)
    vals$dates <- c(vals$dates, substr(date[2], 2, 5))

    vals$rates <- c(vals$rates, results_rate[indx] %>% html_text(trim = TRUE))}
  return(vals)}


##---------------------------------------------------------------------------------
# функция парсит чарт с фильмами и сериалами, сделанный пользователем

parse_user_lists <- function(name){
  
  vals <- list(ids=c(), links=c(), names=c(), dates=c(), rates=c())
  
  for (page in 1:2){
    url <- str_c(name, page)
    webpage <- read_html(url)
    vals <- get_movie_id_users_list(webpage, vals)}

  df <- as_tibble(vals)
  #df <- tibble(movies = vals$names, years = vals$dates,
  #             ids = vals$ids, links = vals$links, rates=vals$rates)
  return(df)}



##---------------------------------------------------------------------------------
# Парсим рейтинги

for (name in attributes(imbd_links)[[1]]){
  
  if (name != 'low_rated_tv_link'){
    webpage <- read_html(imbd_links[name])
    df <- parse_imdb_lists(webpage)} 
  
  else{df<- parse_user_lists(imbd_links[name])}
  
  write_csv(df, str_c("r_imdb/", name, '.csv'))
            
  Sys.sleep(5)}





