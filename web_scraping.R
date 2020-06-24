###load packages

library(tidyverse)
library(janitor)
library(tidytext)
library(rvest)
library(rtweet)


### Analysis of unstructured text into R

tweet_text <- "Informed Dr. Fauci this morning that he has nothing to do with NFL Football. 
Forced Democrat run Minnesota to bring in the National Guard & end rioting & looting after seeing the destruction & crime in Minneapolis. 
100% successful! Waiting to hear from Dem run Washington State..."

tweet_url <- "https://twitter.com/realDonaldTrump/status/1274170611280068615"

tibble(tweet_text) %>% unnest_tokens(out,tweet_text,token="words") %>%
  summarise(num_words=n())  #44

#base R:
length(unlist(strsplit(tweet_text,split=" "))) #47 
#different answers tho.. interesting


##### Web Scraping #####

buzzfeed <- "https://www.buzzfeednews.com/section/politics"
buzznews <- read_html(buzzfeed)

#buzznews
#str(buzznews)

body_nodes <- buzznews %>% html_node("body") %>% html_children()
body_nodes %>% html_children()

articles <- buzznews %>% rvest::html_nodes("div.sm-mt2") %>% html_text() %>% head()
  
#preprocess - remove '\n'
article_dat <- gsub("\n","",articles)

article_dat <- gsub("{ }","",article_dat)

article_dat <- gsub("  ","",article_dat)
article_dat <- gsub("\"","",article_dat)
article_dat <- gsub("}'","",article_dat)
article_dat <- str_remove(article_dat,"i18n:.*$")
article_dat <- str_remove(article_dat,"feedName: section/politics,page: 1")
article_dat <- gsub("\\{","",article_dat)
article_dat <- gsub("\\}","",article_dat)

article_get_dat <- article_dat %>% str_split("•")

buzz_news <- data.frame(article_get_dat)

buzz_news <- apply(buzz_news,2,function(x) str_remove(x,"\\d\\s(hour.|day.) ago\\d (hour.|day.) ago\\s"))
#worked well for the most part but not for a few rows

#remove last row
buzz_news <- buzz_news[c(1:nrow(buzz_news)-1),]

#remove dates for remaining rows

buzz_news[2] <- buzz_news[2] %>% str_remove("\\s1 hour ago1 hour ago\\s")

buzz_news[3] <- buzz_news[3] %>% str_remove("\\s14 hours ago14 hours ago\\s")

buzz_news[4] <- buzz_news[4] %>% str_remove("\\s17 hours ago17 hours ago\\s")

buzz_news[5] <- buzz_news[5] %>% str_remove("\\s22 hours ago22 hours ago\\s")

buzz_news[6] <- buzz_news[6] %>% str_remove("\\s1 day ago1 day ago\\s")

buzz_news[29]<- buzz_news[29] %>% str_remove("\\s10 days ago10 days ago\\s")

buzz_news[30]<- buzz_news[30] %>% str_remove("\\s11 days ago10 days ago\\s")

buzz_news <- as.data.frame(buzz_news)  ##cleaned data frame with just headlines

buzz_news2 <- sapply(buzz_news,function(x) gsub("    ","",buzz_news[x,])) #remove whitespace?


###TO DO:













#remove_dates <- str_remove(article_get_dat,"\\d\\s(hour.|day.) ago\\d (hour.|day.) ago\\s")

#article_get_dat <- sapply(remove_dates,function(x) x[1])


#article_get_dat <- article_dat %>%
  #str_split("\\d\\s(hour.|day.) ago\\d (hour.|day.) ago\\s") 


#article_get_dat[[1]][1] %>% str_split("•")




#all_headlines <- sapply(article_get_dat, function(x) x[1])


#article_dat <- gsub("{18n: |","",article_dat)
#r <- regexpr("i18n:",article_dat)
#regmatches(article_dat,r)
#regexec("i18n:",article_dat)

#regexec("bfnews",article_dat)

#delete_this <- substr(article_dat,8217,8218+2307)


#article_get_dat <- article_dat %>%
  #str_split("\\d (hour.|day.) ago") 

#%>% str_split("\\d day ago")
#article_dat <- article_dat %>% str_split("\\")

#article_dat <- article_dat %>% str_split("\\d day. ago")


#headline_all <- sapply(article_dat, function(x) x[1]) # extract only the first elements


#all_headlines[1:10] # take a look at the first ten


#article_dat <- slice(-[8218:10525])
