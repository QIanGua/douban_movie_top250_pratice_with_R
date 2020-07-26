#!/usr/bin/env Rscript
# --------------
# Author: Qiangua
# Date: 2020-02-21 Fri 11:53
# --------------
## rm(list = ls())
library(tidyverse)
library(magrittr)
library(data.table)

base_url <- "https://movie.douban.com/top250?start=page&filter="

test <- "https://movie.douban.com/top250?start=0&filter="

web <- readLines(test, encoding = "UTF8")

get_title <- function(web) {
  # web <- readLines(url, encoding="UTF8")
  title_vec <- grep('class="title">\\w+', web, value = T) %>% gsub(".*>(.*?)<.*", "\\1", ., perl = T)
  return(title_vec)
}

get_title(web)

get_score <- function(web) {
  # web <- readLines(url, encoding = "UTF8")
  score_vec <- grep('property="v:average"', web, value = T) %>% gsub(".*>(.*?)<.*", "\\1", ., perl = T)
  return(score_vec)
}
# get_score(test)

get_commentNum <- function(web) {
  # web <- readLines(url, encoding = "UTF8")
  commentNum_vec <- grep("评价</span>", web, value = T) %>% gsub(".*>(.*?)人评价<.*", "\\1", ., perl = T)
  return(commentNum_vec)
}
# get_commentNum(test)


get_link <- function(web) {
  # web <- readLines(url, encoding = "UTF8")
  link_vec <- grep("<a href=\".*subject.*class.*>", web, value = T) %>% gsub('.*\\"(.*?)\\" .*', "\\1", ., perl = T)
  return(link_vec)
}
# get_link(test)
get_quote <- function(web) {
  # web <- readLines(url, encoding = "UTF8")
  quote_vec <- grep('<span class="inq">', web, value = T) %>% gsub(".*>(.*?)<.*", "\\1", ., perl = T)
  return(quote_vec)
}
# get_quote(test)

page_num <- seq(0, 225, 25)

title_vec <- c()
rank_vec <- seq(1, 250, 1)
score_vec <- c()
comNum_vec <- c()
link_vec <- c()
quote_vec <- c()

for (i in page_num) {
  url <- gsub("page", i, base_url)
  web <- readLines(url, encoding = "UTF8")
  title_vec <- append(title_vec, get_title(web))
  score_vec <- append(score_vec, get_score(web))
  comNum_vec <- append(comNum_vec, get_commentNum(web))
  link_vec <- append(link_vec, get_link(web))
  quote_vec <- append(quote_vec, get_quote(web))
  Sys.sleep(1)
}

quote_vec[250] <- "NA"
# save as csv file
MovieData <- data.frame(
  MovieName = title_vec,
  Rank = seq(1, 250),
  Score = score_vec,
  CommentNumber = comNum_vec,
  Link = link_vec,
  Quote = quote_vec
)

write.csv(MovieData, file = "douban_mov_top250.csv", row.names = FALSE)

# View(MovieData)
# visualization
# library(ggplot2)
# ggplot(data = MovieData,aes(x = Rank,y = Score)) +
#     geom_point(aes(size = CommentNumber))+
#     geom_text(aes(label = MovieName), colour = "blue",size = 4,vjust = -0.6)
