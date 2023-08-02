remove(list = ls())

#libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(tidyr)

setwd('/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo')
getwd()

#read in data
videogamesales_db <- read_excel("video_game_sales_cleaned.xlsx")


#################################
#######   GENRE TAB   #########
#################################

#sales per genre across years
summary_genre_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  summarise(sum_global_sales = sum(global_sales),
            sum_asia_sales = sum(asia_sales),
            sum_namerica_sales = sum(north_american_sales),
            sum_europe_sales = sum(european_sales),
            sum_japan_sales = sum(japan_sales))

ggplot(summary_genre_db)  + 
  geom_line(aes(x=release_year, y=sum_global_sales, colour=genre)) +
  labs(title= "Video Game Units Sold per Genre Globally",
       x="Release Year",y="Number of Copies Sold (in millions)")

## of titles per genre
summary_genre_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  count(release_year, genre)

ggplot(summary_genre_db)  + 
  geom_bar(aes(x = release_year, y = n, fill = genre), stat="identity", position="dodge") +
  labs(title= "Total Genre Titles Released per Year", x="Year",y="Number of Titles")


##genre per platform
genre_platform_db <- videogamesales_db %>%
  group_by(platform, genre, release_year) %>%
  summarise(sum_global_sales = sum(global_sales),
            sum_asia_sales = sum(asia_sales),
            sum_namerica_sales = sum(north_american_sales),
            sum_europe_sales = sum(european_sales),
            sum_japan_sales = sum(japan_sales))

DS_genre <- genre_platform_db %>%
  filter(platform == "DS")

ggplot(DS_genre)  + 
  geom_line(aes(x=release_year, y=sum_global_sales, colour=genre)) +
  labs(title= "Video Game Sales 2000-2020 Globally",
       x="Platform",y="Number of Games Sold Globally (in millions)")

##PRODUCTION COST METRIC
#production cost per genre across years
summary_productioncost_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  summarise(sum_productioncost = sum(`Production Cost`))

ggplot(summary_productioncost_db)  + 
  geom_line(aes(x=release_year, y=sum_productioncost, colour= genre)) +
  labs(title= "Production Cost per Genre 2000-2020",
       x="Year",y="Millions of Dollars")


