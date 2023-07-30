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
#######   PLATFORM TAB  #########
#################################

##BY TIME
#total units sold over time
summary_platform_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarise(sum_global_sales = sum(global_sales),
            sum_asia_sales = sum(asia_sales),
            sum_namerica_sales = sum(north_american_sales),
            sum_europe_sales = sum(european_sales),
            sum_japan_sales = sum(japan_sales))

ggplot(summary_platform_db)  + 
  geom_line(aes(x=release_year, y=sum_global_sales, colour=platform)) +
  labs(title= "Total Video Games Units Sold per Platform Globally",
       x="Release Year",y="Number of Copies Sold (in millions)")

#avg units sold over time
summary_platform_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarise(mean_global_sales = mean(global_sales),
            mean_asia_sales = mean(asia_sales),
            mean_namerica_sales = mean(north_american_sales),
            mean_europe_sales = mean(european_sales),
            mean_japan_sales = mean(japan_sales))

ggplot(summary_platform_db)  + 
  geom_line(aes(x=release_year, y=mean_global_sales, colour=platform)) +
  labs(title= "Avg Video Games Units Sold per Platform Globally",
       x="Release Year",y="Number of Copies Sold (in millions)")

#total production cost over time
total_productioncost_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarise(sum_productioncost = sum(`Production Cost`))

ggplot(total_productioncost_db)  + 
  geom_line(aes(x=release_year, y=sum_productioncost, colour=platform)) +
  labs(title= "Total Production Costs per Year",
       x="Year",y="Millions of Dollars")

#average production cost over time
avg_productioncost_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarise(mean_productioncost = mean(`Production Cost`))

ggplot(avg_productioncost_db)  + 
  geom_line(aes(x=release_year, y=mean_productioncost, colour=platform)) +
  labs(title= "Average Production Costs per Year",
       x="Year",y="Millions of Dollars")

##BY REGION
#sales per platform per year
platform_db <- videogamesales_db %>%
  group_by(platform) %>%
  summarise(sum_global_sales = sum(global_sales),
            sum_asia_sales = sum(asia_sales),
            sum_namerica_sales = sum(north_american_sales),
            sum_europe_sales = sum(european_sales),
            sum_japan_sales = sum(japan_sales))

ggplot(platform_db)  + 
  geom_bar(aes(x=platform, y=sum_global_sales, fill = platform), stat='identity') +
  labs(title= "Video Game Sales 2000-2020 Globally",
       x="Platform",y="Number of Games Sold (in millions)")

ggplot(platform_db)  + 
  geom_bar(aes(x=platform, y=sum_namerica_sales, fill = platform), stat='identity') +
  labs(title= "Video Game Sales 2000-2020 in North America",
       x="Platform",y="Number of Games Sold (in millions)")

ggplot(platform_db)  + 
  geom_bar(aes(x=platform, y=sum_europe_sales, fill = platform), stat='identity') +
  labs(title= "Video Game Sales 2000-2020 in Europe",
       x="Platform",y="Number of Games Sold (in millions)")

ggplot(platform_db)  + 
  geom_bar(aes(x=platform, y=sum_asia_sales, fill = platform), stat='identity') +
  labs(title= "Video Game Sales 2000-2020 in Asia",
       x="Platform",y="Number of Games Sold (in millions)")

ggplot(platform_db)  + 
  geom_bar(aes(x=platform, y=sum_japan_sales, fill = platform), stat='identity') +
  labs(title= "Video Game Sales 2000-2020 in Japan",
       x="Platform",y="Number of Games Sold (in millions)")


##TITLES PER YEAR
#count of platform titles per year
summary_genre_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  count(release_year, platform)

ggplot(summary_genre_db)  + 
  geom_bar(aes(x = release_year, y = n, fill = platform), stat="identity", position="dodge") +
  labs(title= "Total Platform Titles Released per Year", x="Year",y="Number of Titles")

##look at top five titles sold per platform
top_platform_db <- videogamesales_db %>%
  group_by(platform) %>%
  arrange(platform, desc(global_sales)) %>%
  slice_head(n=5) 

top_platform_db %>%
  select(title, platform, global_sales) %>%
  filter(platform == "DS") %>%
  kable("html",
        col.names = c("Game Title", 
                      "Platform", 
                      "Global Sales (in millions)")) %>%
  kable_styling(c("striped", "hover"), full_width = T)



