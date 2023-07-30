remove(list = ls())

#libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(tidyr)

setwd('/Users/rachel/Library/CloudStorage/OneDrive-Personal/Documents/Job Stuff/Job Portfolio Projects/Video Game Sales')

#read in data
videogamesales_db <- read_excel("video_game_sales.xlsx")

#check for NAs in each database
table(is.na(videogamesales_db))
sapply(videogamesales_db, function(columns)
  sum(is.na(columns)))

##clean column names
videogamesales_db <- videogamesales_db %>%
  mutate(asia_sales = aisan_sales) %>%
  select(-aisan_sales) %>%
  relocate(asia_sales, .before = north_american_sales)

##explore data and visualize
###lets focus on global sales to start

#################################
#######   SUMMARY TAB   #########
#################################

#UNITS SOLD
#total unit sold plot
summary_sales_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(sum_global_sales = sum(global_sales),
            sum_asia_sales = sum(asia_sales),
            sum_namerica_sales = sum(north_american_sales),
            sum_europe_sales = sum(european_sales),
            sum_japan_sales = sum(japan_sales))

ggplot(summary_sales_db)  + 
  geom_line(aes(x=release_year, y=sum_namerica_sales, color = "North America")) +
  geom_line(aes(x=release_year, y=sum_europe_sales, color = "Europe")) +
  geom_line(aes(x=release_year, y=sum_asia_sales, color = "Asia")) +
  geom_line(aes(x=release_year, y=sum_japan_sales, color = "Japan")) +
  labs(title= "Total Video Game Units Sold Across Regions", x="Year",y="Millions of Copies Sold")

#total units sold table
top_sales_db <- summary_sales_db %>%
  select(-sum_global_sales) %>%
  rename(Asia = sum_asia_sales,
         Europe = sum_europe_sales,
         `North America` = sum_namerica_sales,
         Japan = sum_japan_sales) %>%
  pivot_longer(cols = c(Asia, `North America`, Europe, Japan),
                        names_to = "region", values_to = "sales") %>%
  arrange(desc(sales)) %>%
  relocate(sales, .before = region) %>%
  slice_head(n=5) 

top_sales_db %>%
  kable("html",
        align = 'c',
        col.names = c("Year", 
                      "Total Units Sold", 
                      "Region")) %>%
  kable_styling(c("striped", "hover"), full_width = T, position = "center")



#total production costs plot
summary_productioncost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(sum_productioncost = sum(`Production Cost`))

ggplot(summary_productioncost_db)  + 
  geom_line(aes(x=release_year, y=sum_productioncost)) +
  labs(title= "Total Production Cost per Year", x="Year",y="Millions of Dollars")


##total production cost table 
top_prodcost_db <- summary_productioncost_db %>%
  rename(`Production Cost` = sum_productioncost,
         Year = release_year) %>%
  arrange(desc(`Production Cost`)) %>%
  slice_head(n=5) 

top_prodcost_db %>%
  kable("html",
        align = 'c',
        col.names = c("Year", 
                      "Total Production Cost (in millions)")) %>%
  kable_styling(c("striped", "hover"), full_width = T, position = "center")




##AVERAGES
#average units sold plot
summary_avgsales_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarize(mean_global_sales = mean(global_sales),
            mean_asia_sales = mean(asia_sales),
            mean_namerica_sales = mean(north_american_sales),
            mean_europe_sales = mean(european_sales),
            mean_japan_sales = mean(japan_sales))

ggplot(summary_avgsales_db)  + 
  geom_line(aes(x=release_year, y=mean_asia_sales, color = "Asis")) +
  geom_line(aes(x=release_year, y=mean_namerica_sales, color = "North America")) +
  geom_line(aes(x=release_year, y=mean_europe_sales, color = "Europe")) +
  geom_line(aes(x=release_year, y=mean_japan_sales, color = "Japan")) +
  labs(title= "Average Units Sold Across Regions", x="Year",y="Millions of Dollars")



#avg units sold table
top_avg_sales_db <- summary_avgsales_db %>%
  select(-mean_global_sales) %>%
  rename(Asia = mean_asia_sales,
         Europe = mean_europe_sales,
         `North America` = mean_namerica_sales,
         Japan = mean_japan_sales) %>%
  pivot_longer(cols = c(Asia, `North America`, Europe, Japan),
               names_to = "region", values_to = "sales") %>%
  arrange(desc(sales)) %>%
  relocate(sales, .before = region) %>%
  slice_head(n=5) 

top_avg_sales_db %>%
  kable("html",
        align = 'c',
        col.names = c("Year", 
                      "Avg Units Sold", 
                      "Region")) %>%
  kable_styling(c("striped", "hover"), full_width = T, position = "center")


#average production cost
avg_productioncost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(avg_productioncost = mean(`Production Cost`))

ggplot(avg_productioncost_db)  + 
  geom_line(aes(x=release_year, y=avg_productioncost)) +
  labs(title= "Average Production Cost 2000-2020",
       x="Year",y="Millions of Dollars")


##total production cost table 
top_prodcost_db <- avg_productioncost_db %>%
  rename(`Production Cost` = avg_productioncost,
         Year = release_year) %>%
  arrange(desc(`Production Cost`)) %>%
  slice_head(n=5) 

top_prodcost_db %>%
  kable("html",
        align = 'c',
        col.names = c("Year", 
                      "Avg Production Cost (in millions)")) %>%
  kable_styling(c("striped", "hover"), full_width = T, position = "center")













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


