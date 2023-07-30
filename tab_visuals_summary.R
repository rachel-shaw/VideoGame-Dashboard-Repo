#remove(list = ls())

#libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(tidyr)

#setwd('/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo')
#getwd()

#read in data
#videogamesales_db <- read_excel("video_game_sales_cleaned.xlsx")


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














###PRODUCTION
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



