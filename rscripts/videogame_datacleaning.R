remove(list = ls())

#libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(tidyr)
library(openxlsx)

setwd('/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo')
getwd()

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

##write clean dataset
write.xlsx(videogamesales_db, "video_game_sales_cleaned.xlsx")

#save as RDS
saveRDS(videogamesales_db, file = "video_game_sales_cleaned.rds")




