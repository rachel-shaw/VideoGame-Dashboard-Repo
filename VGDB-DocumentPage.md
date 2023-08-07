---
output:
  html_document:
    theme: flatly
    highlight: pygments
    #css: styles.css
    number_sections: false
    fig_caption: true
    toc: true
    toc_float: true
---

# Technical Walkthrough of Dashboard

The dashboard was built with RShiny. The structure of the code base is outlined below. Please use the table of contents to the left to fast travel to your desired section. 

## 1. The Dataset

The dataset used in this dashboard is entitled **"Video Game Sales Dataset"**, written by Gregory Smith and downloaded from [Kaggle.com](https://www.kaggle.com/datasets/mathurtanvi/video-game-sales-dataset).

This dataset contains the following fields:

**Title:** The name of the video game\
**Platform:** Platform of the game's release (*Playstation (PS), Playstation 2 (PS2), PlayStation 3 (PS3), Playstation 4 (PS4), Playstation 5 (PS5), PC, Linux, Nintendo DS, iOS, PlayStation Network (PSN)*)\
**Year:** The year the game released\
**Genre:** Genre of the game (*Adventure, Fighting, Racing, Puzzle, Shooter, Simulation, Sports, Board Game*)\
**Publisher:** Publisher of the game\
**Vg_Score:** Vgchartz critical score (out of 10)\
**Critic_Score:** Metacritic crital score (out of 10)\
**User_Score:** User critic score (out of 10)\
**Total_Shipped:** Total number of units shipped\
**North_American_Sales:** Number of units sold in North America (in millions)\
**Europe_Sales:** Number units sold in Europe (in millions)\
**Japan_Sales:** Number of units sold in Japan (in millions)\
**Asia_Sales:** Number of units sold in Asia, other than Japan (in millions)\
**Global_Sales:** Number of units sold worldwide (in millions)\
**Production_Cost:** Cost of producing the game (in millions)\

The dataset was cleaned in RStudio by checking for missing values, removing unwanted columns, correcting typos, etc. The data was outputted as an excel file and an RDS file (to be used in RShiny).


```r
#open libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)

#read in data
videogamesales_db <- read_excel("video_game_sales.xlsx")

#check for NAs, missing information
table(is.na(videogamesales_db))
```

```
## 
##  FALSE 
## 133440
```

```r
sapply(videogamesales_db, function(columns)
  sum(is.na(columns)))
```

```
##                title             platform                genre            publisher             vg_score         critic_score           user_score        total_shipped 
##                    0                    0                    0                    0                    0                    0                    0                    0 
##          aisan_sales north_american_sales          japan_sales       european_sales         global_sales      Production Cost         release_year 
##                    0                    0                    0                    0                    0                    0                    0
```

```r
##clean column names
videogamesales_db <- videogamesales_db %>%
  mutate(asia_sales = aisan_sales) %>%
  select(-c(aisan_sales, vg_score, critic_score, user_score, total_shipped)) %>%
  relocate(asia_sales, .before = north_american_sales) 

##write clean dataset
write.xlsx(videogamesales_db, "video_game_sales_cleaned.xlsx")

#save as RDS (used in RShiny)
saveRDS(videogamesales_db, file = "video_game_sales_cleaned.rds")
```


## 2. Main Page

The **Main Page** of the dashboard was developed using two main scripts: the ***user interface (UI)*** script and the ***server*** script. 

> ### A. User Interface (UI) Script

> ### B. Server Script

## 3. Documentation Page

This page was written as an RMarkdown file and read into the RShiny dashboard.

## 4. About Page

This page was written in ___ and read into the RShiny dashboard.


