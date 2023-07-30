##########################################
####   Visualization                  ####
##########################################

#libraries
library(shiny)

setwd('/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo')

#import data
videogamesales_db <- read_excel("video_game_sales_cleaned.xlsx")

#visualize summary tab
source("./tab_visuals_summary.R")




##########################################
####   Shiny UI                       ####
##########################################
ui = fluidPage(
  h1("Video Game Industry Dashboard"),
  selectInput("units_sold", "Units Sold", c("Total", "Average")),
  selectInput("production_cost", "Production Cost", c("Total", "Average")))


##########################################
####   Shiny Server                   ####
##########################################
#where dqtq is manipulated and visualizations are prepared
server = function(input, output) {}

#launch the app
shinyApp(ui = ui, server = server)