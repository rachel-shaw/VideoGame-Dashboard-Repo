##########################################
####   Visualization                  ####
##########################################

#libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(knitr)
library(shinythemes)
library(kableExtra)
library(readxl)
library(leaflet)
library(sf)
library(sp)
library(rgdal)
library(plotly)



#setwd('/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo')

#import data

#visualize summary tab
#source("./tab_visuals_summary.R")

##########################################
####   Shiny UI                       ####
##########################################
# ------------------
# Main Page
# ------------------

ui = navbarPage(
  "R. Shaw Portfoilo",
  theme = shinytheme("flatly"),
  tabPanel(
    "Main",
    
    # App title ----
    titlePanel(div(
      windowTitle = "",
      img(src = "titlebanner_blue.jpg", width = "100%", class = "bg"),
    )),
  
                                                    ##########################################
                                                    ####  Panel: Summary                  ####
                                                    ##########################################
    
    

    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        
        fluidPage(
        
        fluidRow(
          
          ##########################################
          ####  Summary: Units Sold Section     ####
          ##########################################
          column(12, align = "center", 
          h2(textOutput("welcome")),
          tags$br(),
          h4(htmlOutput("welcome_details")),
          
          tags$hr(),
          ),
          
          sidebarLayout(
              sidebarPanel(
                #add drop down section to select metric for units sold
                h3(textOutput("units_sold_title")),
                textOutput("units_sold_instructions"),
                tags$br(),
                
                selectInput(inputId = "units_sold",
                            label = "Select Metric",
                            list("Total", "Average")),
                tags$br(),
                
                sliderInput("UnitsSold_YearRange",
                            label = "Year Range",
                            min = 2000,
                            max = 2020,
                            value = c(2000,2020),
                            sep = ""),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                
              ),
              
              mainPanel(
                tags$br(),
                fluidRow(
                  column(8, plotlyOutput("sales_totalavg_sum_plot")),
                  h4(htmlOutput("units_sold_table_title"), align = "center"),
                  column(4, tableOutput("sales_totalavg_sum_table")),
                  tags$br(),
                  
                  column(12, leafletOutput("map")),
                  )
                ),
              
              ), #end of sidebar layout
             
              tags$hr(),
          
          ##########################################
          #### Summary: Production Cost Section ####
          ##########################################
          sidebarLayout(
              sidebarPanel(
              #add drop down section to select metric for production costs
                h3(textOutput("production_cost_title")),
                textOutput("production_cost_instructions"),
              tags$br(),
              selectInput(inputId = "production_cost", 
                            label = "Select Metric", 
                            list("Total", "Average")),
              sliderInput("ProductionCosts_YearRange",
                          label = "Year Range",
                          min = 2000,
                          max = 2020,
                          value = c(2000,2020),
                          sep = ""),
                br(),
              ), #end of sidebarPanel
              
              mainPanel(
                fluidRow(
                  column(8, plotlyOutput("productioncost_plot")),
                  h4(htmlOutput("production_cost_table_title"), align = "center"),
                  column(4, tableOutput("productioncost_table"))),
                
                tags$br(),
                tags$br(),)  
             ),#end of sidebarLayout
                
              tags$hr(),
          
          column(12, align = "center", 
              h4(htmlOutput("tabs_above_instructions"))),
              
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br()
          ))),
    
    
                                                ##########################################
                                                ####  Panel: Platform                 ####
                                                ##########################################
    tabPanel(
      "Platform",

      fluidPage(

        fluidRow(

          ##########################################
          ####  Platform: Units Sold Section    ####
          ##########################################
          sidebarLayout(
            sidebarPanel(
              #add drop down section to select metric for units sold
              h3(textOutput("platform_units_sold_title")),
              textOutput("platform_units_sold_instructions"),
              tags$br(),

              selectInput(inputId = "platform_units_sold",
                          label = "Select Metric",
                          list("Total", "Average")),
              tags$br(),
              
              radioButtons("platform_region_radio",
                           label = "Select Region",
                           choices = list(
                             "Global" = "Global",
                             "North America" = "North America",
                             "Europe" = "Europe",
                             "Japan" = "Japan",
                             "Asia" = "Asia"),
                           selected = "Global",
                           inline=TRUE),
              
              tags$br(),
              
              sliderInput("platform_UnitsSold_YearRange",
                          label = "Year Range",
                          min = 2000,
                          max = 2020,
                          value = c(2000,2020),
                          sep = ""),             
              
              tags$br(),
              tags$br(),
            ),

            mainPanel(
              tags$br(),
              fluidRow(
                column(8, plotlyOutput("platform_sales_totalavg_sum_plot")),
                h4(htmlOutput("platform_units_sold_table_title"), align = "center"),
                column(4, tableOutput("platform_sales_table")),
                tags$br(),
              )
            ),

          ), #end of sidebar layout

          tags$hr(),

          ##########################################
          ####  Platform: Production Costs      ####
          ##########################################
          sidebarLayout(
            sidebarPanel(
              #add drop down section to select metric for production costs
              h3(textOutput("platform_production_cost_title")),
              textOutput("platform_production_cost_instructions"),
              tags$br(),
              selectInput(inputId = "platform_production_cost",
                          label = "Select Metric",
                          list("Total", "Average")),
              sliderInput("platform_ProductionCosts_YearRange",
                          label = "Year Range",
                          min = 2000,
                          max = 2020,
                          value = c(2000,2020),
                          sep = ""),
              br(),
            ), #end of sidebarPanel

            mainPanel(
              fluidRow(
                column(8, plotlyOutput("platform_productioncost_plot")),
                h4(htmlOutput("platform_production_cost_table_title"), align = "center"),
                column(4, tableOutput("platform_productioncost_table"))),

              tags$br(),
              tags$br(),)
          ),#end of sidebarLayout

          tags$hr(),

          ##########################################
          ####  Platform: Units per Region      ####
          ##########################################
          
          sidebarLayout(
            sidebarPanel(
              #add drop down section to select metric for production costs
              h3(textOutput("platform_unitsperregion_title")),
              textOutput("platform_unitsperregion_instructions"),
              
              tags$br(),
              
              selectInput(inputId = "platform_unitsperregion",
                          label = "Select Metric",
                          list("Total", "Average")),
              
              tags$br(),
      
              radioButtons("platform_unitsperregion_radio",
                           label = "Select Region",
                           choices = list(
                             "Global" = "Global",
                             "North America" = "North America",
                             "Europe" = "Europe",
                             "Japan" = "Japan",
                             "Asia" = "Asia"),
                           selected = "Global",
                           inline=TRUE),

              br(),
            ), #end of sidebarPanel
            
            mainPanel(
              fluidRow(
                column(8, plotlyOutput("platform_unitsperregion_plot")),
                h4(htmlOutput("platform_unitsperregion_table_title"), align = "center"),
                column(4, tableOutput("platform_unitsperregion_table"))),
              
              tags$br(),
              tags$br(),)
          ),#end of sidebarLayout
          
          
          
          
          tags$hr(),
          
          
          #####################################################
          ####  Platform: Titles per Region/Platform       ####
          #####################################################
          

          sidebarLayout(
            sidebarPanel(
              #add drop down section to select metric for production costs
              h3(textOutput("platform_titleyear_title")),
              textOutput("platform_titleyear_instructions"),

              tags$br(),

              # selectInput(inputId = "platform_titleyear",
              #             label = "Select Metric",
              #             list("Total", "Average")),
              # 
              #tags$br(),
              
              checkboxGroupInput("checkGroup",
                label = "Select Region",
                choices = list(
                  "Global" = "Global",
                  "North America" = "North America",
                  "Europe" = "Europe",
                  "Japan" = "Japan",
                  "Asia" = "Asia"),
                selected = list(
                  "Global" = "Global")),
              
              checkboxGroupInput("checkGroup_platform",
                                 label = "Select Platform",
                                 choices = list(
                                   "PC" = "PC",
                                   "Linux" = "Linux",
                                   "PS" = "PS",
                                   "PS2" = "PS2",
                                   "PS3" = "PS3",
                                   "PS4" = "PS4",
                                   "PS5" = "PS5",
                                   "DS" = "DS",
                                   "iOS" = "iOS",
                                   "PSN" = "PSN"),
                                 selected = list(
                                   "PC" = "PC",
                                   "Linux" = "Linux",
                                   "PS" = "PS",
                                   "PS2" = "PS2",
                                   "PS3" = "PS3",
                                   "PS4" = "PS4",
                                   "PS5" = "PS5",
                                   "DS" = "DS",
                                   "iOS" = "iOS",
                                   "PSN" = "PSN")),
              
              br(),
              
              # sliderInput("platform_titlyear_YearRange",
              #             label = "Year Range",
              #             min = 2000,
              #             max = 2020,
              #             value = c(2000,2020)),

              #br(),

            ), #end of sidebarPanel

            mainPanel(
              fluidRow(
                #column(8, plotlyOutput("platform_titleyear_plot")),
                h4(htmlOutput("platform_titleyear_table_title"), align = "center"),
                column(12, tableOutput("platform_titleyear_table"))),

              tags$br(),
              tags$br(),)
          ),#end of sidebarLayout
          
          
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br()
        ))
      ),
  

    
    
    
    
    
    
    
                                                  ##########################################
                                                  ####  Panel: Genre                    ####
                                                  ##########################################     

    tabPanel(
      "Genre",
      
      fluidPage(
        
        fluidRow(
          
          ##########################################
          ####  genre: Units Sold Section    ####
          ##########################################
          sidebarLayout(
            sidebarPanel(
              #add drop down section to select metric for units sold
              h3(textOutput("genre_units_sold_title")),
              textOutput("genre_units_sold_instructions"),
              tags$br(),
              
              selectInput(inputId = "genre_units_sold",
                          label = "Select Metric",
                          list("Total", "Average")),
              tags$br(),
              
              radioButtons("genre_region_radio",
                           label = "Select Region",
                           choices = list(
                             "Global" = "Global",
                             "North America" = "North America",
                             "Europe" = "Europe",
                             "Japan" = "Japan",
                             "Asia" = "Asia"),
                           selected = "Global",
                           inline=TRUE),
              
              tags$br(),
              
              sliderInput("genre_UnitsSold_YearRange",
                          label = "Year Range",
                          min = 2000,
                          max = 2020,
                          value = c(2000,2020),
                          sep = ""),             
              
              tags$br(),
              tags$br(),
            ),
            
            mainPanel(
              tags$br(),
              fluidRow(
                column(8, plotlyOutput("genre_sales_totalavg_sum_plot")),
                h4(htmlOutput("genre_units_sold_table_title"), align = "center"),
                column(4, tableOutput("genre_sales_table")),
                tags$br(),
              )
            ),
            
          ), #end of sidebar layout
          
          tags$hr(),
          
          ##########################################
          ####  genre: Production Costs      ####
          ##########################################
          sidebarLayout(
            sidebarPanel(
              #add drop down section to select metric for production costs
              h3(textOutput("genre_production_cost_title")),
              textOutput("genre_production_cost_instructions"),
              tags$br(),
              selectInput(inputId = "genre_production_cost",
                          label = "Select Metric",
                          list("Total", "Average")),
              sliderInput("genre_ProductionCosts_YearRange",
                          label = "Year Range",
                          min = 2000,
                          max = 2020,
                          value = c(2000,2020),
                          sep = ""),
              br(),
            ), #end of sidebarPanel
            
            mainPanel(
              fluidRow(
                column(8, plotlyOutput("genre_productioncost_plot")),
                h4(htmlOutput("genre_production_cost_table_title"), align = "center"),
                column(4, tableOutput("genre_productioncost_table"))),
              
              tags$br(),
              tags$br(),)
          ),#end of sidebarLayout
          
          tags$hr(),
          
          ##########################################
          ####  genre: Units per Region      ####
          ##########################################
          
          sidebarLayout(
            sidebarPanel(
              #add drop down section to select metric for production costs
              h3(textOutput("genre_unitsperregion_title")),
              textOutput("genre_unitsperregion_instructions"),
              
              tags$br(),
              
              selectInput(inputId = "genre_unitsperregion",
                          label = "Select Metric",
                          list("Total", "Average")),
              
              tags$br(),
              
              radioButtons("genre_unitsperregion_radio",
                           label = "Select Region",
                           choices = list(
                             "Global" = "Global",
                             "North America" = "North America",
                             "Europe" = "Europe",
                             "Japan" = "Japan",
                             "Asia" = "Asia"),
                           selected = "Global",
                           inline=TRUE),
              
              br(),
            ), #end of sidebarPanel
            
            mainPanel(
              fluidRow(
                column(8, plotOutput("genre_unitsperregion_plot")),
                h4(htmlOutput("genre_unitsperregion_table_title"), align = "center"),
                column(4, tableOutput("genre_unitsperregion_table"))),
              
              tags$br(),
              tags$br(),)
          ),#end of sidebarLayout
          
          
          
          
          tags$hr(),
          
          
          #####################################################
          ####  genre: Titles per Region/genre       ####
          #####################################################
          sidebarLayout(
            sidebarPanel(
              #add drop down section to select metric for production costs
              h3(textOutput("genre_titleyear_title")),
              textOutput("genre_titleyear_instructions"),
              
              tags$br(),
              
              checkboxGroupInput("checkGroup_genre_region",
                                 label = "Select Region",
                                 choices = list(
                                   "Global" = "Global",
                                   "North America" = "North America",
                                   "Europe" = "Europe",
                                   "Japan" = "Japan",
                                   "Asia" = "Asia"),
                                 selected = list(
                                   "Global" = "Global")),
              
              checkboxGroupInput("checkGroup_genre_genre",
                                 label = "Select genre",
                                 choices = list(
                                   "Adventure" = "Adventure",
                                   "Fighting" = "Fighting",
                                   "Racing" = "Racing",
                                   "Puzzle" = "Puzzle",
                                   "Shooter" = "Shooter",
                                   "Simulation" = "Simulation",
                                   "Sports" = "Sports",
                                   "Board Game" = "Board Game"),
                                 selected = list(
                                   "Adventure" = "Adventure",
                                   "Fighting" = "Fighting",
                                   "Racing" = "Racing",
                                   "Puzzle" = "Puzzle",
                                   "Shooter" = "Shooter",
                                   "Simulation" = "Simulation",
                                   "Sports" = "Sports",
                                   "Board Game" = "Board Game")),
              
              br(),
              
            ), #end of sidebarPanel
            
            mainPanel(
              fluidRow(
                h4(htmlOutput("genre_titleyear_table_title"), align = "center"),
                column(12, tableOutput("genre_titleyear_table"))),
              
              tags$br(),
              tags$br(),)
            
          ),#end of sidebarLayout
          
          
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br()
        ))
    ),
    
    

    
       
    
      )),

  # ------------------
  # Documentation Page
  # ------------------

  #tabPanel('How to use it?', uiOutput('markdown'))
  
  tabPanel("Documentation",
           fluidPage(uiOutput("VGDBDocumentPage"))),

  # ------------------
  # About Page
  # ------------------
  
  tabPanel("About",
           fluidPage(htmlOutput("abo"))),
  
  
  # credit banner----
  footer = (div(
    windowTitle = "",
    img(src = "Dashboard_Credit_Banner_small.jpg", width = "100%", class = "bg"),
  )),
  
  
) #end of navbarPage











##########################################
####   Shiny Server                   ####
##########################################

#read server script
source("rscripts/server.R")

#launch the app
shinyApp(ui = ui, server = server)