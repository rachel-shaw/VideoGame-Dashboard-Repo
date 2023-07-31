##########################################
####   Visualization                  ####
##########################################

#libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(knitr)
library(shinythemes)

setwd('/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo')

#import data

#visualize summary tab
#source("./tab_visuals_summary.R")




##########################################
####   Shiny UI                       ####
##########################################
# ------------------
# Main title section
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
          
          ##UNITS SOLD PANEL
          column(12, align = "center", 
          h2(textOutput("welcome")),
          tags$br(),
          htmlOutput("welcome_details"),
          
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
                sliderInput("UnitsSold_YearRange",
                            label = "Year Range",
                            min = 2000,
                            max = 2020,
                            value = c(2000,2020)),
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
                  column(8, plotOutput("units_sold_plot")),
                  column(4, tableOutput("units_sold_table")))
                )
              ), #end of sidebar layout
              
              tags$hr(),
          
          ##PRODUCTION COST PANEL    
          sidebarLayout(
              sidebarPanel(
              #add drop down section to select metric for production costs
                h3(textOutput("production_cost_title")),
                textOutput("production_cost_instructions"),
              tags$br(),
              selectInput(inputId = "production_cost", 
                            label = "Select Metric", 
                            list("Total", "Average")),
                br(),
              ), #end of sidebarPanel
              
              mainPanel(
                fluidRow(
                  column(8, plotOutput("productioncost_plot")),
                  column(4, tableOutput("productioncost_table"))),
                
                tags$br(),
                tags$br(),)  
             ),#end of sidebarLayout
                
              tags$hr(),
          
          column(12, align = "center", 
              textOutput("tabs_above_instructions")),
              
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
        
    ),
      
      
    ##########################################
    ####  Panel: Genre                    ####
    ##########################################     
    tabPanel(
      "Genre"
        
    ),
      
    

      
      )),

  # ------------------
  # Documentation Section
  # ------------------

    
  tabPanel("Documentation",
           fluidPage(htmlOutput("doc"))),

  # ------------------
  # About Section
  # ------------------
  
  tabPanel("About",
           fluidPage(htmlOutput("abo"))),
  
  
  # credit banner----
  footer = (div(
    windowTitle = "",
    img(src = "Dashboard_Credit_Banner.jpg", width = "100%", class = "bg"),
  )),
  
  
) #end of navbarPage











##########################################
####   Shiny Server                   ####
##########################################
#where dqtq is manipulated and visualizations are prepared
server = function(input, output, session) {
  
  #import data
  videogamesales_db <- read_excel("video_game_sales_cleaned.xlsx")
  
  ##UNITS SOLD SECTION
  #output text for side panel
  output$welcome <- renderText({
    paste("Welcome to the Video Game Industry Dashboard!")
  })
  
  output$welcome_details <- renderUI({
    HTML(paste("This summary page provides an overview of video game sales and production costs from 2000 to 2020. If you'd like an in-depth analysis, use the tabs above to view game sales by platform and genre.", "Happy analyzing!", sep = "<br>"))
  })
  
  output$tabs_above_instructions <- renderText({
      paste("Interested in learning more about how this dashboard was created? Use the `Documentation` and `About` tabs at the top of the page to view the code used in RShiny and learn about the analyst.")
    })

  output$units_sold_title <- renderText("Number of Units Sold")
  output$units_sold_instructions <- renderText({paste("To view results of the number of game units old, please select a metric from the drop-down menu. The range of years populated in the graph can be adjusted through the slider.")})
  
  output$production_cost_title <- renderText("Production Cost")
  output$production_cost_instructions <- renderText({paste("To view data on production costs each year, please select a metric from the drop-down menu.")})
  
  #data
  summary_sales_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarise(Global = sum(global_sales),
              Asia = sum(asia_sales),
              `North America` = sum(north_american_sales),
              Europe = sum(european_sales),
              Japan = sum(japan_sales)) %>%
    pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
                 names_to = "Region", values_to = "Total Sales")
  
  summary_avgsales_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarize(Global = mean(global_sales),
              Asia = mean(asia_sales),
              `North America` = mean(north_american_sales),
              Europe = mean(european_sales),
              Japan = mean(japan_sales)) %>%
    pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
                 names_to = "Region", values_to = "Average Sales")
  
  units_sales_db <- summary_sales_db %>%
    full_join(summary_avgsales_db, by = join_by(release_year, Region)) %>%
    rename(`Total` = `Total Sales`,
           `Average` = `Average Sales`) %>%
    pivot_longer(cols = c(`Total`, `Average`),
                 names_to = "Metric", values_to = "Sales")  %>%
    filter(!Region == "Global") 
  
      #return correct dataset based on selection
      unitssold_datasetInput <- reactive({
        req(input$units_sold)
        df <- units_sales_db %>% filter(Metric %in% input$units_sold) %>% filter(release_year >= input$UnitsSold_YearRange[1] & release_year <= input$UnitsSold_YearRange[2])
      }) 
      
  #plot
  output$units_sold_plot <- renderPlot({
    g <- ggplot(unitssold_datasetInput(), 
                aes(y = Sales, 
                    x = release_year, 
                    color= factor(Region))) 
    g + geom_line(size = 1) + 
      labs(x="Year",
           y="Millions of Copies Sold",
           color = "Regions") +
      ggtitle(paste(input$units_sold, "Video Game Units Sold Across Regions")) +
      theme_bw() + 
      theme(plot.title = element_text(size=16, face="bold", hjust = 0.5),
            legend.position = "right",
            axis.title.x = element_text(size = 14, vjust = 1.3),
            axis.title.y = element_text(size = 14, vjust = 1.3),
            axis.text = element_text(size = 12))
  })
  
  #table
  top_sales_db <- summary_sales_db %>%
    filter(!Region == "Global") %>%
    relocate(`Total Sales`, .before = Region) %>%
    arrange(desc(`Total Sales`)) %>%
    slice_head(n=5) 

  output$units_sold_table <- function()({  
   top_sales_db %>% kable("html", align = 'c', col.names = c("Year", "Total Units Sold", "Region")) %>% kable_styling(c("striped", "hover"), full_width = T, position = "center")
})
  
  
  
  
  ##PRODUCTION COST SECTION
  #output text for selected production cost metric
  output$selected_productioncost <- renderText({
    paste("Showing results for", input$production_cost, "Video Game Production Costs")
    })
  
  #data
    total_productioncost_db <- videogamesales_db %>%
      group_by(release_year) %>%
      summarise(productioncost = sum(`Production Cost`))
  
    avg_productioncost_db <- videogamesales_db %>%
      group_by(release_year) %>%
      summarise(productioncost = mean(`Production Cost`))
    
    #return correct dataset based on selection
    productioncost_datasetInput <- reactive({
      req(input$production_cost)
      if (input$production_cost == "Total"){
        productioncost_dataset <- total_productioncost_db
      }
      else if (input$production_cost == "Average"){
        productioncost_dataset <- avg_productioncost_db
      }
      return(productioncost_dataset)
    }) 


  #plot production costs total
  output$productioncost_plot <- renderPlot({
    g <- ggplot(productioncost_datasetInput(), aes(x=release_year, y=productioncost))
    g + geom_line() + 
      labs(x="Year",
           y="Millions of Dollars") +
    theme_bw() + 
      theme(legend.position = "right",
            axis.title.x = element_text(size = 14, vjust = 1.3),
            axis.title.y = element_text(size = 14, vjust = 1.3),
            axis.text = element_text(size = 12))
    })
  
  #table
  top_prodcost_db <- total_productioncost_db %>%
    rename(`Production Cost` = productioncost,
           Year = release_year) %>%
    arrange(desc(`Production Cost`)) %>%
    slice_head(n=5) 
  
  output$productioncost_table <- function()({  
    top_prodcost_db %>% kable("html", align = 'c', col.names = c("Year", "Average Units Sold")) %>% kable_styling(c("striped", "hover"), full_width = T, position = "center")
  })
  
  
}


#launch the app
shinyApp(ui = ui, server = server)