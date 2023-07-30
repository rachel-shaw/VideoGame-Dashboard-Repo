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
      
      sidebarLayout(
        
        sidebarPanel(
          #add drop down section to select metric for units sold
          selectInput(inputId = "units_sold",
                      label = "Units Sold",
                      list("Total", "Average")),
          textOutput("selected_unitssold"),
          sliderInput("YearRange",
                      label = "Year Range",
                      min = 2000,
                      max = 2020,
                      value = c(2000, 2020)),
          
          selectInput(inputId = "production_cost", 
                      label = "Production Cost", 
                      list("Total", "Average")),
          textOutput("selected_productioncost")
        ),
        
        mainPanel(
          plotOutput("units_sold_plot"),
          tableOutput("units_sold_table"),
          
          plotOutput("productioncost_plot"),
          tableOutput("productioncost_table"),
        )
        
        
      )
    ),
    
    
    
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
    )
  )
),

    ################################################
    #### Panel: Documentation                   ####
    ################################################
    
    tabPanel("Documentation",
             fluidPage(htmlOutput("doc"))),
    
    ################################################
    #### Panel: About                           ####
    ################################################
    tabPanel("About",
             fluidPage(htmlOutput("abo")))
)


# ui = fluidPage(
#   h1("Video Game Industry Dashboard"),
#   
#   #add drop down section to select metric for units sold
#   selectInput(inputId = "units_sold", 
#               label = "Units Sold", 
#               list("Total", "Average")),
#   textOutput("selected_unitssold"),
#   plotOutput("units_sold_plot"),
#   tableOutput("units_sold_table"),
#   
#   #add drop down section to select metric for production cost
#   selectInput(inputId = "production_cost", 
#               label = "Production Cost", 
#               list("Total", "Average")),
#   textOutput("selected_productioncost"),
#   plotOutput("productioncost_plot"),
#   
# )









##########################################
####   Shiny Server                   ####
##########################################
#where dqtq is manipulated and visualizations are prepared
server = function(input, output) {
  
  #import data
  videogamesales_db <- read_excel("video_game_sales_cleaned.xlsx")
  
  ##UNITS SOLD SECTION
  #output text for selected units sold metric
  output$selected_unitssold <- renderText({
      paste("Showing results for", input$units_sold, "Video Game Units Sold Across Regions")
    })

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
        df <- units_sales_db %>% filter(Metric %in% input$units_sold)
      }) 
      
  #plot
  output$units_sold_plot <- renderPlot({
    g <- ggplot(unitssold_datasetInput(), aes(y = Sales, x = release_year, color= factor(Region))) 
    g + geom_line() + labs(x="Year",y="Millions of Copies Sold")
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
    g + geom_line() + labs(x="Year",y="Millions of Dollars")
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