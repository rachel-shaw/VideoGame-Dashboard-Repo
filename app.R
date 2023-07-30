##########################################
####   Visualization                  ####
##########################################

#libraries
library(shiny)
library(ggplot2)
library(dplyr)


setwd('/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo')

#import data

#visualize summary tab
#source("./tab_visuals_summary.R")







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
      
       # if (input$selected_unitssold == "Total"){
      #    unitssold_dataset <- summary_sales_db
      #  }
      #  else if (input$selected_unitssold == "Average"){
      #    unitssold_dataset <- summary_avgsales_db
      # }
      #  return(unitssold_dataset)
      #})

    
  #plot
  output$plot <- renderPlot({
    g <- ggplot(unitssold_datasetInput(), aes(y = Sales, x = release_year, color= factor(Region))) 
    g + geom_line() + labs(x="Year",y="Millions of Copies Sold")
  })
  
  

  
  
  
  
  ##PRODUCTION COST SECTION
  #output text for selected production cost metric
  output$selected_productioncost <- renderText({
    paste("Showing results for", input$production_cost, "Video Game Production Costs")
    })
  
  #data
    summary_productioncost_db <- videogamesales_db %>%
      group_by(release_year) %>%
      summarise(productioncost = sum(`Production Cost`))
  
    avg_productioncost_db <- videogamesales_db %>%
      group_by(release_year) %>%
      summarise(productioncost = mean(`Production Cost`))
    
    #return correct dataset based on selection
    productioncost_datasetInput <- reactive({
      req(input$production_cost)
      if (input$production_cost == "Total"){
        productioncost_dataset <- summary_productioncost_db
      }
      else if (input$production_cost == "Average"){
        productioncost_dataset <- avg_productioncost_db
      }
      return(productioncost_dataset)
    }) 


  #plot production costs total

  output$productioncost_total_plot <- renderPlot({
    g <- ggplot(productioncost_datasetInput(), aes(x=release_year, y=productioncost))
    g + geom_line() + labs(x="Year",y="Millions of Dollars")
  })
  
}


##########################################
####   Shiny UI                       ####
##########################################
ui = fluidPage(
  h1("Video Game Industry Dashboard"),
  
  #add drop down section to select metric for units sold
  selectInput(inputId = "units_sold", 
              label = "Units Sold", 
              list("Total", "Average")),
  textOutput("selected_unitssold"),
  plotOutput("plot"),
  
  #add drop down section to select metric for production cost
  selectInput(inputId = "production_cost", 
              label = "Production Cost", 
              list("Total", "Average")),
  textOutput("selected_productioncost"),
  plotOutput("productioncost_total_plot"),
  
)


#launch the app
shinyApp(ui = ui, server = server)