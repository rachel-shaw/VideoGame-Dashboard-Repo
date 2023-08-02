
##########################################
####   Shiny Server                   ####
##########################################
#where dqtq is manipulated and visualizations are prepared
server = function(input, output, session) {
  
  #import data
  #videogamesales_db <- read_excel("video_game_sales_cleaned.xlsx")
  videogamesales_db <- readRDS("video_game_sales_cleaned.rds")
  tail(videogamesales_db)
  dim(videogamesales_db)
 

  # ------------------
  # Main Page
  # ------------------ 
  
                                                    ##########################################
                                                    ####  Panel: Summary                  ####
                                                    ##########################################
  

  ##########################################
  ####  Summary: Units Sold Section     ####
  ##########################################
  
  #output text for side panel
  output$welcome <- renderText({
    paste("Welcome to the Video Game Industry Dashboard!")
  })
  
  output$welcome_details <- renderUI({
    HTML(paste("This", "<b>", "summary", "</b>", "page provides an overview of video game sales and production costs from 2000 to 2020. If you'd like an in-depth analysis, use the tabs above to view game sales by", "<b>", "platform", "</b>", "and", "<b>", "genre.", "</b>", "<br>", "Happy analyzing!"))
  })
  
  output$tabs_above_instructions <- renderUI({
    HTML(paste("Interested in learning more about how this dashboard was created? Use the", "<b>", "Documentation", "</b>", "and", "<b>", "About", "</b>", "tabs at the top of the page to view the code used in RShiny and learn about the analyst."))
  })
  
  output$units_sold_title <- renderText("Number of Units Sold")
  output$units_sold_instructions <- renderText({paste("To view the number of game units sold across regions, please select a metric from the drop-down menu. The range of years populated can be adjusted through the slider.")})
  output$units_sold_table_title <- renderUI({
    HTML(paste("<strong>Highest Years by Sales Volume:<br>", input$UnitsSold_YearRange[1], "to", input$UnitsSold_YearRange[2], "</strong>"))})
  
  output$production_cost_title <- renderText("Production Cost")
  output$production_cost_instructions <- renderText({paste("To view data on production costs each year, please select a metric from the drop-down menu. Use the slider below to increase or decrease the range of years populated.")})
  output$production_cost_table_title <- renderUI({
    HTML(paste("<strong>Highest Years by Production Cost: <br>", input$ProductionCosts_YearRange[1], "to", input$ProductionCosts_YearRange[2], "</strong>"))})
  
  #data
  sales_total_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarise(Global = sum(global_sales),
              Asia = sum(asia_sales),
              `North America` = sum(north_american_sales),
              Europe = sum(european_sales),
              Japan = sum(japan_sales)) %>%
    pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
                 names_to = "Region", values_to = "Total Sales")
  
  sales_average_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarize(Global = mean(global_sales),
              Asia = mean(asia_sales),
              `North America` = mean(north_american_sales),
              Europe = mean(european_sales),
              Japan = mean(japan_sales)) %>%
    pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
                 names_to = "Region", values_to = "Average Sales") %>%
    mutate(`Average Sales` = as.double(formatC(as.double(as.character(round(`Average Sales`, 2))), digits = 2, format = "f")))
  
  sales_totalavg_sum_db <- sales_total_db %>%
    full_join(sales_average_db, by = join_by(release_year, Region)) %>%
    rename(`Total` = `Total Sales`,
           `Average` = `Average Sales`) %>%
    pivot_longer(cols = c(`Total`, `Average`),
                 names_to = "Metric", values_to = "Sales")  %>%
    filter(!Region == "Global") 
  
  #return correct dataset based on selection
  unitssold_datasetInput <- reactive({
    req(input$units_sold)
    df <- sales_totalavg_sum_db %>% filter(Metric %in% input$units_sold) %>% filter(release_year >= input$UnitsSold_YearRange[1] & release_year <= input$UnitsSold_YearRange[2])
  }) 
  
  #plot
  output$sales_totalavg_sum_plot <- renderPlotly({
    g <- ggplot(unitssold_datasetInput(), 
                aes(y = Sales, 
                    x = release_year, 
                    color= factor(Region),
                    group = factor(Region),
                    text = paste("Region: ", Region,
                                 "<br>Year: ", release_year,
                                 "<br>", input$units_sold, "Sales: ", round(`Sales`, digits = 2)))) + 
      geom_line(size = .8) + 
      labs(x="Year",
           y="Millions of Copies Sold",
           color = "Regions") +
      ggtitle(paste(input$units_sold, "Video Game Units Sold Across Regions")) +
      theme_bw() + 
      theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
            legend.position = "right",
            axis.title.x = element_text(size = 10, vjust = 1.3),
            axis.title.y = element_text(size = 10, vjust = 1.3),
            axis.text = element_text(size = 9))
    ggplotly(g, tooltip = "text")
  })
  
  
  #table
  sales_TOP_total_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarise(Global = sum(global_sales),
              Asia = sum(asia_sales),
              `North America` = sum(north_american_sales),
              Europe = sum(european_sales),
              Japan = sum(japan_sales)) %>%
    pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
                 names_to = "Region", values_to = "Sales") %>%
    filter(!Region == "Global") %>%
    relocate(`Sales`, .before = Region) 
  
  sales_TOP_average_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarize(Global = mean(global_sales),
              Asia = mean(asia_sales),
              `North America` = mean(north_american_sales),
              Europe = mean(european_sales),
              Japan = mean(japan_sales)) %>%
    pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
                 names_to = "Region", values_to = "Sales") %>%
    filter(!Region == "Global") %>%
    relocate(`Sales`, .before = Region) %>%
    mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 2, format = "f")))
  
  #return correct dataset based on selection
  unitssold_datasetInput_table <- reactive({
    req(input$units_sold)
    if (input$units_sold == "Total"){
      sales_totalavg_sum_table_df <- sales_TOP_total_db %>% filter(release_year >= input$UnitsSold_YearRange[1] & release_year <= input$UnitsSold_YearRange[2]) %>% arrange(desc(`Sales`)) %>% slice_head(n=5) 
    }
    else if (input$units_sold == "Average"){
      sales_totalavg_sum_table_df <- sales_TOP_average_db %>% filter(release_year >= input$UnitsSold_YearRange[1] & release_year <= input$UnitsSold_YearRange[2]) %>% arrange(desc(`Sales`)) %>% slice_head(n=5) 
    }
    return(sales_totalavg_sum_table_df)
  }) 
  
  output$sales_totalavg_sum_table <- function()({  
    unitssold_datasetInput_table() %>% 
      kable(format ="html", align = 'c', col.names = c("Year", paste(input$units_sold, "Units Sold*"), "Region")) %>% 
      kable_styling(c("striped", "hover"), font_size = 16, full_width = T, position = "center") %>%
      add_footnote(("in millions of copies"), notation = "symbol")
    
  })
  
  
  #add map
  region_map <- sf::st_read("shapefiles/Merged_Shapes/Merged_Shapes2.shp", stringsAsFactors = F) %>%
    st_transform() %>%
    st_zm() %>%
    as("Spatial")
  
  region_map@data <- region_map@data %>%
    mutate(Region = CONTINENT) %>%
    select(-CONTINENT) %>%
    relocate(Region, .after = FID) 
  
  #Make a static map
  WorldMap <- leaflet(data= region_map, options = leafletOptions(center = c(90,90), minZoom = 1.4)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(noWrap = TRUE)) %>%
    fitBounds(lng1 = 0, lat1 = 90,
              lng2 = 0, lat2 = -50) 
  
  output$map <- renderLeaflet({WorldMap})
  
  ##data for map
  map_datasetInput <- reactive({
    req(input$units_sold)
    if (input$units_sold == "Total"){
      map_df <- sales_total_db %>% 
        rename(Total = `Total Sales`) %>%
        filter(!Region == "Global") %>%
        filter(release_year >= input$UnitsSold_YearRange[1] & release_year <= input$UnitsSold_YearRange[2]) %>% 
        group_by(Region) %>%
        summarize(Total = sum(Total)) %>%
        mutate(Percent = (Total/sum(Total))*100)}
    else if (input$units_sold == "Average"){
      map_df <- sales_total_db %>% 
        rename(Total = `Total Sales`) %>%
        filter(!Region == "Global") %>%
        filter(release_year >= input$UnitsSold_YearRange[1] & release_year <= input$UnitsSold_YearRange[2]) %>% 
        group_by(Region) %>%
        summarize(Total = mean(Total)) %>%
        mutate(Percent = (Total/sum(Total))*100)}
    return(map_df)
  }) 
  
  
  observe({
    req(input$units_sold)
    req(input$UnitsSold_YearRange)
    
    region_map@data <- region_map@data %>%
      full_join(map_datasetInput(),by="Region") %>%
      mutate(label = paste0("<strong>",Region,"</strong>","<br/>",
                            input$units_sold, " Sales in Year Range: ", round(Total,digits = 2),
                            "<br/>", 
                            "Percent of Global Sales: ", round(Percent,digits = 2), "%"))
    
    #define colors for heatmap
    pal <- colorNumeric(
      palette = "RdYlBu",reverse = TRUE,
      domain = map_datasetInput()$Percent)
    
    data <- region_map
    
    leafletProxy("map",data= data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data=data,
                  popup = ~label,
                  color = "black",
                  weight = 1,
                  fillColor = ~pal(Percent),
                  fillOpacity = 0.7,
                  layerId = ~Region) %>%
      addLegend("bottomright", pal = pal, map_datasetInput()$Percent,
                title = "Percent",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1)
  })
  
  ##########################################
  ####  Summary: Production Cost        ####
  ##########################################
  
  #output text for selected production cost metric
  output$selected_productioncost <- renderText({
    paste("Showing results for", input$production_cost, "Video Game Production Costs")
  })
  
  #data
  total_productioncost_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarise(`Production Cost` = sum(`Production Cost`))
  
  avg_productioncost_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarise(`Production Cost` = mean(`Production Cost`)) %>%
    mutate(`Production Cost` = as.double(formatC(as.double(as.character(round(`Production Cost`, 2))), digits = 2, format = "f")))
  
  
  #return correct dataset based on selection
  productioncost_datasetInput <- reactive({
    req(input$production_cost)
    if (input$production_cost == "Total"){
      productioncost_dataset <- total_productioncost_db %>% filter(release_year >= input$ProductionCosts_YearRange[1] & release_year <= input$ProductionCosts_YearRange[2])
    }
    else if (input$production_cost == "Average"){
      productioncost_dataset <- avg_productioncost_db %>% filter(release_year >= input$ProductionCosts_YearRange[1] & release_year <= input$ProductionCosts_YearRange[2])
    }
    return(productioncost_dataset)
  }) 
  
  
  #plot production costs total
  output$productioncost_plot <- renderPlotly({
    g <- ggplot(productioncost_datasetInput(), 
                aes(x=release_year, 
                    y=`Production Cost`, 
                    group = 1,
                    text = paste("Year: ", release_year,
                                 "<br>", input$production_cost, "Production Cost: ", round(`Production Cost`, digits = 2)))) + 
      geom_line() + 
      ggtitle(paste(input$production_cost, "Production Costs Across Regions")) +
      labs(x="Year",
           y="Millions of Dollars") +
      theme_bw() + 
      theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
            legend.position = "right",
            axis.title.x = element_text(size = 10, vjust = 1.3),
            axis.title.y = element_text(size = 10, vjust = 1.3),
            axis.text = element_text(size = 9))
    ggplotly(g, tooltip = "text")
  })
  
  #table
  #data
  table_total_prodcost_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarise(`Production Cost` = sum(`Production Cost`))
  
  table_avg_prodcost_db <- videogamesales_db %>%
    group_by(release_year) %>%
    summarise(`Production Cost` = mean(`Production Cost`)) %>%
    mutate(`Production Cost` = as.double(formatC(as.double(as.character(round(`Production Cost`, 2))), digits = 2, format = "f")))
  
  
  
  #select correct dataset
  productioncost_datasetInput_table <- reactive({
    req(input$production_cost)
    if (input$production_cost == "Total"){
      productioncost_dataset_table <- table_total_prodcost_db %>% filter(release_year >= input$ProductionCosts_YearRange[1] & release_year <= input$ProductionCosts_YearRange[2]) %>% arrange(desc(`Production Cost`)) %>% slice_head(n=5) 
    }
    else if (input$production_cost == "Average"){
      productioncost_dataset_table <- table_avg_prodcost_db %>% filter(release_year >= input$ProductionCosts_YearRange[1] & release_year <= input$ProductionCosts_YearRange[2]) %>% arrange(desc(`Production Cost`)) %>% slice_head(n=5) 
    }
    return(productioncost_dataset_table)
  }) 
  
  
  output$productioncost_table <- function()({  
    productioncost_datasetInput_table() %>% 
      kable("html", align = 'c', col.names = c("Year", paste(input$production_cost, "Production Cost*"))) %>% 
      kable_styling(c("striped", "hover"), full_width = T, position = "center") %>%
      add_footnote(("in millions of dollars"), notation = "symbol")
    
  })
  
  
}

  ##########################################
  ####  Panel: Platform                 ####
  ##########################################
  
##########################################
####    Platform: Units Sold Section  ####
##########################################

##########################################
####  Platform: Production Costs      ####
##########################################

##########################################
####   Plaform: Units Sold per Region ####
##########################################

##########################################
####  Platform: Titles per Year       ####
##########################################  



  ##########################################
  ####  Panel: Genre                    ####
  ##########################################  

##########################################
####     Genre: Units Sold Section    ####
##########################################

##########################################
####     Genre: Production Costs      ####
##########################################

##########################################
####     Genre: Units Sold per Region ####
##########################################

##########################################
####     Genre: Titles per Year       ####
##########################################  

# ------------------
# Documentation Page
# ------------------

# ------------------
# About Page
# ------------------
