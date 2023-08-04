
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
  output$units_sold_instructions <- renderText({paste("To view the number of game units sold across regions, please select a metric from the drop-down menu. The range of years populated can be adjusted through the slider. On the graph, double-click on the legend to isolate regions and single-click to populate additional regions. Select regions in the map to view the summary of units sold for the year range specified.")})
  output$units_sold_table_title <- renderUI({
    HTML(paste("<strong>Highest Years by Sales Volume:<br>", input$UnitsSold_YearRange[1], "to", input$UnitsSold_YearRange[2], "</strong>"))})
  
  output$production_cost_title <- renderText("Production Cost")
  output$production_cost_instructions <- renderText({paste("To view data on production costs each year, please select a metric from the drop-down menu. Use the slider below to adjust the range of years populated.")})
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
    df <- sales_totalavg_sum_db %>% 
      filter(Metric %in% input$units_sold) %>% 
      filter(release_year >= input$UnitsSold_YearRange[1] & release_year <= input$UnitsSold_YearRange[2])
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
                                 "<br>", input$units_sold, "Units Sold: ", round(`Sales`, digits = 2)))) + 
      geom_line(size = .8) + 
      geom_point() +
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
    ggplotly(g, tooltip = "text") %>% config(displayModeBar = F)
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
                            input$units_sold, " Units Sold in Year Range: ", round(Total,digits = 2),
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
      productioncost_dataset <- total_productioncost_db %>% 
        filter(release_year >= input$ProductionCosts_YearRange[1] & release_year <= input$ProductionCosts_YearRange[2])
    }
    else if (input$production_cost == "Average"){
      productioncost_dataset <- avg_productioncost_db %>% 
        filter(release_year >= input$ProductionCosts_YearRange[1] & release_year <= input$ProductionCosts_YearRange[2])
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
      geom_point() +
      ggtitle(paste(input$production_cost, "Production Costs Across Regions")) +
      labs(x="Year",
           y="Millions of Dollars") +
      theme_bw() + 
      theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
            legend.position = "right",
            axis.title.x = element_text(size = 10, vjust = 1.3),
            axis.title.y = element_text(size = 10, vjust = 1.3),
            axis.text = element_text(size = 9))
    ggplotly(g, tooltip = "text") %>% config(displayModeBar = F)
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
      productioncost_dataset_table_df <- table_total_prodcost_db %>% filter(release_year >= input$ProductionCosts_YearRange[1] & release_year <= input$ProductionCosts_YearRange[2]) %>% arrange(desc(`Production Cost`)) %>% slice_head(n=5) 
    }
    else if (input$production_cost == "Average"){
      productioncost_dataset_table_df <- table_avg_prodcost_db %>% filter(release_year >= input$ProductionCosts_YearRange[1] & release_year <= input$ProductionCosts_YearRange[2]) %>% arrange(desc(`Production Cost`)) %>% slice_head(n=5) 
    }
    return(productioncost_dataset_table_df)
  }) 
  
  
  output$productioncost_table <- function()({  
    productioncost_datasetInput_table() %>% 
      kable("html", align = 'c', col.names = c("Year", paste(input$production_cost, "Production Cost*"))) %>% 
      kable_styling(c("striped", "hover"), full_width = T, position = "center") %>%
      add_footnote(("in millions of dollars"), notation = "symbol")
    
  })
  
  


                                              ##########################################
                                              ####  Panel: Platform                 ####
                                              ##########################################
  
##########################################
####    Platform: Units Sold Section  ####
##########################################

#output text for side panel
output$platform_units_sold_title <- renderText("Number of Units Sold")
output$platform_units_sold_instructions <- renderText({paste("To view the number of game units sold across regions, please select a metric from the drop-down menu and a region below. The range of years populated can be adjusted through the slider. Double-click on the legend to isolate platforms in the graph and single-click to populate additional platforms")})
output$platform_units_sold_table_title <- renderUI({
  HTML(paste("<strong>Highest Years by Sales Volume:<br>", input$platform_UnitsSold_YearRange[1], "to", input$platform_UnitsSold_YearRange[2], "</strong>"))})

output$platform_production_cost_title <- renderText("Production Cost")
output$platform_production_cost_instructions <- renderText({paste("To view data on production costs each year, please select a metric from the drop-down menu. Use the slider below to increase or decrease the range of years populated.")})
output$platform_production_cost_table_title <- renderUI({
  HTML(paste("<strong>Highest Years by Production Cost: <br>", input$platform_ProductionCosts_YearRange[1], "to", input$platform_ProductionCosts_YearRange[2], "</strong>"))})

#data
platform_sales_total_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Total Sales")

platform_sales_average_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarize(Global = mean(global_sales),
            Asia = mean(asia_sales),
            `North America` = mean(north_american_sales),
            Europe = mean(european_sales),
            Japan = mean(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Average Sales") %>%
  mutate(`Average Sales` = as.double(formatC(as.double(as.character(round(`Average Sales`, 2))), digits = 2, format = "f")))

platform_sales_totalavg_sum_db <- platform_sales_total_db %>%
  full_join(platform_sales_average_db, by = join_by(release_year, platform, Region)) %>%
  rename(`Total` = `Total Sales`,
         `Average` = `Average Sales`) %>%
  pivot_longer(cols = c(`Total`, `Average`),
               names_to = "Metric", values_to = "Sales")

#return correct dataset based on selection
platform_unitssold_datasetInput <- reactive({
  req(input$platform_units_sold)
  df <- platform_sales_totalavg_sum_db %>% 
    filter(Metric %in% input$platform_units_sold) %>% 
    filter(Region == input$platform_region_radio) %>%
    filter(release_year >= input$platform_UnitsSold_YearRange[1] & release_year <= input$platform_UnitsSold_YearRange[2])
}) 

#plot
output$platform_sales_totalavg_sum_plot <- renderPlotly({
  g <- ggplot(platform_unitssold_datasetInput(), 
              aes(y = Sales, 
                  x = release_year, 
                  color= factor(platform),
                  group = factor(platform),
                  text = paste("Platform: ", platform,
                               "<br>Year: ", release_year,
                               "<br>", input$platform_units_sold, "Sales: ", round(`Sales`, digits = 2)))) + 
    geom_line(size = .8) + 
    geom_point() +
    labs(x="Year",
         y="Millions of Copies Sold",
         color = "platform") +
    ggtitle(paste(input$platform_units_sold, "Video Game Units Sold Across Platform")) +
    theme_bw() + 
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.position = "right",
          axis.title.x = element_text(size = 10, vjust = 1.3),
          axis.title.y = element_text(size = 10, vjust = 1.3),
          axis.text = element_text(size = 9))
  ggplotly(g, tooltip = "text") %>% config(displayModeBar = F)
}) 


#table
platform_sales_TOP_total_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  #filter(!Region == "Global") %>%
  relocate(`Sales`, .before = Region) 

platform_sales_TOP_average_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarize(Global = mean(global_sales),
            Asia = mean(asia_sales),
            `North America` = mean(north_american_sales),
            Europe = mean(european_sales),
            Japan = mean(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  #filter(!Region == "Global") %>%
  relocate(`Sales`, .before = Region) %>%
  mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 2, format = "f")))

#return correct dataset based on selection
platform_unitssold_datasetInput_table <- reactive({
  req(input$platform_units_sold) 
  if (input$platform_units_sold == "Total"){
    platform_sales_table_df <- platform_sales_TOP_total_db %>% 
      ungroup() %>%
      filter(Region == input$platform_region_radio) %>%
      select(-Region) %>%
      filter(release_year >= input$platform_UnitsSold_YearRange[1] & release_year <= input$platform_UnitsSold_YearRange[2]) %>% 
      arrange(desc(`Sales`)) %>% 
      slice_head(n=5) %>%
      relocate(platform, .after = Sales)
    
  }
  else if (input$platform_units_sold == "Average"){
    platform_sales_table_df <- platform_sales_TOP_average_db %>% 
      ungroup() %>%
      filter(Region == input$platform_region_radio) %>%
      select(-Region) %>%
      filter(release_year >= input$platform_UnitsSold_YearRange[1] & release_year <= input$platform_UnitsSold_YearRange[2]) %>% 
      arrange(desc(`Sales`)) %>% 
      slice_head(n=5) %>%
      relocate(platform, .after = Sales)
  }
  return(platform_sales_table_df)
}) 

output$platform_sales_table <- function()({  
  platform_unitssold_datasetInput_table() %>% 
    kable(format ="html", align = 'c', col.names = c("Year", paste(input$units_sold, "Units Sold*"), "Region")) %>% 
    kable_styling(c("striped", "hover"), font_size = 16, full_width = T, position = "center") %>%
    add_footnote(("in millions of copies"), notation = "symbol")
  
})

##########################################
####  Platform: Production Costs      ####
##########################################

#data
platform_total_productioncost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(`Production Cost` = sum(`Production Cost`))

platform_avg_productioncost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(`Production Cost` = mean(`Production Cost`)) %>%
  mutate(`Production Cost` = as.double(formatC(as.double(as.character(round(`Production Cost`, 2))), digits = 2, format = "f")))


#return correct dataset based on selection
platform_productioncost_datasetInput <- reactive({
  req(input$platform_production_cost)
  if (input$platform_production_cost == "Total"){
    platform_productioncost_dataset <- platform_total_productioncost_db %>% filter(release_year >= input$platform_ProductionCosts_YearRange[1] & release_year <= input$platform_ProductionCosts_YearRange[2])
  }
  else if (input$platform_production_cost == "Average"){
    platform_productioncost_dataset <- platform_avg_productioncost_db %>% filter(release_year >= input$platform_ProductionCosts_YearRange[1] & release_year <= input$platform_ProductionCosts_YearRange[2])
  }
  return(platform_productioncost_dataset)
}) 


#plot production costs total
output$platform_productioncost_plot <- renderPlotly({
  g <- ggplot(platform_productioncost_datasetInput(), 
              aes(x=release_year, 
                  y=`Production Cost`, 
                  group = 1,
                  text = paste("Year: ", release_year,
                               "<br>", input$platform_production_cost, "Production Cost: ", round(`Production Cost`, digits = 2)))) + 
    geom_line() + 
    geom_point() +
    ggtitle(paste(input$platform_production_cost, "Production Costs Across Regions")) +
    labs(x="Year",
         y="Millions of Dollars") +
    theme_bw() + 
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.position = "right",
          axis.title.x = element_text(size = 10, vjust = 1.3),
          axis.title.y = element_text(size = 10, vjust = 1.3),
          axis.text = element_text(size = 9))
  ggplotly(g, tooltip = "text") %>% config(displayModeBar = F)
})

#table
#data
platform_table_total_prodcost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(`Production Cost` = sum(`Production Cost`))

platform_table_avg_prodcost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(`Production Cost` = mean(`Production Cost`)) %>%
  mutate(`Production Cost` = as.double(formatC(as.double(as.character(round(`Production Cost`, 2))), digits = 2, format = "f")))



#select correct dataset
platform_productioncost_datasetInput_table <- reactive({
  req(input$platform_production_cost)
  if (input$platform_production_cost == "Total"){
    platform_productioncost_dataset_table <- platform_table_total_prodcost_db %>% filter(release_year >= input$platform_ProductionCosts_YearRange[1] & release_year <= input$platform_ProductionCosts_YearRange[2]) %>% arrange(desc(`Production Cost`)) %>% slice_head(n=5) 
  }
  else if (input$platform_production_cost == "Average"){
    platform_productioncost_dataset_table <- platform_table_avg_prodcost_db %>% filter(release_year >= input$platform_ProductionCosts_YearRange[1] & release_year <= input$platform_ProductionCosts_YearRange[2]) %>% arrange(desc(`Production Cost`)) %>% slice_head(n=5) 
  }
  return(platform_productioncost_dataset_table)
}) 


output$platform_productioncost_table <- function()({  
  platform_productioncost_datasetInput_table() %>% 
    kable("html", align = 'c', col.names = c("Year", paste(input$platform_production_cost, "Production Cost*"))) %>% 
    kable_styling(c("striped", "hover"), full_width = T, position = "center") %>%
    add_footnote(("in millions of dollars"), notation = "symbol")
  
})



##########################################
####   Platform: Units Sold per Region ####
##########################################

output$platform_unitsperregion_title <- renderText("Lifetime Units Sold per Region")
output$platform_unitsperregion_instructions <- renderText({paste("To view data on lifetime units sold per region, please select a metric from the drop-down menu and a region below.")})
output$platform_unitsperregion_table_title <- renderUI({
  HTML(paste("<strong>Highest Lifetime Units Sold by Platform</strong>"))})

#data
platform_total_unitsperregion_db <- videogamesales_db %>%
  group_by(platform) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales")

platform_avg_unitsperregion_db <- videogamesales_db %>%
  group_by(platform) %>%
  summarise(Global = mean(global_sales),
            Asia = mean(asia_sales),
            `North America` = mean(north_american_sales),
            Europe = mean(european_sales),
            Japan = mean(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 2, format = "f")))

#return correct dataset based on selection
platform_unitsperregion_datasetInput <- reactive({
  req(input$platform_unitsperregion)
  if (input$platform_unitsperregion == "Total"){
    platform_unitsperregion_dataset <- platform_total_unitsperregion_db %>% 
      filter(Region == input$platform_unitsperregion_radio)
  }
  else if (input$platform_unitsperregion == "Average"){
    platform_unitsperregion_dataset <- platform_avg_unitsperregion_db %>% 
      filter(Region == input$platform_unitsperregion_radio)
  }
  return(platform_unitsperregion_dataset)
}) 


##plot
output$platform_unitsperregion_plot <- renderPlotly({
  g <- ggplot(platform_unitsperregion_datasetInput(), 
              aes(y = Sales, 
                  x = platform, 
                  fill = platform,
                  group = 1,
                  text = paste("Platform: ", platform,
                               "<br>Region: ", Region,
                               "<br>", input$platform_units_sold, "Sales: ", round(`Sales`, digits = 2)))) +
    geom_bar(stat='identity') +
    labs(x="Platform",
         y="Number of Games Sold (in millions)",
         color = "platform") +
    ggtitle(paste(input$platform_units_sold, "Video Game Sales 2000-2020")) +
    theme_bw() + 
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.position = "right",
          axis.title.x = element_text(size = 10, vjust = 1.3),
          axis.title.y = element_text(size = 10, vjust = 1.3),
          axis.text = element_text(size = 9))
  ggplotly(g, tooltip = "text") %>% config(displayModeBar = F)
}) 



#table
platform_unitsperregion_TOP_total_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  #filter(!Region == "Global") %>%
  relocate(`Sales`, .before = Region) 

platform_unitsperregion_TOP_average_db <- videogamesales_db %>%
  group_by(release_year, platform) %>%
  summarize(Global = mean(global_sales),
            Asia = mean(asia_sales),
            `North America` = mean(north_american_sales),
            Europe = mean(european_sales),
            Japan = mean(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  #filter(!Region == "Global") %>%
  relocate(`Sales`, .before = Region) %>%
  mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 2, format = "f")))

#return correct dataset based on selection
platform_unitsperregion_datasetInput_table <- reactive({
  req(input$platform_unitsperregion) 
  if (input$platform_unitsperregion == "Total"){
    platform_unitsperregion_table_df <- platform_unitsperregion_TOP_total_db %>% 
      ungroup() %>%
      filter(Region == input$platform_unitsperregion_radio) %>%
      select(-Region) %>%
      arrange(desc(`Sales`)) %>% 
      slice_head(n=5) %>%
      relocate(platform, .after = Sales)
    
  }
  else if (input$platform_unitsperregion == "Average"){
    platform_unitsperregion_table_df <- platform_unitsperregion_TOP_average_db %>% 
      ungroup() %>%
      filter(Region == input$platform_unitsperregion_radio) %>%
      select(-Region) %>%
      arrange(desc(`Sales`)) %>% 
      slice_head(n=5) %>%
      relocate(platform, .after = Sales)
  }
  return(platform_unitsperregion_table_df)
}) 

output$platform_unitsperregion_table <- function()({  
  platform_unitsperregion_datasetInput_table() %>% 
    kable(format ="html", align = 'c', col.names = c("Year", paste(input$platform_unitsperregion, "Units Sold*"), "Platform")
          ) %>% 
    kable_styling(c("striped", "hover"), font_size = 16, full_width = T, position = "center") %>%
    add_footnote(("in millions of copies"), notation = "symbol")
  
})




    #####################################################
    ####  Platform: Titles per Region/Platform       ####
    #####################################################


output$platform_titleyear_title <- renderText("Top Selling Titles")
output$platform_titleyear_instructions <- renderText({paste("To view popular game titles per region and platform, check the boxes below.")})
output$platform_titleyear_table_title <- renderUI({
  HTML(paste("<strong>Top Titles by Platform</strong>"))})

# #data
# platform_total_titleyear <- videogamesales_db %>%
#   group_by(release_year, platform, title) %>%
#   summarize(Global = sum(global_sales),
#             Asia = sum(asia_sales),
#             `North America` = sum(north_american_sales),
#             Europe = sum(european_sales),
#             Japan = sum(japan_sales)) %>%
#   pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
#                names_to = "Region", values_to = "n") %>%
#   count(release_year, platform, Region)
# 
# platform_avg_titleyear <- videogamesales_db %>%
#   group_by(release_year, platform, title) %>%
#   summarize(Global = mean(global_sales),
#             Asia = mean(asia_sales),
#             `North America` = mean(north_american_sales),
#             Europe = mean(european_sales),
#             Japan = mean(japan_sales)) %>%
#   pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
#                names_to = "Region", values_to = "n") %>%
#   count(release_year, platform, Region) %>%
#   mutate(`n` = as.double(formatC(as.double(as.character(round(`n`, 2))), digits = 2, format = "f")))
# 
# 
# #select correct dataset
# platform_titleyear_datasetInput_table <- reactive({
#   req(input$platform_titleyear)
#   if (input$platform_titleyear == "Total"){
#     platform_titleyear_dataset <- platform_total_titleyear %>% 
#       filter(release_year >= input$platform_titleyear_YearRange[1] & release_year <= input$platform_titleyear_YearRange[2]) %>%
#       filter(Region %in% input$checkGroup)
#   }
#   else if (input$platform_titleyear == "Average"){
#     platform_titleyear_dataset <- platform_avg_titleyear %>% 
#       filter(release_year >= input$platform_titleyear_YearRange[1] & release_year <= input$platform_titleyear_YearRange[2]) %>%
#       filter(Region %in% input$checkGroup)
#   }
#   return(platform_titleyear_dataset)
# }) 
# 
# 
# ##plot
# output$platform_titleyear_plot <- renderPlotly({
#   g <- ggplot(platform_titleyear_datasetInput_table(),
#               aes(y = n, 
#                   x = release_year, 
#                   fill = platform,
#                   group = platform,
#                   text = paste("Platform: ", platform,
#                                "<br>Region: ", Region,
#                                "<br>", "Number of Titles Released: ", round(`n`, digit = 0)))) +
#     geom_bar(stat='identity') +
#     labs(x="Year",
#          y="Number of Titles Released",
#          color = "platform") +
#     ggtitle(paste(input$platform_titleyear, "Number of Titles Released per Year")) +
#     theme_bw() + 
#     theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
#           legend.position = "right",
#           axis.title.x = element_text(size = 10, vjust = 1.3),
#           axis.title.y = element_text(size = 10, vjust = 1.3),
#           axis.text = element_text(size = 9))
#   ggplotly(g, tooltip = "text")
# }) 



#table
platform_titleyear_table_df <- videogamesales_db %>%
  group_by(platform, title) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  relocate(`Sales`, .before = Region) 

output$platform_titleyear_table <- function()({
  platform_titleyear_table_df %>%
    ungroup() %>%
    filter(Region %in% input$checkGroup) %>%
    filter(platform %in% input$checkGroup_platform) %>%
    group_by(title, platform, Region) %>%
    summarise(Sales = sum(Sales)) %>%
    arrange(desc(`Sales`)) %>%
    ungroup() %>%
    slice_head(n=10) %>%
    relocate(platform, .after = Sales) %>%
    relocate(Region, .after = platform) %>%
    kable(format ="html", align = 'c', col.names = c("Title", "Units Sold*", "Platform", "Region")) %>%
    kable_styling(c("striped", "hover"), font_size = 16, full_width = T, position = "center") %>%
    add_footnote(("in millions of copies"), notation = "symbol")
})







                                                            ##########################################
                                                            ####  Panel: Genre                    ####
                                                            ##########################################  

##########################################
####     Genre: Units Sold Section    ####
##########################################


#output text for side panel
output$genre_units_sold_title <- renderText("Number of Units Sold")
output$genre_units_sold_instructions <- renderText({paste("To view the number of game units sold across regions, please select a metric from the drop-down menu and a region below. The range of years populated can be adjusted through the slider. Double-click on the legend to isolate genres in the graph and single-click to populate additional genres")})
output$genre_units_sold_table_title <- renderUI({
  HTML(paste("<strong>Highest Years by Sales Volume:<br>", input$genre_UnitsSold_YearRange[1], "to", input$genre_UnitsSold_YearRange[2], "</strong>"))})

output$genre_production_cost_title <- renderText("Production Cost")
output$genre_production_cost_instructions <- renderText({paste("To view data on production costs each year, please select a metric from the drop-down menu. Use the slider below to increase or decrease the range of years populated.")})
output$genre_production_cost_table_title <- renderUI({
  HTML(paste("<strong>Highest Years by Production Cost: <br>", input$genre_ProductionCosts_YearRange[1], "to", input$genre_ProductionCosts_YearRange[2], "</strong>"))})

#data
genre_sales_total_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Total Sales")

genre_sales_average_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  summarize(Global = mean(global_sales),
            Asia = mean(asia_sales),
            `North America` = mean(north_american_sales),
            Europe = mean(european_sales),
            Japan = mean(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Average Sales") %>%
  mutate(`Average Sales` = as.double(formatC(as.double(as.character(round(`Average Sales`, 2))), digits = 2, format = "f")))

genre_sales_totalavg_sum_db <- genre_sales_total_db %>%
  full_join(genre_sales_average_db, by = join_by(release_year, genre, Region)) %>%
  rename(`Total` = `Total Sales`,
         `Average` = `Average Sales`) %>%
  pivot_longer(cols = c(`Total`, `Average`),
               names_to = "Metric", values_to = "Sales")

#return correct dataset based on selection
genre_unitssold_datasetInput <- reactive({
  req(input$genre_units_sold)
  df <- genre_sales_totalavg_sum_db %>% 
    filter(Metric %in% input$genre_units_sold) %>% 
    filter(Region == input$genre_region_radio) %>%
    filter(release_year >= input$genre_UnitsSold_YearRange[1] & release_year <= input$genre_UnitsSold_YearRange[2])
}) 

#plot
output$genre_sales_totalavg_sum_plot <- renderPlotly({
  g <- ggplot(genre_unitssold_datasetInput(), 
              aes(y = Sales, 
                  x = release_year, 
                  color= factor(genre),
                  group = factor(genre),
                  text = paste("Genre: ", genre,
                               "<br>Year: ", release_year,
                               "<br>", input$genre_units_sold, "Sales: ", round(`Sales`, digits = 2)))) + 
    geom_line(size = .8) + 
    geom_point() +
    labs(x="Year",
         y="Millions of Copies Sold",
         color = "genre") +
    ggtitle(paste(input$genre_units_sold, "Video Game Units Sold Across Genre")) +
    theme_bw() + 
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.position = "right",
          axis.title.x = element_text(size = 10, vjust = 1.3),
          axis.title.y = element_text(size = 10, vjust = 1.3),
          axis.text = element_text(size = 9))
  ggplotly(g, tooltip = "text") %>% config(displayModeBar = F)
}) 


#table
genre_sales_TOP_total_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  #filter(!Region == "Global") %>%
  relocate(`Sales`, .before = Region) 

genre_sales_TOP_average_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  summarize(Global = mean(global_sales),
            Asia = mean(asia_sales),
            `North America` = mean(north_american_sales),
            Europe = mean(european_sales),
            Japan = mean(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  #filter(!Region == "Global") %>%
  relocate(`Sales`, .before = Region) %>%
  mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 2, format = "f")))

#return correct dataset based on selection
genre_unitssold_datasetInput_table <- reactive({
  req(input$genre_units_sold) 
  if (input$genre_units_sold == "Total"){
    genre_sales_table_df <- genre_sales_TOP_total_db %>% 
      ungroup() %>%
      filter(Region == input$genre_region_radio) %>%
      select(-Region) %>%
      filter(release_year >= input$genre_UnitsSold_YearRange[1] & release_year <= input$genre_UnitsSold_YearRange[2]) %>% 
      arrange(desc(`Sales`)) %>% 
      slice_head(n=5) %>%
      relocate(genre, .after = Sales)
    
  }
  else if (input$genre_units_sold == "Average"){
    genre_sales_table_df <- genre_sales_TOP_average_db %>% 
      ungroup() %>%
      filter(Region == input$genre_region_radio) %>%
      select(-Region) %>%
      filter(release_year >= input$genre_UnitsSold_YearRange[1] & release_year <= input$genre_UnitsSold_YearRange[2]) %>% 
      arrange(desc(`Sales`)) %>% 
      slice_head(n=5) %>%
      relocate(genre, .after = Sales)
  }
  return(genre_sales_table_df)
}) 

output$genre_sales_table <- function()({  
  genre_unitssold_datasetInput_table() %>% 
    kable(format ="html", align = 'c', col.names = c("Year", paste(input$units_sold, "Units Sold*"), "Region")) %>% 
    kable_styling(c("striped", "hover"), font_size = 16, full_width = T, position = "center") %>%
    add_footnote(("in millions of copies"), notation = "symbol")
  
})


##########################################
####     Genre: Production Costs      ####
##########################################


#data
genre_total_productioncost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(`Production Cost` = sum(`Production Cost`))

genre_avg_productioncost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(`Production Cost` = mean(`Production Cost`)) %>%
  mutate(`Production Cost` = as.double(formatC(as.double(as.character(round(`Production Cost`, 2))), digits = 2, format = "f")))


#return correct dataset based on selection
genre_productioncost_datasetInput <- reactive({
  req(input$genre_production_cost)
  if (input$genre_production_cost == "Total"){
    genre_productioncost_dataset <- genre_total_productioncost_db %>% filter(release_year >= input$genre_ProductionCosts_YearRange[1] & release_year <= input$genre_ProductionCosts_YearRange[2])
  }
  else if (input$genre_production_cost == "Average"){
    genre_productioncost_dataset <- genre_avg_productioncost_db %>% filter(release_year >= input$genre_ProductionCosts_YearRange[1] & release_year <= input$genre_ProductionCosts_YearRange[2])
  }
  return(genre_productioncost_dataset)
}) 


#plot production costs total
output$genre_productioncost_plot <- renderPlotly({
  g <- ggplot(genre_productioncost_datasetInput(), 
              aes(x=release_year, 
                  y=`Production Cost`, 
                  group = 1,
                  text = paste("Year: ", release_year,
                               "<br>", input$genre_production_cost, "Production Cost: ", round(`Production Cost`, digits = 2)))) + 
    geom_area(fill = "lightblue") + 
    geom_point(color = "darkblue") +
    geom_line(color = "darkblue") +
    ggtitle(paste(input$genre_production_cost, "Production Costs Across Regions")) +
    labs(x="Year",
         y="Millions of Dollars") +
    theme_bw() + 
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.position = "right",
          axis.title.x = element_text(size = 10, vjust = 1.3),
          axis.title.y = element_text(size = 10, vjust = 1.3),
          axis.text = element_text(size = 9))
  ggplotly(g, tooltip = "text") %>% config(displayModeBar = F)
})

#table
#data
genre_table_total_prodcost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(`Production Cost` = sum(`Production Cost`))

genre_table_avg_prodcost_db <- videogamesales_db %>%
  group_by(release_year) %>%
  summarise(`Production Cost` = mean(`Production Cost`)) %>%
  mutate(`Production Cost` = as.double(formatC(as.double(as.character(round(`Production Cost`, 2))), digits = 2, format = "f")))



#select correct dataset
genre_productioncost_datasetInput_table <- reactive({
  req(input$genre_production_cost)
  if (input$genre_production_cost == "Total"){
    genre_productioncost_dataset_table <- genre_table_total_prodcost_db %>% filter(release_year >= input$genre_ProductionCosts_YearRange[1] & release_year <= input$genre_ProductionCosts_YearRange[2]) %>% arrange(desc(`Production Cost`)) %>% slice_head(n=5) 
  }
  else if (input$genre_production_cost == "Average"){
    genre_productioncost_dataset_table <- genre_table_avg_prodcost_db %>% filter(release_year >= input$genre_ProductionCosts_YearRange[1] & release_year <= input$genre_ProductionCosts_YearRange[2]) %>% arrange(desc(`Production Cost`)) %>% slice_head(n=5) 
  }
  return(genre_productioncost_dataset_table)
}) 


output$genre_productioncost_table <- function()({  
  genre_productioncost_datasetInput_table() %>% 
    kable("html", align = 'c', col.names = c("Year", paste(input$genre_production_cost, "Production Cost*"))) %>% 
    kable_styling(c("striped", "hover"), full_width = T, position = "center") %>%
    add_footnote(("in millions of dollars"), notation = "symbol")
  
})



##########################################
####     Genre: Units Sold per Region ####
##########################################
output$genre_unitsperregion_title <- renderText("Lifetime Units Sold per Region")
output$genre_unitsperregion_instructions <- renderText({paste("To view data on lifetime units sold per region, please select a metric from the drop-down menu and a region below.")})
output$genre_unitsperregion_table_title <- renderUI({
  HTML(paste("<strong>Highest Lifetime Units Sold by Genre</strong>"))})

#data
genre_total_unitsperregion_db <- videogamesales_db %>%
  group_by(genre) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") 

genre_avg_unitsperregion_db <- videogamesales_db %>%
  group_by(genre) %>%
  summarise(Global = mean(global_sales),
            Asia = mean(asia_sales),
            `North America` = mean(north_american_sales),
            Europe = mean(european_sales),
            Japan = mean(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 2, format = "f")))

#return correct dataset based on selection
genre_unitsperregion_datasetInput <- reactive({
  req(input$genre_unitsperregion)
  if (input$genre_unitsperregion == "Total"){
    genre_unitsperregion_dataset <- genre_total_unitsperregion_db %>% 
      filter(Region == input$genre_unitsperregion_radio) %>%
      mutate(Sales = (Sales/(sum(Sales)))*100) %>%
      mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 0, format = "f")))
  }
  else if (input$genre_unitsperregion == "Average"){
    genre_unitsperregion_dataset <- genre_avg_unitsperregion_db %>% 
      filter(Region == input$genre_unitsperregion_radio) %>%
      mutate(Sales = (Sales/(sum(Sales)))*100) %>%
      mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 0, format = "f")))
  }
  return(genre_unitsperregion_dataset)
}) 


##plot
output$genre_unitsperregion_plot <- renderPlot({
    ggplot(genre_unitsperregion_datasetInput(), 
                aes(y = Sales, 
                    x = "", 
                    fill = genre)) +
      geom_bar(stat='identity', width=1, color="white") +
      labs(x="genre",
           y="Number of Games Sold (in millions)",
           color = "genre") +
      ggtitle(paste(input$genre_unitsperregion, "Video Game Sales 2000-2020")) +
      theme_void() + 
      theme(plot.title = element_text(size=19, face="bold", hjust = 0.5),
            legend.position = "right") +
      scale_fill_discrete(name = "Genre")+
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(Sales, "%")), position = position_stack(vjust=0.5), color = "white", size = 5)
    
  })  


#table
genre_unitsperregion_TOP_total_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  #filter(!Region == "Global") %>%
  relocate(`Sales`, .before = Region) 

genre_unitsperregion_TOP_average_db <- videogamesales_db %>%
  group_by(release_year, genre) %>%
  summarize(Global = mean(global_sales),
            Asia = mean(asia_sales),
            `North America` = mean(north_american_sales),
            Europe = mean(european_sales),
            Japan = mean(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  #filter(!Region == "Global") %>%
  relocate(`Sales`, .before = Region) %>%
  mutate(`Sales` = as.double(formatC(as.double(as.character(round(`Sales`, 2))), digits = 2, format = "f")))

#return correct dataset based on selection
genre_unitsperregion_datasetInput_table <- reactive({
  req(input$genre_unitsperregion) 
  if (input$genre_unitsperregion == "Total"){
    genre_unitsperregion_table_df <- genre_unitsperregion_TOP_total_db %>% 
      ungroup() %>%
      filter(Region == input$genre_unitsperregion_radio) %>%
      select(-Region) %>%
      arrange(desc(`Sales`)) %>% 
      slice_head(n=5) %>%
      relocate(genre, .after = Sales)
    
  }
  else if (input$genre_unitsperregion == "Average"){
    genre_unitsperregion_table_df <- genre_unitsperregion_TOP_average_db %>% 
      ungroup() %>%
      filter(Region == input$genre_unitsperregion_radio) %>%
      select(-Region) %>%
      arrange(desc(`Sales`)) %>% 
      slice_head(n=5) %>%
      relocate(genre, .after = Sales)
  }
  return(genre_unitsperregion_table_df)
}) 

output$genre_unitsperregion_table <- function()({  
  genre_unitsperregion_datasetInput_table() %>% 
    kable(format ="html", align = 'c', col.names = c("Year", paste(input$genre_unitsperregion, "Units Sold*"), "Genre")
    ) %>% 
    kable_styling(c("striped", "hover"), font_size = 16, full_width = T, position = "center") %>%
    add_footnote(("in millions of copies"), notation = "symbol")
  
})




###################################################
####     Genre: Titles per Region/Platform     ####
###################################################  
output$genre_titleyear_title <- renderText("Top Selling Titles")
output$genre_titleyear_instructions <- renderText({paste("To view popular game titles per region and genre, check the boxes below.")})
output$genre_titleyear_table_title <- renderUI({
  HTML(paste("<strong>Top Titles by Genre</strong>"))})


#table
genre_titleyear_table_df <- videogamesales_db %>%
  group_by(genre, title) %>%
  summarise(Global = sum(global_sales),
            Asia = sum(asia_sales),
            `North America` = sum(north_american_sales),
            Europe = sum(european_sales),
            Japan = sum(japan_sales)) %>%
  pivot_longer(cols = c(Global, Asia, `North America`, Europe, Japan),
               names_to = "Region", values_to = "Sales") %>%
  relocate(`Sales`, .before = Region) %>%
  relocate(Region, .after = genre)

output$genre_titleyear_table <- function()({
  genre_titleyear_table_df %>%
    ungroup() %>%
    filter(Region %in% input$checkGroup_genre_region) %>%
    filter(genre %in% input$checkGroup_genre_genre) %>%
    group_by(title, genre, Region) %>%
    summarise(Sales = sum(Sales)) %>%
    arrange(desc(`Sales`)) %>%
    ungroup() %>%
    slice_head(n=10) %>%
    relocate(genre, .after = Sales) %>%
    relocate(Region, .after = genre) %>%
    kable(format ="html", align = 'c', col.names = c("Title", "Units Sold*", "Genre", "Region")) %>%
    kable_styling(c("striped", "hover"), font_size = 16, full_width = T, position = "center") %>%
    add_footnote(("in millions of copies"), notation = "symbol")
})






# ------------------
# Documentation Page
# ------------------

# ------------------
# About Page
# ------------------


}