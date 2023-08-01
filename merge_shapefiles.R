##add shapefile for regions
#original_sf <- sf::st_read("shapefiles/World_Countries_Generalized.shp", stringsAsFactors = F) %>%
# st_transform() %>%
#   st_zm() %>%
#   as("Spatial") #%>%
#filter(utility %!in% "Northglenn")
# 
# north_america <- st_as_sf(original_sf) %>%
#   filter(COUNTRY == c("United States", "Canada")) %>%
#   spTransform("+proj=longlat +datum=WGS84 +no_defs")  
# st_union()
#original_sf@data <- original_sf@data

original_sf <- readOGR("shapefiles/World_Continents/World_Continents.shp") %>%
  spTransform("+proj=longlat +datum=WGS84 +no_defs")
  #raster::aggregate() 

original_sf@data

plot(original_sf)


#japan
japan <- readOGR("shapefiles/World_Countries/World_Countries_Generalized.shp") %>%
  spTransform("+proj=longlat +datum=WGS84 +no_defs") %>%
  subset(COUNTRY == "Japan") %>%
  st_as_sf()

#japan <- spChFIDs(japan, paste("japan", row.names(japan), sep="."))

plot(japan)

#asia
asia <- original_sf %>%
  subset(CONTINENT == "Asia") %>%
  spTransform("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_sf()

plot(asia)

  #remove japan from asia
  asia_cut <- st_difference(asia, japan) %>%
    select(!c(FID.1, COUNTRY, ISO, COUNTRYAFF, AFF_ISO, SHAPE_Leng, SHAPE_Area))
  asia_cut <- as(asia_cut, 'Spatial')
  plot(asia_cut)
  
  japan_cut <- st_intersection(asia, japan) %>%
    select(!c(FID.1, COUNTRY, ISO, COUNTRYAFF, AFF_ISO, SHAPE_Leng, SHAPE_Area)) %>%
    mutate(CONTINENT = case_when(CONTINENT == "Asia" ~ "Japan", TRUE ~ CONTINENT))
  japan_cut <- as(japan_cut, 'Spatial')
  plot(japan_cut)

  #remove original asia from original shp
  # original_sf_cut <- original_sf %>%
  #   subset(!CONTINENT %in% c("Asia", "Africa"))
  
  #cut out north america
  north_america_cut <- original_sf %>%
    subset(CONTINENT == "North America")
  
  #cut out europe
  europe_cut <- original_sf %>%
    subset(CONTINENT == "Europe")


  


  
#merge
asia_cut@data
japan_cut@data
original_sf_cut@data
north_america_cut$data
europe@data

MergedShapes2 <- rbind(north_america_cut, europe_cut, asia_cut, japan_cut) 

MergedShapes2@data

plot(MergedShapes2)

raster::shapefile(MergedShapes2, file= "/Users/rachel/Documents/GitHub/VideoGame-Dashboard-Repo/shapefiles/Merged_Shapes/Merged_Shapes2", overwrite=TRUE)
  