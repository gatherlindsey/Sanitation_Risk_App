library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rgdal)
library(raster)
library(plotly)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(viridis)
library(shinycssloaders)
library(sf)

source('server_modules.R')
source('functions.R')

#open the cua5 outline shapefile
CUA5 <- readOGR("Shapefiles/CUA5.shp",GDAL1_integer64_policy = TRUE, layer = 'CUA5')
CUA5 <- spTransform(CUA5, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#open the fokontanies dataset
fok <- readOGR("Shapefiles/fokontanies_edited.shp", GDAL1_integer64_policy = TRUE, layer = 'fokontanies_edited')
fok <- spTransform(fok, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#Changing certain values if they are incorrect
# fok@data$Nom[fok@data$Nom == 'RiziÃƒÂ¨res'] <- 'Riziares'
# fok@data$Nom[fok@data$Nom == 'Analamahitsy CitÃƒÂ©'] <- 'Analamahitsy Cite'
# writeOGR(obj = fok, layer = "fokontanies_edited", "C:/Users/someg/OneDrive/Desktop/Shiny/Shapefiles", driver="ESRI Shapefile")


# CUA5 Risk
CUA5_Risk <- readOGR("Shapefiles/Final_SRI_1.shp",GDAL1_integer64_policy = TRUE, layer = "Final_SRI_1")
CUA5_Risk<- spTransform(CUA5_Risk, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Adding roads
roads <- readOGR("Shapefiles/roads.shp", GDAL1_integer64_policy = TRUE, layer = 'roads')
roads <- spTransform(roads, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#changing the SRI to 2 decimal places!
fok@data[,'SRI1_max']=round(fok@data[,'SRI1_max'],2)
fok@data[,'SRI2_max']=round(fok@data[,'SRI2_max'],2)
fok@data[,'SRI3_max']=round(fok@data[,'SRI3_max'],2)

#create a rank data column
fok@data$rank_sri1 <- NA
fok@data$rank_sri1[order(-fok@data$SRI1_max)] <- 1:nrow(fok@data)

fok@data$rank_sri2 <- NA
fok@data$rank_sri2[order(-fok@data$SRI2_max)] <- 1:nrow(fok@data)

fok@data$rank_sri3 <- NA
fok@data$rank_sri3[order(-fok@data$SRI3_max)] <- 1:nrow(fok@data)

#use this to check what data is available
#CUA5_Risk@data$





# Server ----
server <- function(input, output, session) {
  # Bounding box filter (If the stats are being used)
  boundingBox <- reactive({
    if(!is.null(input$toilets_bounds)){
      Map <- in_bounding_box(df, df$latitude, df$longitude, input$toilets_bounds)
    } else {
      Map
    }
    
  })
  
  
  #create the symbology bins
  pallete <- c("#008837", "#a6dba0", "#f7f7f7", "#c2a5cf", "#7b3294")
  
  
  bins_sri1_grid <- c(0, 0.1120, 0.2370, 0.3750, 0.5610, 1)
  bins_sri1_fok <- c(0, 0.419, 0.525, 0.621, 0.744, 1)
  pal_sri1_grid <- colorBin(pallete, domain = fok@data$SRI1_max, bins = bins_sri1_grid)
  pal_sri1_fok <- colorBin(pallete, domain = fok@data$SRI1_max, bins = bins_sri1_fok)
  
  bins_sri2_grid <- c(0, 0.072, 0.5262, 0.63, 0.739, 1)
  bins_sri2_fok <- c(0, 0.7, 0.797, 0.836, 0.928, 1)
  pal_sri2_grid <- colorBin(pallete, domain = fok@data$SRI2_max, bins = bins_sri2_grid)
  pal_sri2_fok <- colorBin(pallete, domain = fok@data$SRI2_max, bins = bins_sri2_fok)
  
  bins_sri3_grid <- c(0, 0.2750, 0.4190, 0.5430, 0.6890, 1)
  bins_sri3_fok <- c(0, 0.682, 0.75, 0.79, 0.909, 1)
  pal_sri3_grid <- colorBin(pallete, domain = fok@data$SRI3_max, bins = bins_sri3_grid)
  pal_sri3_fok <- colorBin(pallete, domain = fok@data$SRI3_max, bins = bins_sri3_fok)
  
  #Labels for the symbology
  sym_labels<- c("Lowest"," ","Medium"," ", "Highest")
  
  
  #instructions for the pop ups on the map
  popup_sri1 <- paste(
    "<strong>Summary:</strong>",
    "<br><strong>Name: </strong>"
    , fok$Nom
    , "<br><strong>Max Risk: </strong>"
    , fok$SRI1_max
    , "<br><strong>Rank: </strong>"
    , fok$rank_sri1)
  
  popup_sri2 <- paste(
    "<strong>Name: </strong>"
    , fok$Nom
    ,"<br><strong>Max Risk: </strong>"
    , fok$SRI2_max
    , "<br><strong>Rank: </strong>"
    , fok$rank_sri2)
  
  
  popup_sri3 <- paste(
    "<strong>Name: </strong>"
    , fok$Nom
    ,"<br><strong>Max Risk: </strong>"
    , fok$SRI3_max
    , "<br><strong>Rank: </strong>"
    , fok$rank_sri3)
  
  
  
  #the data input for SRI1 tab
  output$SRI1 <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      # addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
      #              opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
      #              fillColor = "Blue") %>% 
      addPolylines(data = roads, color = "Black", weight = 1, smoothFactor = 0.5,
                   opacity = 1, fillOpacity = 0.3,
                   group = "Roads") %>%
      addPolygons(data = fok, fillColor = ~pal_sri1_fok(SRI1_max), weight = 1, smoothFactor = 0.5, #color = "#444444",
                  opacity = 0.2, fillOpacity = 0.7, popup = ~popup_sri1,
                  #label = HTML_labels(fok$SRI1_max, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Fokontany (Max Risk)") %>%
      addPolygons(data = CUA5_Risk, fillColor = ~pal_sri1_grid(SRI1_), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$SRI1_, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Grid") %>%
      addLegend(title = "Risk", 
                position = "bottomleft", 
                values = CUA5_Risk$SRI1_,
                pal = pal_sri1_fok,
                bins = 5,
                labFormat = function(type, cuts, p){  # Here's the trick
                  paste0(sym_labels)}
      ) %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite Map"),
        overlayGroups = c("Fokontany (Max Risk)", "Grid", "Roads"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Grid") %>%
      hideGroup("Roads")
  })
  
  
  output$SRI2 <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      # addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
      #              opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
      #              fillColor = "Blue") %>% 
      addPolylines(data = roads, color = "Black", weight = 1, smoothFactor = 0.5,
                   opacity = 1, fillOpacity = 0.3,
                   group = "Roads") %>%
      addPolygons(data = fok, fillColor = ~pal_sri2_fok(SRI2_max), weight = 1, smoothFactor = 0.5, #color = "#444444",
                  opacity = 0.2, fillOpacity = 0.7, popup = ~popup_sri2,
                  #label = HTML_labels(fok$SRI2_max, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Fokontany (Max Risk)") %>%
      addPolygons(data = CUA5_Risk, fillColor = ~pal_sri2_grid(SRI2_), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$SRI2_, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Grid") %>%
      addLegend(title = "Risk", 
                position = "bottomleft", 
                values = CUA5_Risk@data$SRI2_,
                pal = pal_sri2_fok,
                bins = 5,
                labFormat = function(type, cuts, p){  # Here's the trick
                  paste0(sym_labels)}
      ) %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite Map"),
        overlayGroups = c("Fokontany (Max Risk)", "Grid", "Roads"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Grid") %>%
      hideGroup("Roads")
  })
  
  output$SRI3 <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      # addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
      #              opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
      #              fillColor = "Blue") %>% 
      addPolylines(data = roads, color = "Black", weight = 1, smoothFactor = 0.5,
                   opacity = 1, fillOpacity = 0.3,
                   group = "Roads") %>%
      addPolygons(data = fok, fillColor = ~pal_sri3_fok(SRI3_max), weight = 1, smoothFactor = 0.5, #color = "#444444",
                  opacity = 0.2, fillOpacity = 0.7, popup = ~popup_sri3,
                  #label = HTML_labels(fok$SRI2_max, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Fokontany (Max Risk)") %>%
      addPolygons(data = CUA5_Risk, fillColor = ~pal_sri3_grid(SRI3_), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$SRI3_, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE),
                  group = "Grid") %>%
      addLegend(title = "Risk", 
                position = "bottomleft", 
                values = CUA5_Risk@data$SRI3_,
                pal = pal_sri3_fok,
                bins = 5,
                labFormat = function(type, cuts, p){  # Here's the trick
                  paste0(sym_labels)}
      ) %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite Map"),
        overlayGroups = c("Fokontany (Max Risk)", "Grid", "Roads"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Grid") %>%
      hideGroup("Roads")
  })
  


  output$updates <- renderUI(HTML("<ul>
  <li>04/10/2019 - Added the final texts and toilets individual informations</li>
  <li>02/10/2019 - Change the sample toilets for a best adaptation to the new borders</li>
  <li> 02/10/2019 - Added a new CUA5 shapefile based on an official source</li>
  <li>11/09/2019 - Created the first interactive version</li>
                                  </ul>"))
  
  output$home <- renderUI(HTML("<p>This prototype visualises three versions of geospatial analysis on the same set of 13 indicators to predict the level of risk of uncontained faecal waste in the environment in the 5eme arrondissement in Antananarivo (CUA5). For more information, contact us at hello@gatherhub.org.</p>")
  
)}

# When ready to deploy, it will then ask if you want to update the original app domain. Click yes. 
#it will upload here >> https://sanitationrisk.shinyapps.io/shiny
#library(rsconnect)
#rsconnect::setAccountInfo(name='sanitation-hub', token='82EA28FA57A3CF8359DEAC9326DA0DDE', secret='UXNQCdSIUZB6Wfv7HhhiWf4Nqf+MEJ894mPJWC2s')
#rsconnect::deployApp('C:\\Users\\someg\\dev\\gather\\git\\Sanitation_Risk_App\\Shiny\\Sanitaion_Risk_App\\Sanitation_Risk_App\\Shiny\\', account = 'sanitation-hub')
