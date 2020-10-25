library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)

source('ui_modules.R')

lorem_ipsum <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Vivamus arcu felis bibendum ut tristique et egestas quis. Et odio pellentesque diam volutpat commodo sed egestas egestas fringilla. Malesuada fames ac turpis egestas integer. Nulla posuere sollicitudin aliquam ultrices sagittis orci a. Libero id faucibus nisl tincidunt eget. Mi bibendum neque egestas congue. Proin sed libero enim sed. Erat imperdiet sed euismod nisi porta lorem. Velit ut tortor pretium viverra suspendisse potenti nullam ac. Mauris vitae ultricies leo integer malesuada nunc vel. Vivamus at augue eget arcu dictum varius duis at. Eleifend mi in nulla posuere sollicitudin aliquam ultrices sagittis orci. Aenean et tortor at risus viverra adipiscing."

# User interface ----
ui <- tagList(
  tags$head(tags$script(type="text/javascript", src = "code.js")),
  navbarPage(title = "Sanitation Data Platform", id = "nav", theme = "style.css",
             tabPanel('Home', value = -1,
                      fluidRow( class = "updateTitle",
                                column(4, "Sanitation Data Platform: Geospatial Visualisations for three Sanitation Risk Indices for Antananarivo, Madagascar", div(style = "height:30px;"), offset = 4)
                      ),
                      fluidRow(class = "updateArea",
                               column(4, uiOutput(outputId = 'home'), offset = 4)
                      )),
             tabPanel("SRI1", value = 0,
                      leafletOutput(outputId = "SRI1", height = 700) %>% withSpinner(type = 4),
                      absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                    draggable = F, top =140, left = "auto", right = 50, bottom = 20,
                                    width = 400, height = 600,
                                    style = "overflow-y: scroll;",
                                    #Stats on the side
                                    br(),
                                    p(id = "mainText", style="text-align: left;", "SRI1 considers household density to be of equal importance to all the other 12 indicators. This is to account for the additional risk that high household density poses in areas where sanitation facilities are poor (Hathi, et al. 2017)."),
                                    p(id = "mainText", style="text-align: left;", "This index predicts a high level of uncontained faecal waste in the southwest, with another hotspot in the northwest of the study area. The northeast and east are generally predicted to have a low level of risk. This index generates values with few areas that have a significantly higher or lower risk than the others. "),
                                    p(id= "mainText", style="text-align: left;", "SRI1 = (Flood risk + Road density + Terrain movement + Households sharing toilet + Main drinking water source + Toilet type + Toilet location + Population density + Children aged under 5 per household + Rent + Tax + Open defecation) * Household density"))
                                    ),
             tabPanel("SRI2", value = 1,
                      leafletOutput(outputId = "SRI2", height = 700) %>% withSpinner(type = 4),
                      absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                    draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
                                    width = 400, height = 600,
                                    style = "overflow-y: scroll;",
                                    #Stats on the side
                                    br(),
                                    p(id = "mainText", style="text-align: left;","With SRI2, we see a much greater spread in areas that are predicted to have lower rates of uncontained faecal matter in the environment. Areas of high risk are largely located in the northeast and west of the study area, whereas the southeast is predicted to be at lower risk."), 
                                    p(id = "mainText", style="text-align: left;","Similar to SRI1, this index is limited by the assumption that all of the indicators are of equal importance to one another, limiting its accuracy. To improve this, we need to be able to weight each indicator according to its importance. "),
                                    p(id = "mainText", style="text-align: left;","SRI2 = Average (Flood risk + Road density + Terrain movement + Open defecation + Households sharing toilet + Main drinking water source + Toilet type + Population density + Household density + Toilet location + Children aged under 5 per household + Rent + Tax)"))

             ), tabPanel("SRI3", value = 2,
                         leafletOutput(outputId = "SRI3", height = 700) %>% withSpinner(type = 4),
                         absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                       draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
                                       width = 400, height = 600, 
                                       style = "overflow-y: scroll;",
                                       br(),
                                       p(id = "mainText", style="text-align: left;", "For SRI3, we utilised the AHP method to develop weights for each indicator according to how important it was in predicting the level of uncontained faecal waste in the environment. It is clear from the weightings that the experts we consulted viewed environmental indicators - flood risk and terrain movement - as the key indicators that predict the level of uncontained faecal waste in the environment."),
                                       p(id = "mainText", style="text-align: left;", "This index shows hotspots in the northeast and northwest, and areas of low risk in the east. It correlates closely to flood risk dataset, as this was the indicator that was most influential by the experts and therefore has the highest weighting. This index gives the most even spread of values across all risk values. "),
                                       p(id = "mainText", style="text-align: left;", "SRI3 = (Flood risk*0.17 + Terrain Movement*0.17 + Road density*0.12 + Household density*0.10 + Population density*0.10 + Rent*0.08 + Tax*0.07 + Main drinking water source*0.04 + Children aged under 5 per household*0.04 + Open Defecation*0.04 + Households sharing toilet*0.03 + Toilet type*0.02 + Toilet location*0.02)"))
                            
                         ),
             #this is in the www folder                        
             tabPanel("Data Sources", value = 5,
                       tags$iframe(class = 'leaflet-container', style="height:400px; width:100%; scrolling=yes", src="Datasets_sources.pdf")),   

            #this is a potential site Updates if people wanted it
            #  tabPanel("Site Updates", value = 5,
            #           fluidRow( class = "updateTitle",
            #             column(4, "Site Updates", div(style = "height:30px;"), offset = 4)
            #           ),
            #           fluidRow(class = "updateArea",
            #             column(4, uiOutput(outputId = 'updates'), offset = 4)
            # )),
            #           
            useShinyjs()
             ))

# When ready to deploy, it will then ask if you want to update the original app domain. Click yes. 
#it will upload here >> https://sanitationrisk.shinyapps.io/shiny
#library(rsconnect)
#rsconnect::setAccountInfo(name='sanitation-hub', token='82EA28FA57A3CF8359DEAC9326DA0DDE', secret='UXNQCdSIUZB6Wfv7HhhiWf4Nqf+MEJ894mPJWC2s')
#rsconnect::deployApp("", account = 'sanitation-hub')
#getwd()
