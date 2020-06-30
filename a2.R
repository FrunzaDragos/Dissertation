#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library("RColorBrewer")
library(leaflet.extras)
library(rgdal)
library(dplyr)
library(shinyWidgets)

#reading in roan and elephant data
eles<- read.csv(paste('../Disso/elemydatuse.csv', sep=''))
roan <- read.csv(paste('../Disso/roanmydatuse.csv', sep=''))

#reading covariate data and transforming location param
covar_shape<-readOGR(dsn = paste('../Disso/covariate shape files', sep=''), layer = 'covariates_grid_ref')
covardat<-covar_shape@data
covardat<-data.frame(covardat, coordinates(covar_shape))
xy <- data.frame(ID = 1:nrow(covardat), X = covardat$coords.x1, Y = covardat$coords.x2)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
res <- spTransform(xy, CRS("+init=epsg:32733"))
covardat$x.pos<- res@coords[,1]
covardat$y.pos<- res@coords[,2]
covardat$area<-0.25

#trasforming roan and elephant data to long-lat
mapdata <- eles
coordinates(mapdata) <- ~x.pos+y.pos #similar to SpatialPoints
proj4string(mapdata) <- CRS("+proj=utm +zone=33 +south") #assign projection and coordinate reference system
longlats <- spTransform(mapdata, CRS("+proj=longlat")) #transform
longlats <- as.data.frame(longlats)

mapdata <- roan
coordinates(mapdata) <- ~x.pos+y.pos #similar to SpatialPoints
proj4string(mapdata) <- CRS("+proj=utm +zone=33 +south") #assign projection and coordinate reference system
longlats2 <- spTransform(mapdata, CRS("+proj=longlat")) #transform
longlats2 <- as.data.frame(longlats2)



#ui
ui <- fluidPage(
  
  #map containig the response
  leafletOutput("response_map"),
  
  #map-containing the covariates
  leafletOutput("covariate_map"),
  
  #slider
  sliderTextInput(inputId = "slider1",
                  label = "Select Month and Year" ,
                  choices = unique(longlats$yearmonth),
                  animate = FALSE, grid = FALSE,
                  hide_min_max = FALSE, from_fixed = FALSE,
                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                  post = NULL, dragRange = TRUE)
)


#server
server <- function(input, output, session) {
  
  #reactive data object for filtering from the slider input
  filteredData <- reactive({
    longlats %>%
      filter(yearmonth == input$slider1)
  })
  
  filteredData2 <- reactive({
    longlats2 %>%
      filter(yearmonth == input$slider1)
  })
  
  #building the response map
  output$response_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      
      #heatmaps for the response variales
      addHeatmap(data = filteredData(), lng = ~x.pos, lat = ~y.pos, intensity = ~response, radius=5, gradient = 'Reds', blur = 10,
                 group = 'Response Ele') %>%
      addHeatmap(data = filteredData2(), lng = ~x.pos, lat = ~y.pos, intensity = ~response, radius=5, gradient = 'Blues', blur = 10,
                 group = 'Response Roan') %>%
      
      #checkbox for response variables
      addLayersControl(
        overlayGroups =c("Response Ele","Response Roan"),
        options = layersControlOptions(collapsed=FALSE)
      )
  })
  
  #building the covariate map
  output$covariate_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      
      #heatmaps for the covariates
      addHeatmap(data=longlats, lng = ~x.pos, lat = ~y.pos, intensity = ~mindistToWaters/1000, radius = 6, gradient = 'Greys',
                 group = 'water') %>%
      addHeatmap(data=longlats, lng = ~x.pos, lat = ~y.pos, intensity = ~mindistToFence/1000, radius = 6, gradient = 'Greens',
                 group = 'fence') %>%
      addHeatmap(data=covardat, lng = ~coords.x1, lat = ~coords.x2, intensity = ~altitude,radius = 5 ,gradient = 'PuRd',
                 group = 'altitude') %>%
      #checkbox for covariates
      addLayersControl(
        overlayGroups =c("water","fence","altitude"),
        options = layersControlOptions(collapsed=FALSE)
      )
  })

  
    #the reactive part of the map: the response datapoints for elephants and covariates so we don't rerender the whole map everytime
    observe({
      leafletProxy("reponse_map") %>%
      clearHeatmap() %>%
      addHeatmap(data=filteredData(), lng = ~x.pos, lat = ~y.pos, intensity = ~response, radius=5, gradient = 'Reds', blur = 10,
              group = 'Response Ele') %>%
      addHeatmap(data=filteredData2(), lng = ~x.pos, lat = ~y.pos, intensity = ~response, radius=5, gradient = 'Blues', blur = 10,
                   group = 'Response Roan')
})
    
}

#observe({
#  leafletProxy("mymap") %>%
#    addLayersControl(
#      overlayGroups =c("Response","water","fence","altitude"),
#      options = layersControlOptions(collapsed=FALSE)
#    )
#})


shinyApp(ui, server)

