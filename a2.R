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
eles<-read.csv(paste('./elemydatuse.csv', sep=''))
covar_shape<-readOGR(dsn = paste('./Shapes/covariate shape files', sep=''), layer = 'covariates_grid_ref')
covardat<-covar_shape@data
covardat<-data.frame(covardat, coordinates(covar_shape))
xy <- data.frame(ID = 1:nrow(covardat), X = covardat$coords.x1, Y = covardat$coords.x2)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
res <- spTransform(xy, CRS("+init=epsg:32733"))
covardat$x.pos<- res@coords[,1]
covardat$y.pos<- res@coords[,2]
covardat$area<-0.25
mapdata <- eles
coordinates(mapdata) <- ~x.pos+y.pos #similar to SpatialPoints
proj4string(mapdata) <- CRS("+proj=utm +zone=33 +south") #assign projection and coordinate reference system
longlats <- spTransform(mapdata, CRS("+proj=longlat")) #transform
longlats <- as.data.frame(longlats)

ui <- fluidPage(
  leafletOutput("mymap"),
  leafletOutput("map2"),
  
  sliderTextInput(inputId = "slider1",
                  label = "Select Month and Year" ,
                  choices = unique(longlats$yearmonth),
                  animate = FALSE, grid = FALSE,
                  hide_min_max = FALSE, from_fixed = FALSE,
                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                  post = NULL, dragRange = TRUE)
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    longlats %>%
      filter(yearmonth == input$slider1)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addHeatmap(data = filteredData(), lng = ~x.pos, lat = ~y.pos, intensity = ~response, radius=5, gradient = 'Reds', blur = 10,
                 group = 'Response')
  })
  output$map2 <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addHeatmap(data=longlats, lng = ~x.pos, lat = ~y.pos, intensity = ~mindistToWaters/1000, radius = 6, gradient = 'Blues',
                 group = 'water') %>%
      addHeatmap(data=longlats, lng = ~x.pos, lat = ~y.pos, intensity = ~mindistToFence/1000, radius = 6, gradient = 'Greens',
                 group = 'fence') %>%
      addHeatmap(data=covardat, lng = ~coords.x1, lat = ~coords.x2, intensity = ~altitude,radius = 5 ,gradient = 'PuRd',
                 group = 'altitude') %>%
      addLayersControl(
        overlayGroups =c("water","fence","altitude"),
        options = layersControlOptions(collapsed=FALSE)
      )
  })
  
}

    observe({
      leafletProxy("mymap",data=filteredData()) %>%
      clearHeatmap() %>%
      addHeatmap(lng = ~x.pos, lat = ~y.pos, intensity = ~response, radius=5, gradient = 'Reds', blur = 10,
              group = 'Response')
})

#observe({
#  leafletProxy("mymap") %>%
#    addLayersControl(
#      overlayGroups =c("Response","water","fence","altitude"),
#      options = layersControlOptions(collapsed=FALSE)
#    )
#})


shinyApp(ui, server)

