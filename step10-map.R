
library(cansim); library(tidyverse); library(plotly); library(magrittr)
library(leaflet); library(rgdal); library(spdplyr); library(rgeos); library(geosphere)

# read dairy set
dairy <- get_cansim_ndm(32100114)

# read map
prov_map = readOGR('Energy/Canada/Canada.shp',stringsAsFactors = FALSE)
prov_map <- spTransform(prov_map, CRS("+proj=longlat +datum=WGS84"))

# edit dairy set
dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(Commodity, GEO) %>%
  summarise(value = sum(VALUE,na.rm=TRUE))

# join dairy set to prov map
prov_map %<>% left_join(dairy,by=c('NAME'='GEO'))

library(shiny)

ui <- fluidPage(
  
  sidebarPanel(
    selectInput("map_commodity", label = 'Select Commodity',
                   choices = unique(prov_map$Commodity),
                   selected = 'Buttermilk'),
    width=2),
  
  mainPanel(
    leafletOutput("plot",height=600), 
    width=10
  )
  
)

server <- function(input, output, session) {
  
  # create map
  map_reactive <- reactive({

    prov_map@data %<>% filter(Commodity==input$map_commodity)
    
    prov_map
    
  })
  
  output$plot <- renderLeaflet({
    
    value_range = map_reactive()$value
    
  leaflet(
    options = leafletOptions(minZoom = 3, maxZoom = 5,
                             attributionControl=FALSE)
    ) %>%
    setView(lng = -98.4, lat = 58.2, zoom = 4) %>%
    # add basemap
    addProviderTiles(providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 0.8)) %>%
    # remove legend and colors
    clearShapes() %>% clearControls() %>%
    # add colors
    addPolygons(
      data = map_reactive(),
      fillColor = ~colorBin(c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), value, 5)(value),
      color = "#BDBDC3",
      fillOpacity = 0.7,
      weight = 4) %>%
    # add legend
      addLegend(
        opacity = 0.7, title = 'Kilolitres',"bottomleft", 
        pal = colorBin(palette = c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), 
        domain = map_reactive()$value, 5), values = value_range, 
        labFormat = labelFormat(transform = function(value_range) sort(value_range, decreasing = FALSE))
        )
    
  })
  
}

shinyApp(ui, server)





