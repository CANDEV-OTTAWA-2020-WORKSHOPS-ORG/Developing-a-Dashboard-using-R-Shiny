
library(cansim); library(tidyverse); library(plotly); library(magrittr)
library(leaflet); library(rgdal); library(spdplyr); library(rgeos); library(geosphere)

# read dairy set
dairy <- get_cansim_ndm(32100114)

# read map
prov_map = readOGR('Energy/Canada/Canada.shp',stringsAsFactors = FALSE)

# transform coordinates from WGS84 to longitude-latitude
prov_map <- spTransform(prov_map, CRS("+proj=longlat +datum=WGS84"))

# edit dairy set
dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(Commodity, GEO) %>%
  summarise(value = sum(VALUE,na.rm=TRUE))

# join dairy set to the provincial map 
# (this particular way to add data to the map is possible because of the spdplyr package)
prov_map %<>% left_join(dairy,by=c('NAME'='GEO'))

library(shiny)

ui <- fluidPage(
  
  # create sidebar panel
  sidebarPanel(
    
    # select commodity
    selectInput("map_commodity", label = 'Select Commodity',
                   choices = unique(prov_map$Commodity),
                   selected = 'Buttermilk'),
    width=2
    ),
  
  # create main panel
  mainPanel(
    
    # create map output
    leafletOutput("plot",height=600), 
    width=10
  )
  
)

server <- function(input, output, session) {
  
  # create map reactive function
  map_reactive <- reactive({

    # filter map my commodity
    prov_map@data %<>% filter(Commodity==input$map_commodity)
    
    prov_map
    
  })
  
  # create and update map
  output$plot <- renderLeaflet({
    
    
  leaflet() %>%
    # sets initial view of mpa (zoom level and map center)
    setView(lng = -98.4, lat = 58.2, zoom = 4) %>%
    # add basemap
    addProviderTiles(providers$Esri.WorldImagery) %>%
    # remove legend and colors (part of the reactive)
    clearShapes() %>% clearControls() %>%
    # add colors
    addPolygons(
      # adds province shapes
      data = map_reactive(),
      # adds colors to provinces based on selected commodity
      fillColor = ~colorBin(c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), value, 5)(value),
      # choose opacity of polygon fills
      fillOpacity = 0.7,
      #customize map edges
      color = "#BDBDC3", weight = 4) %>%
    # add legend
    addLegend(
      # choose legend position
      "bottomleft", 
      # write legend title
      title = 'Kilolitres',
      # create legend color palette (this corresponds with fill colors of polygons)
      pal = colorBin(
        palette = c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), 
        domain = map_reactive()$value, 5), 
      # create legend values (these correspond with the fill colors of polygons)
      values = map_reactive()$value
        )
    
  })
  
}

shinyApp(ui, server)


