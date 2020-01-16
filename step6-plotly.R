library(cansim); library(tidyverse); library(plotly)

dairy <- get_cansim_ndm(32100114)


dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))



library(shiny)

ui <- fluidPage(
  
  selectInput('commodity','Milk',choices = unique(dairy$Commodity), selected = 'Buttermilk'),
  
  selectInput('geo','Geo',choices = unique(dairy$GEO), selected = 'Canada'),
  
  # instead of allowing space on the app for a R base plot (hist, plot, etc.), shiny allows the use visualisations from
  # other packages, in this case plotly
  plotlyOutput('line')
  
)




server <- function(input, output, session) {
  
  
  

  dairy_reactive <- reactive({
    dairy %>% filter(Commodity == input$commodity, GEO == input$geo) %>% ungroup()
  })  
  
  
  
  output$line = renderPlotly({
    
    
    plot_ly(dairy_reactive(),x = ~year) %>% 
      add_trace(y = ~value, type = 'scatter',mode = 'lines', line = list(color = 'rgb(22, 96, 167)'))
    
  })
}


shinyApp(ui, server)