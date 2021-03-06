library(cansim); library(tidyverse); library(plotly)

dairy <- get_cansim_ndm(32100114)


dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))


library(shiny)

ui <- fluidPage(
  
  navbarPage('',
             
             tabPanel('2 Line Chart',
                      
                      sidebarPanel(
                        
                        selectInput('commodity','Milk',choices = unique(dairy$Commodity), selected = 'Buttermilk'),
                        selectInput('commodity_2','Milk',choices = unique(dairy$Commodity), selected = 'Sour cream'),
                        selectInput('geo','Geo',choices = unique(dairy$GEO), selected = 'Canada')
                        
                        ),
                      
                      mainPanel(
                        
                        plotlyOutput('line'),
                        width=10
                        
                        )
                      
                      ),
             
             tabPanel('1 Line Chart',
                      
                      sidebarPanel(
                        
                        selectInput('commodity_3','Milk',choices = unique(dairy$Commodity), selected = 'Buttermilk'),
                        selectInput('geo_2','Geo',choices = unique(dairy$GEO), selected = 'Canada')
                        
                      ),
                      
                      mainPanel(
                        
                        plotlyOutput('line_2'),
                        width=10
                        
                      )
                      
             )
             )
  )




server <- function(input, output, session) {
  
  
  dairy_reactive <- reactive({
    
    input_commodities = c(input$commodity,input$commodity_2)
    
    dairy %>% filter(Commodity %in% input_commodities, GEO == input$geo) %>% ungroup()
    
  })  
  
  output$line = renderPlotly({
    
    plot_ly() %>% 
      add_trace(
        data = dairy_reactive() %>% filter(Commodity==input$commodity),
        x = ~year, y = ~value, 
        type = 'scatter',mode = 'lines', 
        line = list(color = 'rgb(22, 96, 167)'),
        name = input$commodity ) %>% 
      add_trace(
        data = dairy_reactive() %>% filter(Commodity==input$commodity_2),
        x = ~year, y = ~value, 
        type = 'scatter',mode = 'lines', 
        line = list(color = 'red'),
        name = input$commodity_2 )
    
  })
  
  dairy_reactive_2 <- reactive({
    
    dairy %>% filter(Commodity %in% input$commodity_3, GEO == input$geo_2) %>% ungroup()
    
  })  
  
  output$line_2 = renderPlotly({
    
    plot_ly() %>% 
      add_trace(
        data = dairy_reactive_2(),
        x = ~year, y = ~value, 
        type = 'scatter',mode = 'lines', 
        line = list(color = 'rgb(22, 96, 167)'),
        name = input$commodity_3 )
    
  })
}


shinyApp(ui, server)

