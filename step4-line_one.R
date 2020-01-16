library(cansim); library(tidyverse)


dairy <- get_cansim_ndm(32100114)


dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))
dairy_national <- dairy %>% filter(GEO == "Canada")



library(shiny)



ui <- fluidPage(
  
  
  selectInput('commodity','Milk',choices = unique(dairy_national$Commodity), selected = 'Buttermilk'),
  
  plotOutput('line')
)




server <- function(input, output, session) {
  
  dairy_reactive <- reactive({
    dairy_national %>% filter(Commodity == input$commodity)
    
  })  
  
  
  
  output$line = renderPlot({
    
    
    # the only real difference in this app is that we are passing a new R function in the 
    # render function. Here we use plot instead of hist. You can use 100 lines of code in the render function
    # everything inside the squiggly brackets will run as one chunk of code every time the app is invalidated
    
    plot(dairy_reactive()$year,dairy_reactive()$value,type = 'l',
         main = paste('Line chart of',input$commodity))
    
  })
}


shinyApp(ui, server)