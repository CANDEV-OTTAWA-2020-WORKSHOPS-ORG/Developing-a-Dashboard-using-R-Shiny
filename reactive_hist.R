
library(cansim); library(tidyverse)


dairy <- get_cansim_ndm(32100114)


dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>% 
  group_by(year, Commodity, GEO) %>% 
  summarise(value = sum(VALUE))


library(shiny)


ui <- fluidPage(
  
  
 #added 2nd level of input, user can choose commodity
  sliderInput('binwidth', 'Bins', 5,50,5),
  selectInput('commodity','Milk',choices = unique(dairy$Commodity), selected = 'Buttermilk'),
 
  plotOutput('hist')
)


server <- function(input, output, session) {
 
# create reactive data set based on input called commodity

  dairy_reactive <- reactive({
  dairy %>% filter(Commodity == input$commodity)
})   

  
  output$hist = renderPlot({

    # the background r script (the renderPlot function) now calls the reactive data
    
    hist(dairy_reactive()$value,breaks=input$binwidth)
    
  })
}

shinyApp(ui, server)
