library(cansim); library(tidyverse)


dairy <- get_cansim_ndm(32100114)




dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))


# instead of specifying which commodity to filter for in the R script, we can
# allow the user to choose the commodity by including it in the shiny reactive environment

library(shiny)



ui <- fluidPage(
  
  
  sliderInput('binwidth', 'Bins', 5,50,5),
  
  #the selectInput is the input object the user will use to select the commodity
  selectInput('commodity','Milk',choices = unique(dairy$Commodity), selected = 'Buttermilk'),
  
  plotOutput('hist')
)




server <- function(input, output, session) {
  
  # create reactive data set based on input object (selectInput) called commodity
  
  
  dairy_reactive <- reactive({
    dairy %>% filter(Commodity == input$commodity)
  })  
  
  
  
  output$hist = renderPlot({
    
    
    # the background r script (the renderPlot function) now calls the reactive data
    # note the open close bracket, which tells the function called to use reactive data
    
    hist(dairy_reactive()$value,breaks=input$binwidth,
         main = paste('Histogram of',input$commodity,' - ',input$binwidth,'breaks'),
         xlab = '')
    
  })
}


shinyApp(ui, server)