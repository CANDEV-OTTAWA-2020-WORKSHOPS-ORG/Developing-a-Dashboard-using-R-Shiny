

library(cansim); library(tidyverse)


dairy <- get_cansim_ndm(32100114)




dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))


# the process seen in step 2 is the main function of r shiny, and that it called reactivity
# here we add a second level of reactivity
# instead of specifying which commodity to filter for in the R script, we can
# allow the user to choose the commodity by including it in the shiny reactive environment

library(shiny)



ui <- fluidPage(
  
  
  sliderInput('binwidth', 'Bins', 5,50,5),
  
  # here we choose a different input widget (radioButtons) to allow the user to select the commodity
  radioButtons('commodity','Milk',choices = unique(dairy$Commodity), selected = 'Buttermilk'),
  
  plotOutput('hist')
)




server <- function(input, output, session) {
  
  # how do we tell our app to filter the commodity to build a new data frame every time
  # the function used to do is called reactive()
  # here we create a reactive data set based on input object (selectInput) called commodity
  
  
  dairy_reactive <- reactive({
    dairy %>% filter(Commodity == input$commodity)
  })  
  
  
  
  output$hist = renderPlot({
    
    
    # the background r script (the renderPlot function) now calls the reactive data
    # the reactive is technically a function so when you call the reactive you have created
    # you need to put open-close brackets after it to tell the render function that this is a 
    # reactive data set

    
    hist(dairy_reactive()$value,breaks=input$binwidth,
         main = paste('Histogram of',input$commodity,' - ',input$binwidth,'breaks'),
         xlab = '')
    
  })
}


shinyApp(ui, server)