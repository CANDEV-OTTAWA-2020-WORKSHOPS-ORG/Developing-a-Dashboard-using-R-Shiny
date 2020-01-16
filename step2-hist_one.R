# load librairies
library(cansim); library(tidyverse)

dairy <- get_cansim_ndm(32100114)


dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))

dairy_buttermilk <- dairy %>% filter(Commodity == 'Buttermilk')



library(shiny)

ui <- fluidPage(
  
  sliderInput('binwidth', 'Bins', 5,50,5),
  plotOutput('hist')
)


server <- function(input, output, session) {
  
  output$hist = renderPlot({
    
    #unlike the previous app ran, this render function tells the output object (output$hist) 
    #to rerun its code looking up the value supplied by the input object (input$binwidth) every time the app
    #is invalidated. Instead of breaks = 10, breaks will be equal to the input object's value every time it is changed
    hist(dairy_buttermilk$value,breaks=input$binwidth,
         main = paste('Histogram of buttermilk - ',input$binwidth,'breaks'),
         xlab = '')
    
  })
}


shinyApp(ui, server)
