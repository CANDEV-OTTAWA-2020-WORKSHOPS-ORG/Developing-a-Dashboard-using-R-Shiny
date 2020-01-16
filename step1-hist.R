# load librairies
library(cansim); library(tidyverse)


# retrieve full StatCan data set by table number
dairy <- get_cansim_ndm(32100114)


# sum monthly data by year
dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))


# filter the dairy table to include only the commodity Buttermilk
dairy_buttermilk <- dairy %>% filter(Commodity == 'Buttermilk')


# load shiny app
library(shiny)


# create user interface
ui <- fluidPage(
  
  #create user input object to select the number of bins for histogram
  sliderInput('binwidth', 'Bins', 5,50,5),
  
  #create space in webpage for output object
  plotOutput('hist')
)


#tell server (in this case our computer) which R code to call as the users change inputs on the UI
#the code below is not reactive, therefore any input selected by the user will not result in any
#changes in the output object
server <- function(input, output, session) {
  
  output$hist = renderPlot({
    
    #the render function tells the output object (output$hist) which R code to run
    #in this case there is nothing linked to the input object (sliderInput), it tells the app to run with breaks = 10
    #this is not a reactive app
    hist(dairy_buttermilk$value,breaks=10,
         main = paste('Histogram of buttermilk'),
         xlab = '')
    
  })
}


shinyApp(ui, server)

shinyApp(ui, server)
