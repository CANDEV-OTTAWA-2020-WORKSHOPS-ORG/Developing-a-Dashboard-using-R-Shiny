# load librairies
library(cansim); library(tidyverse)


# retrieve full StatCan data set by table number
dairy <- get_cansim_ndm(32100114)


# manipulate data to have monthly data summed by year
dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))


# here we tell R to filter the dairy table to include only the commodity Buttermilk
dairy_buttermilk <- dairy %>% filter(Commodity == 'Buttermilk')




# load shiny app
library(shiny)


# create user interface
ui <- fluidPage(
  
  sliderInput('binwidth', 'Bins', 5,50,5),
  plotOutput('hist')
)


#create output
server <- function(input, output, session) {
  
  output$hist = renderPlot({
    
    hist(dairy_buttermilk$value,breaks=input$binwidth,
         main = paste('Histogram of buttermilk - ',input$binwidth,'breaks'),
         xlab = '')
    
  })
}


shinyApp(ui, server)
