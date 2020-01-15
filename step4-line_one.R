library(cansim); library(tidyverse)


dairy <- get_cansim_ndm(32100114)


dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))
dairy_national <- dairy %>% filter(GEO == "Canada")


# instead of specifying which commodity to filter for in the R script, we can
# allow the user to choose the commodity by including it in the shiny reactive environment

library(shiny)



ui <- fluidPage(
  
  
  #the selectInput is the input object the user will use to select the commodity
  selectInput('commodity','Milk',choices = unique(dairy_national$Commodity), selected = 'Buttermilk'),
  
  plotOutput('line')
)




server <- function(input, output, session) {
  
  # create reactive data set based on input object (selectInput) called commodity
  
  
  dairy_reactive <- reactive({
    dairy_national %>% filter(Commodity == input$commodity)
  })  
  
  
  
  output$line = renderPlot({
    
    
    # the background r script (the renderPlot function) now calls the reactive data
    # note the open close bracket, which tells the function called to use reactive data
    
    plot(dairy_reactive()$year,dairy_reactive()$value,type = 'l',
         main = paste('Line chart of',input$commodity))
    
  })
}


shinyApp(ui, server)