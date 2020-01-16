library(cansim); library(tidyverse)


dairy <- get_cansim_ndm(32100114)


dairy %<>% mutate(year=str_sub(REF_DATE,1,4)) %>%
  group_by(year, Commodity, GEO) %>%
  summarise(value = sum(VALUE))
# in step 4 we filtered the geography so that only the Canada
# level values were included. Similarily to how we went from filtering the dairy commodity
# outside the app in step 2 to inside the reactive in step 3, we will put the function to filter
# the geography inside the reactive here


library(shiny)

ui <- fluidPage(
  
  selectInput('commodity','Milk',choices = unique(dairy$Commodity), selected = 'Buttermilk'),
  
  # first we need to specify an input object to all the app to know what geography to select
  selectInput('geo','Geo',choices = unique(dairy$GEO), selected = 'Canada'),
  
  plotOutput('line')
)




server <- function(input, output, session) {
  
  
  
  #in this reactive, we have 2 input objects called (input$commodity and input$geo)
  dairy_reactive <- reactive({
    dairy %>% filter(Commodity == input$commodity, GEO == input$geo)
  })  
  
  
  
  output$line = renderPlot({
    

    #the reactive now relies on 2 input values and the app will be invalidated anytime either of these are changed
    plot(dairy_reactive()$year,dairy_reactive()$value,type = 'l',
         main = paste('Line chart of',input$commodity,'in',input$geo))
    
  })
}


shinyApp(ui, server)