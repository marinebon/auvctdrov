library(tidyverse)
library(shiny)

url = 'http://stoqs.mbari.org:8000/stoqs_canon_april2017/api/measuredparameter.csv?measurement__instantpoint__activity__platform__name=ahi'

d = read_csv(url)
params = unique(d$parameter__name)

ui <- fluidPage(
  
  titlePanel("CTD Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('param', 'Parameter', params)),
    
      mainPanel(
         plotOutput('profile_plot')
      )
   )
)

server <- function(input, output) {
  
  output$profile_plot <- renderPlot({
    # input = list(param='chlorophyll')
    x = d %>%
      filter(parameter__name == input$param) %>%
      mutate(
        depth_bin = cut(
          measurement__depth, 
          breaks = seq(0,30,by=5))) %>%
      group_by(parameter__name, depth_bin) %>%
      summarise(
        avg_val = mean(datavalue, na.rm=T))
      
    plot(x$depth_bin, x$avg_val)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

