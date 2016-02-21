library(shiny)
library(datasets)

shinyUI(
  fluidPage(    
    titlePanel("Number of Telephones by Region and Year"),
    sidebarLayout(      
      sidebarPanel(
        radioButtons("radio", "Variable to Explore:", choices = list("Region" = 1, "Year" = 2), selected = 1),
        selectInput("region", "Region:", choices=colnames(WorldPhones)),
        selectInput("year", "Year:", choices=rownames(WorldPhones)),
        hr(),
        helpText("Data from AT&T (1961) The World's Telephones.")
      ),
      mainPanel(plotOutput("WorldPhonesPlot"))
    )
  )
)


