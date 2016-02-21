library(shiny)
library(datasets)

shinyServer(function(input, output) {
  output$WorldPhonesPlot <- renderPlot({
    barplot(WorldPhones[,input$region], main=input$region, ylab="Number of Telephones", xlab="Year",col="blue")
    if (input$radio == 2){
      barplot(WorldPhones[input$year,], main=input$region, ylab="Number of Telephones", xlab="Region",col="blue")
    }
  })
})

