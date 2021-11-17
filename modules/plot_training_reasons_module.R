plot_training_reasons_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    plotlyOutput(ns("training_reasons_graph"))
  )
}

plot_training_reasons <- function(input, output, session, data) {
  
  output$training_reasons_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    
    dataTrainingReason <- data() %>%
      distinct(profileID, trainingReason)
    
    fig <- plot_ly(y = ~dataTrainingReason$trainingReason,
                   type = "histogram")
    fig <- fig %>% layout(
      title = "Training Reasons",
      yaxis = list(title = ''))
    return(fig)
  })
}