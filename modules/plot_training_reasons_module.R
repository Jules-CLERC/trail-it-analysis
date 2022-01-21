plot_training_reasons_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    plotlyOutput(ns("training_reasons_graph"))
  )
}

plot_training_reasons <- function(input, output, session, data) {
  
  toReturn <- reactiveValues(df = NULL)
  
  output$training_reasons_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    
    dataTrainingReason <- data() %>%
      distinct(profileID, trainingReason)
    
    popularTrainingReason <- dataTrainingReason %>%
      group_by(trainingReason) %>%
      count() %>%
      ungroup() %>%
      filter(n == max(n))
    toReturn$df <- popularTrainingReason$trainingReason
    
    fig <- plot_ly(y = ~dataTrainingReason$trainingReason,
                   type = "histogram", height = 250)
    fig <- fig %>% layout(
      yaxis = list(title = ''))
    return(fig)
  })
  
  return(toReturn)
}