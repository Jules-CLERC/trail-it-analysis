plot_circle_amount_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    plotlyOutput(ns("circle_amount_graph"))
  )
}

plot_circle_amount <- function(input, output, session, data) {
  
  toReturn <- reactiveValues(df = NULL)
  
  output$circle_amount_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    
    dataCircleAmount <- data() %>%
      distinct(profileID, circleAmount) %>%
      mutate(circleAmount = paste(circleAmount))
    
    popularCircleAmount <- dataCircleAmount %>%
      group_by(circleAmount) %>%
      count() %>%
      ungroup() %>%
      filter(n == max(n))
    toReturn$df <- popularCircleAmount$circleAmount
    
    fig <- plot_ly(x = ~dataCircleAmount$circleAmount,
                   type = "histogram", height = 250)
    fig <- fig %>% layout(
      title = "Circle Amount",
      xaxis = list(title = ''))
    return(fig)
  })
  
  return(toReturn)
}