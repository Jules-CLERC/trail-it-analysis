plot_circle_amount_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    plotlyOutput(ns("circle_amount_graph"))
  )
}

plot_circle_amount <- function(input, output, session, data) {
  
  output$circle_amount_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    
    dataCircleAmount <- data() %>%
      distinct(profileID, circleAmount) %>%
      mutate(circleAmount = paste(circleAmount))
    
    fig <- plot_ly(x = ~dataCircleAmount$circleAmount,
                   type = "histogram")
    fig <- fig %>% layout(
      title = "Circle Amount",
      xaxis = list(title = ''))
    return(fig)
  })
}