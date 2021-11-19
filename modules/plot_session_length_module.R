plot_session_length_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    plotlyOutput(ns("session_length_graph"))
  )
}

plot_session_length <- function(input, output, session, data) {
  
  toReturn <- reactiveValues(df = NULL)
  
  output$session_length_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    
    dataSessionLength <- data() %>%
      distinct(profileID, sessionLength) %>%
      count(sessionLength)
    
    popularSessionLength <- dataSessionLength %>%
      filter(n == max(n))
    toReturn$df <- popularSessionLength$sessionLength
    
    fig <- plot_ly(data = dataSessionLength, x = ~sessionLength, y = ~n,
                   type = 'scatter', mode = 'lines', height = 250)
    fig <- fig %>% layout(
      title = "Session Length",
      xaxis = list(title = ''),
      yaxis = list(title = ''))
    return(fig)
  })
  
  return(toReturn)
}