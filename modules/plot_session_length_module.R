plot_session_length_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    plotlyOutput(ns("session_length_graph"))
  )
}

plot_session_length <- function(input, output, session, data) {
  
  output$session_length_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    
    dataSessionLength <- data() %>%
      distinct(profileID, sessionLength) %>%
      count(sessionLength)
    
    fig <- plot_ly(data = dataSessionLength, x = ~sessionLength, y = ~n,
                   type = 'scatter', mode = 'lines')
    fig <- fig %>% layout(
      title = "Session Length",
      xaxis = list(title = ''),
      yaxis = list(title = ''))
    return(fig)
  })
}