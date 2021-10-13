plot_time_player_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(
      tags$div(class='contextual-toolbar',
               selectInput(
                 ns("select_x_time"),
                 "X Value :",
                 c("Month", "Year")
               )
      )
    ),
    fluidRow(
      plotlyOutput(ns("time_player_graph"))
    )
  )
}

plot_time_player <- function(input, output, session, currentPlayer, D) {
  v <- reactiveValues(data = NULL, trigger = 0)
  
  observeEvent(currentPlayer(), {
    v$trigger <- v$trigger + 1
  })
  
  observeEvent(input$select_x_time,{
    v$trigger <- v$trigger + 1
  })
  
  observeEvent(v$trigger,{
    validate(need(D(), "Waiting for data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."))
    
    if(input$select_x_time == "Month") {
      v$data <- D() %>%
        filter(profileID == currentPlayer()) %>%
        distinct(Timestamp, levelTimeTotal) %>%
        mutate(x_time = format(as.Date(Timestamp), "%Y-%m")) %>%
        group_by(x_time) %>%
        summarise(nbMinutes = sum(levelTimeTotal) / 60)
    }
    else if(input$select_x_time == "Year") {
      v$data <- D() %>%
        filter(profileID == currentPlayer()) %>%
        distinct(Timestamp, levelTimeTotal) %>%
        mutate(x_time = format(as.Date(Timestamp), "%Y")) %>%
        group_by(x_time) %>%
        summarise(nbMinutes = sum(levelTimeTotal) / 60)
    }
  })
  
  output$time_player_graph<- renderPlotly({
    validate(need(D(), "Waiting for data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."))
    if (is.null(v$data)) return()
    plot_ly() %>%
       add_trace(data = v$data, x=~x_time, y=~nbMinutes, type = 'bar') %>%
       layout(xaxis = list(title = input$select_x_time))
  })
  
}