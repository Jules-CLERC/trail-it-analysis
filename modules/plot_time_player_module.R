plot_time_player_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(
      tags$div(class='contextual-toolbar',
               selectInput(
                 ns("select_x_time"),
                 "X Value :",
                 c("Month", "Year")
               ),
               selectInput(
                 ns("select_y"),
                 "Y Value :",
                 c("Minutes", "Number_of_games")
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
  
  observeEvent(input$select_y,{
    v$trigger <- v$trigger + 1
  })
  
  observeEvent(v$trigger,{
    validate(need(D(), "Waiting for data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."))
    
    v$data = D() %>%
      filter(profileID == currentPlayer()) %>%
      filter(eventLabel == "Level 1 Completed!") %>%
      distinct(Timestamp, sessionLength)
    
    if(input$select_x_time == "Month") {
      v$data <- v$data %>%
        mutate(x_time = format(as.Date(Timestamp), "%Y-%m"))
        
    }
    else if(input$select_x_time == "Year") {
      v$data <- v$data %>%
        mutate(x_time = format(as.Date(Timestamp), "%Y"))
    }
    
    if(input$select_y == "Minutes") {
      v$data <- v$data %>%
        group_by(x_time) %>%
        summarise(y_time = sum(sessionLength))
    }
    else if(input$select_y == "Number_of_games") {
      v$data <- v$data %>%
        group_by(x_time) %>%
        count()
      names(v$data)[2] <- 'y_time'
    }
    
  })
  
  output$time_player_graph<- renderPlotly({
    validate(need(D(), "Waiting for data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."))
    if (is.null(v$data)) return()
    plot_ly() %>%
       add_trace(data = v$data, x=~x_time, y=~y_time, type = 'bar') %>%
       layout(xaxis = list(title = input$select_x_time),
              yaxis = list(title = input$select_y))
  })
  
}