page_trends_statistics_players_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            checkboxInput(ns("input_strokes_players"), "Only Strokes players", value = FALSE),
            textOutput(ns("number_of_players")),
            hr(),
            plot_game_type_pie_UI(ns("plot_game_type_pie")),
            hr(),
            plot_circle_amount_pie_UI(ns("plot_circle_amount_pie")),
            hr(),
            plot_session_length_pie_UI(ns("plot_session_length_pie"))
  )
}

#server function
page_trends_statistics_players <- function(input, output, session, D) {
  
  comparePlayers = reactiveValues(df = NULL)
  
  toListen <- reactive({
    list(D(), input$input_strokes_players)
  })
  
  observeEvent(toListen(), {
    validate(need(D(), "No data."))
    if(input$input_strokes_players == TRUE) {
      comparePlayers$df <- D() %>%
        filter(!(trainingReason == "OtherReason"))
    }
    else {
      comparePlayers$df <- D()
    }
  })
  
  output$number_of_players <- renderText({
    validate(need(comparePlayers$df, "No data."))
    nbPlayers <- comparePlayers$df %>%
      distinct(profileID) %>%
      count()
    return(paste("Number of players selected :", nbPlayers[1,1]))
  })
  
  callModule(plot_game_type_pie, "plot_game_type_pie", reactive(comparePlayers$df))
  callModule(plot_circle_amount_pie, "plot_circle_amount_pie", reactive(comparePlayers$df))
  callModule(plot_session_length_pie, "plot_session_length_pie", reactive(comparePlayers$df))
}
