page_individual_performance_over_time_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            h1("Animation : Reaction time by section for each session"),
            p("Select game type"),
            selectInput(ns("select_game_type"), "Select game type", NULL, multiple = TRUE),
            p("Select circle amount"),
            selectInput(ns("select_circle_amount"), "Select circle amount", NULL, multiple = TRUE),
            p("Select session length"),
            selectInput(ns("select_session_length"), "Select session length", NULL, multiple = TRUE),
            plot_reaction_time_player_over_time_UI(ns("plot_reaction_time_player_over_time")),
            hr(),
            h1("Play frequency"),
            plot_time_player_UI(ns("plot_time_player")),
            h1("Regression"),
            plotlyOutput(ns("regression_player_graph"))
  )
}

#server function
page_individual_performance_over_time <- function(input, output, session, currentPlayer, D) {
  
  gameplaysPlayer <- reactiveValues(df = NULL)
  gameplaysPlayerSelected <- reactiveValues(df = NULL)
  
  observeEvent(currentPlayer(), {
    #Check if a current player is selected
    if(!is.null(currentPlayer())) {
      gameplaysPlayer$df <- D() %>%
        filter(profileID == currentPlayer()) %>%
        arrange(Timestamp) %>%
        mutate(
          sessionID = ifelse(
            levelNumber > lag(levelNumber, default = 0), 0, 1
          ),
          sessionID = cumsum(sessionID)
        )
    }
    typesGameplays <- gameplaysPlayer$df %>%
      distinct(gameType, circleAmount, sessionLength)

    updateSelectInput(session, "select_game_type", choices = typesGameplays[,"gameType"], selected = typesGameplays[,"gameType"])
    updateSelectInput(session, "select_circle_amount", choices = typesGameplays[,"circleAmount"], selected = typesGameplays[,"circleAmount"])
    updateSelectInput(session, "select_session_length", choices = typesGameplays[,"sessionLength"], selected = typesGameplays[,"sessionLength"])
  })
  
  toListen <- reactive({
    list(
      input$select_game_type,
      input$select_circle_amount,
      input$select_session_length
    )
  })
  
  observeEvent(toListen(), {
    if(!is.null(D()) && !is.null(currentPlayer())) {

      tmpGameplaysPlayerSelected <- gameplaysPlayer$df

      if(!is.null(input$select_game_type)) {
        tmpGameplaysPlayerSelected <- tmpGameplaysPlayerSelected %>%
          filter(!is.na(match(gameType, input$select_game_type)))
      }
      if(!is.null(input$select_circle_amount)) {
        tmpGameplaysPlayerSelected <- tmpGameplaysPlayerSelected %>%
          filter(!is.na(match(circleAmount, input$select_circle_amount)))
      }
      if(!is.null(input$select_session_length)) {
        tmpGameplaysPlayerSelected <- tmpGameplaysPlayerSelected %>%
          filter(!is.na(match(sessionLength, input$select_session_length)))
      }
      
      gameplaysPlayerSelected$df <- tmpGameplaysPlayerSelected
    }
  })
  
  callModule(plot_reaction_time_player_over_time, "plot_reaction_time_player_over_time", reactive(gameplaysPlayerSelected$df))
  callModule(plot_time_player, "plot_time_player", currentPlayer, D)
  
  output$regression_player_graph<- renderPlotly({
    validate(need(D(), "Waiting for data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."))
    
    GameplaysPlayer <- D() %>%
      filter(profileID == currentPlayer()) %>%
      filter(gameType == "gameA") %>%
      filter(circleAmount == 12) %>%
      filter(eventLabel == "Level 1 Completed!")
    
    y <- GameplaysPlayer$Timestamp
    X <- GameplaysPlayer$sessionMedianReactionTime
    
    lm_model <- linear_reg() %>% 
      set_engine('lm') %>% 
      set_mode('regression') %>%
      fit(X,Y) 
    
    plot_ly() %>%
      add_trace(data = GameplaysPlayer, x=~Timestamp, y=~sessionMedianReactionTime, type = 'scatter', mode = 'lines')
  })
}
