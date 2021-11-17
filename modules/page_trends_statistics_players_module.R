page_trends_statistics_players_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            fluidRow(
              column(2,
                     div(
                       class="trends-statistics-header-div",
                       h1(textOutput(ns("outputNbPlayers"))),
                       p("PLAYERS")
                     )
              ),
              column(2,
                     div(
                       class="trends-statistics-header-div",
                       h1(textOutput(ns("outputNbSessionsPlayed"))),
                       p("SESSIONS PLAYED")
                     )
              ),
              column(2,
                     div(
                       class="trends-statistics-header-div",
                       h1(textOutput(ns("outputNbStrokePatients"))),
                       p("STROKE PATIENTS")
                     )
              )
            ),
            br(),
            fluidRow(
              tags$div(class='contextual-toolbar', 
                       p("Filter by:"),
                       checkboxGroupInput(ns("optionFilter"), label = "",
                                          choices = c(
                                            "Patient" = "selectPatients", 
                                            "Outpatients" = "selectOutPatients",
                                            "Other Players" = "selectOtherPlayers", 
                                            "Reference Players" = "selectReferencePlayers"),
                                          selected = c("selectPatients"), inline = TRUE)
              )
            ),
            div(
              h1("Demographics"),
              hr(),
              fluidRow(
                column(4,
                       div(
                         plot_age_median_UI(ns("plot_age_median"))
                       )
                ),
                column(4,
                       div(
                         class='div-tableOutput',
                         h5("Play Context", class="title-div"),
                         tableOutput(ns("playContext"))
                       )
                ),
                column(4,
                       div(
                         plot_training_reasons_UI(ns("plot_training_reasons"))
                       )
                )
              )
            ),
            div(
              h1("Training Programs"),
              hr(),
              fluidRow(
                column(4,
                       div(
                         plot_circle_amount_UI(ns("plot_circle_amount"))
                       )
                ),
                column(4,
                       div(
                         class='div-tableOutput',
                         h5("Game Type", class="title-div"),
                         tableOutput(ns("gameType"))
                       )
                ),
                column(4,
                       div(
                         plot_session_length_UI(ns("plot_session_length"))
                       )
                )
              )
            )
  )
}

#server function
page_trends_statistics_players <- function(input, output, session, D) {
  
  filterStatisticsData <- reactiveValues(df = NULL)
  trainingProgramsData <- reactiveValues(df = NULL)
  
  output$outputNbPlayers <- renderText({
    validate(need(D(), "No data."))
    
    nbPlayers <- D() %>%
      distinct(profileID) %>%
      count()
    return(nbPlayers[[1]])
  })
  
  output$outputNbSessionsPlayed <- renderText({
    validate(need(D(), "No data."))
    
    nbSessionsPlayed <- D() %>%
      distinct(profileID, sessionID) %>%
      count()
    return(nbSessionsPlayed[[1]])
  })
  
  output$outputNbStrokePatients <- renderText({
    validate(need(D(), "No data."))
    
    nbStrokePatients <- D() %>%
      filter(trainingReason != "OtherReason") %>%
      distinct(profileID) %>%
      count()
    return(nbStrokePatients[[1]])
  })
  
  callModule(plot_age_median, "plot_age_median", reactive(filterStatisticsData$df))
  
  output$playContext <- renderTable({
    validate(need(filterStatisticsData$df, "No data."))
    
    listPlayContext <- filterStatisticsData$df %>%
      distinct(profileID, playContext) %>%
      group_by(playContext) %>%
      count() %>%
      arrange(desc(n))
    return(listPlayContext)
  }, colnames = FALSE, striped = TRUE)
  
  callModule(plot_training_reasons, "plot_training_reasons", reactive(filterStatisticsData$df))
  
  callModule(plot_circle_amount, "plot_circle_amount", reactive(filterStatisticsData$df))
  
  output$gameType <- renderTable({
    validate(need(filterStatisticsData$df, "No data."))
    
    listGameType <- filterStatisticsData$df %>%
      distinct(profileID, gameType) %>%
      mutate(gameType = ifelse(
        gameType == "gameA",
        "1-2-3-4",
        ifelse(
          gameType == "gameB",
          "1-A-2-B",
          "Unknown"
        )
      )) %>%
      group_by(gameType) %>%
      count() %>%
      arrange(desc(n))
    return(listGameType)
  }, colnames = FALSE, striped = TRUE)
  
  callModule(plot_session_length, "plot_session_length", reactive(filterStatisticsData$df))
  
  toListen <- reactive({
    list(D(), input$optionFilter)
  })
  observeEvent(toListen(), {
    validate(need(D(), "No data."))
    
    #Create list that contains all profileID Select
    listProfileIdSelect <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(listProfileIdSelect) <- c("profileID")
    if(!is.na(match("selectPatients", input$optionFilter))) {
      #select profileId of the patients
      tmpListProfileIdSelect <- D() %>%
        filter(trainingReason != "OtherReason") %>%
        filter(!is.na(match(playContext, c("Rehab","Unknown")))) %>%
        distinct(profileID)
      listProfileIdSelect <- merge(listProfileIdSelect, tmpListProfileIdSelect, by = "profileID", all = TRUE)
    }
    if(!is.na(match("selectOutPatients", input$optionFilter))) {
      #select profileId of the out patients
      tmpListProfileIdSelect <- D() %>%
        filter(trainingReason != "OtherReason") %>%
        filter(!is.na(match(playContext, c("Home","RehabNowHome")))) %>%
        distinct(profileID)
      listProfileIdSelect <- merge(listProfileIdSelect, tmpListProfileIdSelect, by = "profileID", all = TRUE)
    }
    if(!is.na(match("selectOtherPlayers", input$optionFilter))) {
      #select profileId of the other players (no MPC)
      tmpListProfileIdSelect <- D() %>%
        filter(trainingReason == "OtherReason") %>%
        filter(!substring(playerName, 1, 4) == "MPC-") %>%
        distinct(profileID)
      listProfileIdSelect <- merge(listProfileIdSelect, tmpListProfileIdSelect, by = "profileID", all = TRUE)
    }
    if(!is.na(match("selectReferencePlayers", input$optionFilter))) {
      #select profileId of the MPC players
      tmpListProfileIdSelect <- D() %>%
        filter(trainingReason == "OtherReason") %>%
        filter(substring(playerName, 1, 4) == "MPC-") %>%
        distinct(profileID)
      listProfileIdSelect <- merge(listProfileIdSelect, tmpListProfileIdSelect, by = "profileID", all = TRUE)
    }
    if(nrow(listProfileIdSelect) > 0) {
      #Get datas for the listProfileIdSelect
      filterStatisticsData$df <- merge(D(), listProfileIdSelect)
    }
    else {
      filterStatisticsData$df <- NULL
    }
  })
}
