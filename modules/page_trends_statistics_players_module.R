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
            hr(style="border-color: black"),
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
              htmlOutput(ns("outputHeadlineDemographics")),
              br(),
              fluidRow(
                column(4,
                       class="container-demographics",
                       div(
                         plot_age_median_UI(ns("plot_age_median"))
                       )
                ),
                column(4,
                       class="container-demographics",
                       div(
                         id='div-tableOutput-playContext',
                         h5("Play Context", class="title-div"),
                         tableOutput(ns("playContext"))
                       )
                ),
                column(4,
                       class="container-demographics",
                       div(
                         plot_training_reasons_UI(ns("plot_training_reasons"))
                       )
                )
              )
            ),
            div(
              h1("Training Programs"),
              hr(),
              htmlOutput(ns("outputHeadlineTrainingPrograms")),
              br(),
              div(
                id='div-tableOutput-gameType',
                h5("Game Type", class="title-div"),
                tableOutput(ns("gameType"))
              ),
              fluidRow(
                column(4,
                       class="container-trainingPrograms",
                       div(
                         plot_circle_amount_UI(ns("plot_circle_amount"))
                       )
                ),
                column(4,
                       class="container-trainingPrograms",
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
  
  filterStatisticsData <- reactiveValues(df = NULL,
                                         ageMedian = NULL,
                                         trainingReason = NULL,
                                         circleAmount = NULL,
                                         gameType = NULL,
                                         sessionLength = NULL)
  
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
  
  output$outputHeadlineDemographics <- renderText({
    validate(need(D(), "No data."))
    toReturn <- paste0("The age median for this group is <b>", 
                      filterStatisticsData$ageMedian$df, 
                      "</b> years old (n=<b>",
                      filterStatisticsData$df %>% distinct(profileID) %>% count(),
                      "</b>), playing to train <b>",
                      filterStatisticsData$trainingReason$df,
                      "</b>.")
    return(toReturn)
  })
  
  filterStatisticsData$ageMedian <- callModule(plot_age_median, "plot_age_median", reactive(filterStatisticsData$df))
  
  output$playContext <- renderTable({
    validate(need(filterStatisticsData$df, "No data."))
    
    listPlayContext <- filterStatisticsData$df %>%
      distinct(profileID, playContext) %>%
      group_by(playContext) %>%
      count() %>%
      arrange(desc(n))
    return(listPlayContext)
  }, colnames = FALSE, striped = TRUE, spacing = "l", bordered = TRUE, align = "c")
  
  filterStatisticsData$trainingReason <- callModule(plot_training_reasons, "plot_training_reasons", reactive(filterStatisticsData$df))
  
  output$outputHeadlineTrainingPrograms <- renderText({
    validate(need(D(), "No data."))
    toReturn <- paste0("Most players in the group is playing with <b>",
                       filterStatisticsData$circleAmount$df,
                       " circles</b> for <b>",
                       filterStatisticsData$sessionLength$df,
                       " minutes</b> in game type <b>",
                       filterStatisticsData$gameType,
                       "</b>.")
    return(toReturn)
  })
  
  filterStatisticsData$circleAmount <- callModule(plot_circle_amount, "plot_circle_amount", reactive(filterStatisticsData$df))
  
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
    
    popularGameType <- listGameType %>%
      ungroup() %>%
      filter(n == max(n))
    filterStatisticsData$gameType <- popularGameType$gameType
    
    return(listGameType)
  }, colnames = FALSE, striped = TRUE)
  
  filterStatisticsData$sessionLength <- callModule(plot_session_length, "plot_session_length", reactive(filterStatisticsData$df))
  
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
