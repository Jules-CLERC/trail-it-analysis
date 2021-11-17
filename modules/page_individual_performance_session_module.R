page_individual_performance_session_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            div(
              h1("Performance from the last session"),
              hr(),
              p("This page summarizes the player's performance from the last played session."),
              br(),
              fluidRow(
                column(6,
                       uiOutput(ns("last_session_performance_left"))
                ),
                column(6,
                       h4("Reaction time:"),
                       uiOutput(ns("last_session_performance_right"))
                )
              )
            ),
            div(
              h1("Spatial Performance"),
              hr(),
              p("The reaction time performance is shown based on where the person touched"),
              br(),
              uiOutput(ns("spatial_performance_ipad"))
            ),
            div(
              h1("Performance per level"),
              hr(),
              p("The reaction time performance based on which session the person was playing"),
              br(),
              plot_performance_per_level_UI(ns("plot_performance_per_level"))
            )
  )
}

#server function
page_individual_performance_session <- function(input, output, session, currentPlayer, D) {
  
  currentSession <- reactiveValues(df = NULL)
  
  observeEvent(currentPlayer(), {
    currentSession$df <- D() %>% 
      filter(profileID == currentPlayer()) %>%
      filter(sessionID == max(sessionID))
  })
  
  output$last_session_performance_left <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    toReturn <- div(
      p(paste("Game Type:",
              as.character(currentSession$df[1, "gameType"]))),
      p(paste("Circle Amount:", 
              as.character(currentSession$df[1, "circleAmount"]))),
      p(paste("Session length:", 
              as.character(currentSession$df[1, "sessionLength"]), "min")),
      p(paste("No. of Levels played:", 
              as.character(nrow(currentSession$df))))
    )
    return(toReturn)
  })
  
  output$last_session_performance_right <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    toReturn <- div(
      p(paste("Best:", 
              as.character(round(currentSession$df[1, "sessionBestReactionTime"], 2)), "s")),
      p(paste("Median:", 
              as.character(round(currentSession$df[1, "sessionMedianReactionTime"], 2)), "s")),
      p(paste("Worst:", 
              as.character(round(currentSession$df[1, "sessionWorstReactionTime"], 2)), "s"))
    )
    return(toReturn)
  })
  
  #TODO : change the settings for the heat map colors
  output$spatial_performance_ipad <- renderUI({
    validate(need(currentPlayer(), "No current player."))
    
    svg <- read.delim("www/ipad.svg")
    svg <- paste(unlist(svg), collapse='')
    
    listReactionTime <- currentSession$df %>%
      select(
        sessionReactionTime_0_1,
        sessionReactionTime_1_1,
        sessionReactionTime_2_1,
        sessionReactionTime_0_0,
        sessionReactionTime_1_0,
        sessionReactionTime_2_0) %>%
      distinct()
    listColors = c('#a4f9cd', '#f9d6a4', '#6955dc', '#a4f1f9', '#acf9a4', '#f9a4a4')
    for (i in 1:6) {
      tmpReactionTime = listReactionTime[[1,i]]
      #change reaction time
      svg <- str_replace(svg, paste0('\\{',i,'\\}'), 
                         paste(round(tmpReactionTime,2), "s"))
      
      #change color
      tmpColor = 'rgb(197, 240, 202)'      #Green
      if(tmpReactionTime > 1) {
        tmpColor = 'rgb(251, 129, 133)'      #Red
      }
      else if (tmpReactionTime > 0.5) {
        tmpColor = 'rgb(243, 200, 180)'     #Orange
      }
      svg <- str_replace(svg, listColors[i], tmpColor)
    }

    toReturn <- HTML(
      svg
    )
    return(toReturn)
  })
  
  callModule(plot_performance_per_level, "plot_performance_per_level", reactive(currentSession$df))
  
}
