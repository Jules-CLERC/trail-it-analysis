plot_group_performance_comparison_scatter_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(
      plotlyOutput(ns("group_performance_comparison_scatter_graph"))
    )
  )
}

plot_group_performance_comparison_scatter <- function(input, output, session, data, currentPlayer) {
  
  layers <- reactiveValues(
    layersDict = c(
      'layerHumanLimit'= 'y2',
      'layerReference' = 'y2',
      'layerPatients' = 'y2',
      'layerNonPatients' = 'y2',
      'layerCurrentPlayer' = 'y1'
    ),
    oldKey = ''
  )
  
  getOpacity <- function(key) {
    if(layers$layersDict[key] == 'y1') {
      return(1)
    }
    else if(layers$layersDict[key] == 'y2') {
      return(0.1)
    }
    else {
      print('unfound layer')
      return(0)
    }
  }
  
  toReturn <- reactiveValues(
    meanPatientsReactionTime = NULL,
    meanNonPatientsReactionTime = NULL,
    meanReference = NULL)
  
  output$group_performance_comparison_scatter_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."), errorClass = "vis")

    #Get the data of the current player
    currentPlayerData <- data() %>%
      filter(profileID == currentPlayer()) %>%
      group_by(sessionID) %>%
      summarise(
        sessionMedianReactionTime = first(sessionMedianReactionTime),
        date = first(date)
      ) %>%
      group_by(date) %>%
      summarise(
        sessionMedianReactionTime = mean(sessionMedianReactionTime)
      ) %>%
      mutate(
        day = round(as.numeric(difftime(date, min(date), units = c("days"))))
      )
    supsmuCurrentPlayer <- supsmu(currentPlayerData$day, currentPlayerData$sessionMedianReactionTime, bass= 10)

    #Get the data of the patients
    patientsData <- data() %>%
      filter(trainingReason != "OtherReason") %>%
      group_by(profileID, sessionID) %>%
      summarise(
        sessionMedianReactionTime = first(sessionMedianReactionTime),
        date = first(date),
        .groups = 'drop'
      ) %>%
      group_by(profileID) %>%
      mutate(
        day = round(as.numeric(difftime(date, min(date), units = c("days"))))
      )
    supsmuPatients <- supsmu(patientsData$day, patientsData$sessionMedianReactionTime, bass= 10)
    toReturn$meanPatientsReactionTime = mean(supsmuPatients$y)

    #Get the data of the non patients
    nonPatientsData <- data() %>%
      filter(trainingReason == "OtherReason") %>%
      group_by(profileID, sessionID) %>%
      summarise(
        sessionMedianReactionTime = first(sessionMedianReactionTime),
        date = first(date),
        .groups = 'drop'
      ) %>%
      group_by(profileID) %>%
      mutate(
        day = round(as.numeric(difftime(date, min(date), units = c("days"))))
      )
    supsmuNonPatients <- supsmu(nonPatientsData$day, nonPatientsData$sessionMedianReactionTime, bass= 10)
    toReturn$meanNonPatientsReactionTime = mean(supsmuNonPatients$y)

    #Get the data of the MPC players
    mpcData <- data() %>%
      filter(substring(playerName, 1, 4) == "MPC-") %>%
      distinct(profileID, sessionID, sessionMedianReactionTime) %>%
      summarise(
        mean = mean(sessionMedianReactionTime)
      )
    toReturn$meanReference <- mpcData$mean

    #Start config plot
    fig <- plot_ly(showlegend = FALSE, source = "layerTraces") %>%
      layout(
        yaxis = list(
          range=c(0,8),
          overlaying = "y2"
        ),
        yaxis2 = list(
          range=c(0,8)
        )
      )

    #Add events on click
    eventData <- event_data(event = "plotly_click", source = "layerTraces")
    if (!is.null(eventData$key) && layers$oldKey != eventData$key[[1]]) {
      keyData = eventData$key[[1]]
      if(layers$layersDict[[keyData]] == 'y2') {
        layers$layersDict[[keyData]] = 'y1'
      }
      else if(layers$layersDict[[keyData]] == 'y1') {
        layers$layersDict[[keyData]] = 'y2'
      }
      layers$oldKey = keyData
    }

    #x limit corresponds to the max x of the data (patients & non patients)
    maxNonPatients <- max(nonPatientsData$day)
    maxPatients <- max(patientsData$day)
    xMax <- max(maxNonPatients, maxPatients) + 100

    #Human limit figs
    humanLimit  = 0.25
    fig <- fig %>% add_segments(x = 0, xend = xMax, y = humanLimit, yend = humanLimit, color = I("red"),
                                key = 'layerHumanLimit', yaxis = layers$layersDict['layerHumanLimit'], opacity = getOpacity('layerHumanLimit'))
    fig <- fig %>% add_annotations(x = xMax,
                                   y = humanLimit,
                                   text = '250MS HUMAN LIMIT',
                                   xref = "x",
                                   yref = "y",
                                   showarrow = TRUE,
                                   arrowhead = 4,
                                   arrowsize = .5,
                                   ax = 90,
                                   ay = -10,
                                   bgcolor = "rgb(252, 0, 6)",
                                   opacity = getOpacity('layerHumanLimit'),
                                   borderpad = 3,
                                   font = list(color = 'white',
                                               family = 'Helvetica Neue',
                                               size = 14))

    #MPC reference figs
    fig <- fig %>% add_segments(x = 0, xend = xMax, y = mpcData$mean, yend = mpcData$mean, color = I("green"), key = 'layerReference', yaxis = layers$layersDict['layerReference'], opacity = getOpacity('layerReference'))
    fig <- fig %>% add_annotations(x = xMax,
                                   y = mpcData$mean,
                                   text = 'REFERENCE',
                                   xref = "x",
                                   yref = "y",
                                   showarrow = TRUE,
                                   arrowhead = 4,
                                   arrowsize = .5,
                                   ax = 50,
                                   ay = -40,
                                   bgcolor = "rgb(18, 131, 1)",
                                   opacity = getOpacity('layerReference'),
                                   borderpad = 3,
                                   font = list(color = 'white',
                                               family = 'Helvetica Neue',
                                               size = 14))

    #Patients figs
    fig <- fig %>% add_trace(x = supsmuPatients$x, y = supsmuPatients$y, type = 'scatter', mode = 'lines', line = list(shape = "spline", color = 'blue'), key = 'layerPatients', yaxis = layers$layersDict['layerPatients'], opacity = getOpacity('layerPatients'))
    fig <- fig %>% add_trace(x = patientsData$day, y = patientsData$sessionMedianReactionTime, type = 'scatter', mode = 'markers', opacity = getOpacity('layerPatients') / 3, marker = list(color = 'blue'), key = 'layerPatients', yaxis = layers$layersDict['layerPatients'])
    fig <- fig %>% add_annotations(x = tail(supsmuPatients$x, 1),
                                   y = tail(supsmuPatients$y, 1),
                                   text = 'PATIENTS',
                                   xref = "x",
                                   yref = "y",
                                   showarrow = TRUE,
                                   arrowhead = 4,
                                   arrowsize = .5,
                                   ax = 50,
                                   ay = -20,
                                   bgcolor = "rgb(0, 0, 255)",
                                   opacity = getOpacity('layerPatients'),
                                   borderpad = 3,
                                   font = list(color = 'white',
                                               family = 'Helvetica Neue',
                                               size = 14))

    #Non patients figs
    fig <- fig %>% add_trace(x = supsmuNonPatients$x, y = supsmuNonPatients$y, type = 'scatter', mode = 'lines', line = list(shape = "spline", color = 'black'), key = 'layerNonPatients', yaxis = layers$layersDict['layerNonPatients'], opacity = getOpacity('layerNonPatients'))
    fig <- fig %>% add_trace(x = nonPatientsData$day, y = nonPatientsData$sessionMedianReactionTime, type = 'scatter', mode = 'markers', opacity = getOpacity('layerNonPatients') / 3, marker = list(color = 'black'), key = 'layerNonPatients', yaxis = layers$layersDict['layerNonPatients'])
    fig <- fig %>% add_annotations(x = tail(supsmuNonPatients$x, 1),
                                   y = tail(supsmuNonPatients$y, 1),
                                   text = 'NON-PATIENTS',
                                   xref = "x",
                                   yref = "y",
                                   showarrow = TRUE,
                                   arrowhead = 4,
                                   arrowsize = .5,
                                   ax = 50,
                                   ay = -20,
                                   bgcolor = 'rgb(0, 0, 0)',
                                   opacity = getOpacity('layerNonPatients'),
                                   borderpad = 3,
                                   font = list(color = 'white',
                                               family = 'Helvetica Neue',
                                               size = 14))

    #Current player figs
    fig <- fig %>% add_trace(x = supsmuCurrentPlayer$x, y = supsmuCurrentPlayer$y, type = 'scatter', mode = 'lines', line = list(shape = "spline", color = 'orange'), key = 'layerCurrentPlayer', yaxis = layers$layersDict['layerCurrentPlayer'], opacity = getOpacity('layerCurrentPlayer'))
    fig <- fig %>% add_annotations(x = tail(supsmuCurrentPlayer$x, 1),
                                   y = tail(supsmuCurrentPlayer$y, 1),
                                   text = (data() %>% filter(profileID == currentPlayer()))[[1,"playerName"]],
                                   xref = "x",
                                   yref = "y",
                                   showarrow = TRUE,
                                   ax = 50,
                                   ay = -20,
                                   bgcolor = 'rgb(235, 167, 70)',
                                   opacity = getOpacity('layerCurrentPlayer'),
                                   borderpad = 3,
                                   font = list(color = 'white',
                                               family = 'Helvetica Neue',
                                               size = 14))
    
    return(fig)
  })
  
  return(toReturn)
}