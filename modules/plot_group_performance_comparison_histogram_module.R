plot_group_performance_comparison_histogram_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(
      tags$div(class='contextual-toolbar', 
               radioButtons(ns("optionFilter"), label = "",
                            choices = c("MIN PER DAY" = "optionMinPerDay", "TOTAL MIN" = "optionTotalMin"),
                            selected = c("optionMinPerDay"), inline = TRUE)
      )
    ),
    fluidRow(
      plotlyOutput(ns("group_performance_comparison_histogram_graph"))
    )
  )
}

plot_group_performance_comparison_histogram <- function(input, output, session, data, currentPlayer) {
  
  toReturn <- reactiveValues(
    patientsAvgMinutesTotal = NULL,
    nonPatientsAvgMinutesTotal = NULL
  )
  
  output$group_performance_comparison_histogram_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."), errorClass = "vis")
    
    dataPlayersBar <- data() %>%
      group_by(profileID, sessionID) %>%
      distinct(sessionLength, date, trainingReason, playerName) %>%
      group_by(profileID) %>%
      summarise(
        playerName = first(playerName),
        totalMinutes = sum(sessionLength),
        totalDays = n_distinct(date),
        color = ifelse(
          first(trainingReason) == "Unknown" || first(trainingReason) == "OtherReason",
          "rgb(14,14,14)",
          "rgb(63,117,180)"
        )
      ) %>% 
      arrange(desc(color))
    
    #calculate the avg minutes for patients and non patients
    dataAvgMinutes <- dataPlayersBar %>%
      group_by(color) %>%
      summarise(avgMinutes = mean(totalMinutes))
    toReturn$patientsAvgMinutesTotal <- (dataAvgMinutes %>% filter(color == "rgb(63,117,180)"))$avgMinutes
    toReturn$nonPatientsAvgMinutesTotal <- (dataAvgMinutes %>% filter(color == "rgb(14,14,14)"))$avgMinutes
    
    #Change color of the current player
    #Add minPerDay column
    dataPlayersBar <- dataPlayersBar %>%
      mutate(
        color = ifelse(
          profileID == currentPlayer(),
          "rgb(236,167,69)",
          color
        ),
        minPerDay = totalMinutes / totalDays
      )
    
    if(input$optionFilter == "optionMinPerDay") { 
      yValues <- dataPlayersBar$minPerDay
      hoverText <- paste0(dataPlayersBar$playerName, " (", round(dataPlayersBar$minPerDay), " min per day)")
      yLabel1 <- -1
      yLabel2 <- -4
    }
    else if(input$optionFilter == "optionTotalMin"){
      yValues <- dataPlayersBar$totalMinutes
      hoverText <- paste0(dataPlayersBar$playerName, " (", round(dataPlayersBar$totalMinutes), " min)")
      yLabel1 <- -5
      yLabel2 <- -15
    }
    
    fig <- plot_ly(
      x = dataPlayersBar$profileID,
      y = yValues,
      type = "bar",
      marker = list(color = dataPlayersBar$color),
      text = hoverText,
      hoverinfo = 'text'
    )
    
    fig <- fig %>% 
      layout(
        xaxis = list(title = "",
                     showticklabels = FALSE,
                     categoryorder = "array",
                     categoryarray = dataPlayersBar$profileID))
    
    
    #Label PATIENTS
    listPatients <- dataPlayersBar %>% filter(color != "rgb(14,14,14)")
    fig <- fig %>% add_annotations(x = listPatients[round(nrow(listPatients) / 2), "profileID"],
                                   y = yLabel2,
                                   text = "PATIENTS",
                                   xref = "x",
                                   yref = "y",
                                   showarrow = FALSE,
                                   ax = 50,
                                   ay = 0,
                                   bgcolor = "rgb(63,117,180)",
                                   borderpad = 3,
                                   font = list(color = 'white',
                                               family = 'Helvetica Neue',
                                               size = 14))

    #Label NON-PATIENTS
    listNonPatients <- dataPlayersBar %>% filter(color == "rgb(14,14,14)")
    fig <- fig %>% add_annotations(x = listNonPatients[round(nrow(listNonPatients) / 2), "profileID"],
                                   y = yLabel2,
                                   text = "NON-PATIENTS",
                                   xref = "x",
                                   yref = "y",
                                   showarrow = FALSE,
                                   ax = 0,
                                   ay = 0,
                                   bgcolor = "rgb(14,14,14)",
                                   borderpad = 3,
                                   font = list(color = 'white',
                                               family = 'Helvetica Neue',
                                               size = 14))

    #Label Current player
    dataCurrentPlayer <- dataPlayersBar %>% filter(profileID == currentPlayer())
    fig <- fig %>% add_annotations(x = dataCurrentPlayer$profileID,
                                   y = yLabel1,
                                   text = (data() %>% filter(profileID == currentPlayer()) %>% distinct(playerName))$playerName,
                                   xref = "x",
                                   yref = "y",
                                   showarrow = FALSE,
                                   ax = 0,
                                   ay = 0,
                                   bgcolor = "rgb(236,167,69)",
                                   borderpad = 3,
                                   font = list(color = 'white',
                                               family = 'Helvetica Neue',
                                               size = 14))
    
    return(fig)
  })
  
  return(toReturn)
}