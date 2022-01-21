plot_timeline_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(
      plotlyOutput(ns("timeline_graph"))
    )
  )
}

plot_timeline <- function(input, output, session, data, currentPlayer) {
  output$timeline_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    validate(need(nrow(data()) > 0, "No data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player"), errorClass = "vis")
    
    #Get all datas important for the plot
    dataTimeline <- data() %>%
      distinct(profileID, sessionID, playerName, date, gameType, sessionLength) %>%
      group_by(profileID, date, playerName) %>%
      summarise(
        dateGameType = list(unique(gameType)),
        dateSessionLength = sum(sessionLength),
        .groups = 'drop'
      ) %>%
      ungroup() %>%
      mutate(
        dateGameType = ifelse(
          dateGameType == "gameA",
          "1-2-3-4",
          ifelse(
            dateGameType == "gameB",
            "1-A-2-B",
            "Both types"
          )
        )
      )
    
    #Get first and last dates for the segments
    #We also need to calc the y position that depends on the start date
    dataTimelineSegments <- dataTimeline %>%
      group_by(profileID) %>%
      summarise(
        start = min(date),
        end = max(date),
        .groups = 'drop') %>%
      arrange(desc(start)) %>%
      mutate(
        position = row_number(),
        color = ifelse(
          profileID == currentPlayer(),
          "orange",
          "#337ab7"
        )
      )
    
    #Transfer the position of the playerID in dataTimeline
    dataTimeline <- merge(dataTimeline, dataTimelineSegments %>% select(profileID, position, color), "profileID")
    
    fig <- plot_ly()
    
    for(i in 1:nrow(dataTimelineSegments)) {
      row <- dataTimelineSegments[i,]
      fig <- fig %>% add_segments(
        x = row$start,
        xend = row$end,
        y= row$position,
        yend= row$position,
        color = I(row$color),
        line = list(width= 4)) %>% hide_legend()
    }

    fig <- fig %>% add_markers(
      x = ~dataTimeline$date,
      y = ~dataTimeline$position,
      hoverinfo = 'text',
      text = ~paste0(dataTimeline$playerName, ", ", dataTimeline$date,
        "<br>", dataTimeline$dateGameType,
        "<br>", dataTimeline$dateSessionLength, " minutes"),
      alpha = 2, size = I(45),
      color = I("#ffffff"),
      stroke = ~I(dataTimeline$color),
      span = I(2)) %>% hide_legend()
    
    fig <- fig %>% layout(
      xaxis= list(
        title = '',
        showline = TRUE,
        showgrid = FALSE,
        type = "date",
        tickmode=  'linear',
        tickformat ="%Y",
        rangeslider = list(
          type = "date",
          bordercolor = "#000000",
          borderwidth = 1),
        dtick = 30 * 24 * 60 * 60 * 1000 * 12 #milliseconds
      ),
      yaxis= list(
        title = '',
        showticklabels=FALSE,
        zeroline = FALSE,
        showline = TRUE,
        showgrid = FALSE
      )
    )
    
    return(fig)
  })
}