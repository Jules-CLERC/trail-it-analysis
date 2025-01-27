plot_individual_comparison_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(
      plotlyOutput(ns("plot_individual_comparison_graph"))
    )
  )
}

plot_individual_comparison <- function(input, output, session, data, currentPlayer) {
  
  output$plot_individual_comparison_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    validate(need(currentPlayer(), "No current player."), errorClass = "vis")
    
    #Data for the current player
    nameCurrentPlayer = data() %>% filter(profileID == currentPlayer()) %>% tail(1)
    dataCurrentPlayer <- data() %>%
      filter(profileID == currentPlayer()) %>%
      group_by(sessionID) %>%
      summarise(
        y = first(sessionMedianReactionTime),
        x = nameCurrentPlayer$playerName
      ) %>% select(x,y)
    
    #Data for other players
    dataPlayers <- data() %>%
      group_by(profileID) %>%
      filter(sessionID == max(sessionID)) %>%
      summarise(
        y = first(sessionMedianReactionTime),
        x = ifelse(
          first(trainingReason) == "Unknown" || first(trainingReason) == "OtherReason",
          "Non-Patients",
          "Patients"
        )
      ) %>% select(x,y)
    
    #Bind data
    df <- rbind(dataPlayers, dataCurrentPlayer)
    
    dfMean <- df %>%
      group_by(x) %>%
      summarise(
        meanY = mean(y)
      )
    
    fig1 <- plot_ly() %>%
      add_trace(x=df$x, y=jitter(df$y,amount=.02),
                scalemode='width', points='all', pointpos=0,name='C', jitter=.3,
                scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray')) %>%
      add_trace(x=dfMean$x, y=dfMean$meanY, 
                type='scatter',mode='markers', color=I('black'),marker=list(size=10),
                error_y= list(array=dfMean$meanY)) %>%
      layout(showlegend=F, 
             yaxis = list(title="", violinmode = 'overlay', violingap = 0), 
             xaxis=list(title=""))
    
    return(fig1)
  })
}