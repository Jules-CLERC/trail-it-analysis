plot_reaction_time_player_over_time_UI <- function(id) {
  ns = NS(id)
  
  list(
    sliderInput(ns("range_colors"), "Range:",
                min = 0, max = 15,
                value = c(0.5,1),
                step = 0.1),
    fluidRow(class="vis-plot",
             plotlyOutput(ns("gridPlot"))
    )
  )
}

plot_reaction_time_player_over_time <- function(input, output, session, gameplaysPlayer) {
  
  output$gridPlot <- renderPlotly({
    validate(need(gameplaysPlayer(), "Need to select a current player."), errorClass = "vis")
    
    #Create 6 zones
    x_zones = c(1,2,3,1,2,3)
    y_zones = c(1,1,1,2,2,2)
    
    #Init a plot
    vistemplate <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    
    #Add the 6 zones to the plot
    fig <- vistemplate %>%
      add_trace(
        x=~x_zones, 
        y=~y_zones, 
        type='scatter',
        mode='markers',
        symbol=I('o'),
        marker=list(size=128),
        hoverinfo='none')
    
    #Get the reaction time of each zone of each session
    listSessionReactionTime <- gameplaysPlayer() %>% filter(levelNumber == 1) %>% 
      select(
        sessionReactionTime_0_0,
        sessionReactionTime_1_0,
        sessionReactionTime_2_0,
        sessionReactionTime_0_1,
        sessionReactionTime_1_1,
        sessionReactionTime_2_1,
        sessionID)
    
    nbSessions <- nrow(
      listSessionReactionTime %>%
      distinct(sessionID)
    )
    
    #Create animation
    #Each frame corresponds to a session
    for (tmpSession in 0:(nbSessions-1)) {
      tmpListSessionReactionTime <- listSessionReactionTime %>% filter(sessionID == tmpSession)
      #For each zone, show the reaction time with an indicative color
      for(i in 1:6) {
        tmpSessionReactionTime = round(tmpListSessionReactionTime[1,i], 2)
        tmp_color = 'rgba (77, 220, 32, 0.4)'      #Green
        if(tmpSessionReactionTime > input$range_colors[2]) {
          tmp_color = 'rgba(240, 77, 66,0.4)'      #Red
        }
        else if (tmpSessionReactionTime > input$range_colors[1]) {
          tmp_color = 'rgba(242, 152, 11,0.4)'     #Orange
        }
        fig <- fig %>%
          add_trace(x=x_zones[i], y=y_zones[i], frame=tmpListSessionReactionTime[1,"sessionID"], type='scatter', mode='markers',
                    marker=list(size=128, color=tmp_color)) %>%
          add_trace(x=x_zones[i], y=y_zones[i], frame=tmpListSessionReactionTime[1,"sessionID"], type='scatter', mode='text',
                    text=tmpSessionReactionTime, textfont = list(color = '#000000', size = 32))
      }
    }
    
    fig <- fig %>%
      layout(yaxis=list(titlefont = list(size=0), title=" "), xaxis=list(titlefont = list(size=0), title=" "))
    
    return(fig)
  })
}