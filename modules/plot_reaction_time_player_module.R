plot_reaction_time_player_UI <- function(id) {
  ns = NS(id)
  
  list(
    selectInput(ns("gameplay_player_select"),
                label = "Select gameplay",
                choices = NULL,
                selected = NULL),
    fluidRow(class="vis-plot",
             plotlyOutput(ns("gridPlot")),
    )
  )
}

plot_reaction_time_player <- function(input, output, session, currentPlayer, D) {
  
  currentGameplay <- reactiveValues(df = NULL)
  
  observeEvent(currentPlayer(), {
    if(!is.null(currentPlayer())) {
      #create list of all gamePlays dates
      #Get the first level of each gameplay
      listGameplays = D() %>%
        filter(profileID == currentPlayer()) %>%
        filter(eventLabel == "Level 1 Completed!") %>%
        select(Timestamp)
      
      #I need to do a special case when there is only one choice, because otherwise the date is not detected
      if(nrow(listGameplays["Timestamp"]) == 1) {
        updateSelectInput(session, "gameplay_player_select",
                          choices = listGameplays[1,"Timestamp"])
      }
      else {
        updateSelectInput(session, "gameplay_player_select",
                          choices = listGameplays["Timestamp"])
      }
    }
  })
  
  observeEvent(input$gameplay_player_select, {
      if(input$gameplay_player_select != "") {
        #get the row of the first level
        firstLevel <- D() %>%
          filter(profileID == currentPlayer()) %>%
          mutate(TimestampString = paste0(Timestamp,"")) %>%
          filter(TimestampString == input$gameplay_player_select)
        #get the session results
        currentGameplay$df <- D() %>%
          filter(profileID == currentPlayer()) %>%
          filter(Timestamp >= as.POSIXct(input$gameplay_player_select, format = "%Y-%m-%d %H:%M:%OS"))%>%
          arrange(Timestamp) %>%
          mutate(
            sessionID = ifelse(
              levelNumber > lag(levelNumber, default = 0), 0, 1
            ),
            sessionID = cumsum(sessionID)
          ) %>%
          filter(sessionID == 0)
      }
  })
  
  output$gridPlot <- renderPlotly({
    validate(need(currentPlayer(), "Need current player."), errorClass = "vis")
    
    x_zones = c(1,2,3,1,2,3)
    y_zones = c(1,1,1,2,2,2)
    
    vistemplate <- plot_ly() %>%
      config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    
    fig <- vistemplate %>%
      add_trace(
                x=~x_zones, 
                y=~y_zones, 
                type='scatter',
                mode='markers',
                symbol=I('o'),
                marker=list(size=128),
                hoverinfo='none')
    
    listSessionReactionTime <- currentGameplay$df %>% filter(levelNumber == 1) %>% 
      select(
        sessionReactionTime_0_0,
        sessionReactionTime_1_0,
        sessionReactionTime_2_0,
        sessionReactionTime_0_1,
        sessionReactionTime_1_1,
        sessionReactionTime_2_1)
    
    for(i in 1:6) {
      tmpSessionReactionTime = round(listSessionReactionTime[1,i], 2)
      tmp_color = 'rgba (77, 220, 32, 0.4)'      #Green
      if(tmpSessionReactionTime > 1) {
        tmp_color = 'rgba(240, 77, 66,0.4)'      #Red
      }
      else if (tmpSessionReactionTime > 0.5) {
        tmp_color = 'rgba(242, 152, 11,0.4)'     #Orange
      }
      
      fig <- fig %>%
        add_trace(x=x_zones[i], y=y_zones[i], type='scatter', mode='markers',
                  marker=list(size=128, color=tmp_color)) %>%
        add_trace(x=x_zones[i], y=y_zones[i], type='scatter', mode='text',
                  text=tmpSessionReactionTime, textfont = list(color = '#000000', size = 32))
    }

    fig <- fig %>%
      layout(yaxis=list(titlefont = list(size=0), title=" "), xaxis=list(titlefont = list(size=0), title=" "))
    
    return(fig)
  })
}