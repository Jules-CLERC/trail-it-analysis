plot_reaction_time_player_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(class="vis-plot",
             plotlyOutput(ns("gridPlot"))
    )
  )
}

plot_reaction_time_player <- function(input, output, session, currentGameplay) {
  
  output$gridPlot <- renderPlotly({
    validate(need(currentGameplay(), "Need to select a gameplay."), errorClass = "vis")
    
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
    
    listSessionReactionTime <- currentGameplay() %>% filter(levelNumber == 1) %>% 
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