plot_performance_per_level_UI <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
      tags$div(class='contextual-toolbar', 
               radioButtons(ns("optionFilter"), label = " ",
                                  choices = c("Whole level" = "optionAll", "Left/Right" = "optionLR"),
                                  selected = c("optionAll"), inline = FALSE)
      )
    ),
    fluidRow(
      plotlyOutput(ns("performance_per_level_graph"))
    )
  )
}

plot_performance_per_level <- function(input, output, session, currentSession) {
  
  dataPlot <- reactiveValues(df = NULL)
  
  observeEvent(currentSession(), {
    validate(need(currentSession(), "Waiting for current session"), errorClass = "vis")
    
    dataPlot$df <- currentSession() %>%
      select(levelNumber, levelReactionTime, levelReactionTimeLeft, levelReactionTimeRight) %>%
      mutate(color = ifelse(
        min(levelReactionTime) == levelReactionTime,
        "rgb(97,225,108)",
        "rgb(108,174,242)"
      )) %>%
      mutate(colorL = ifelse(
        min(levelReactionTimeLeft) == levelReactionTimeLeft,
        "rgb(97,225,108)",
        "rgb(108,174,242)"
      )) %>%
      mutate(colorR = ifelse(
        min(levelReactionTimeRight) == levelReactionTimeRight,
        "rgb(97,225,108)",
        "rgb(108,174,242)"
      ))
  })
  
  output$performance_per_level_graph <- renderPlotly({
    validate(need(currentSession(), "Waiting for current session"), errorClass = "vis")
    
    fig <- plot_ly() %>%
      layout(
        xaxis = list(title = 'Level'),
        yaxis = list(title = 'Reaction Time'),
        showlegend = FALSE
      )
    
    if(input$optionFilter == "optionAll") {
      fig <- fig %>%
        add_trace(data = dataPlot$df, x=~levelNumber, y=~levelReactionTime, type = 'bar',
                  text=~paste(round(levelReactionTime,2), "s"), textposition = 'auto', textfont = list(color = '#ffffff', size = 16), 
                  marker = list(color=~color))
    } 
    else if(input$optionFilter == "optionLR") {
      fig <- fig %>%
        add_trace(data = dataPlot$df, x=~levelNumber, y=~levelReactionTimeLeft, type = 'bar',
                  text=~paste(round(levelReactionTimeLeft,2), "s"), textposition = 'auto', textfont = list(color = '#ffffff', size = 16), 
                  marker = list(color=~colorL)) %>%
        add_trace(data = dataPlot$df, x=~levelNumber, y=~levelReactionTimeRight, type = 'bar', 
                  text=~paste(round(levelReactionTimeRight,2), "s"), textposition = 'auto', textfont = list(color = '#ffffff', size = 16), 
                  marker = list(color=~colorR))
    }
    return(fig)
  })
}