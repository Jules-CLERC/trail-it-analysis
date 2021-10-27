plot_session_length_pie_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(class="vis-plot",
             plotlyOutput(ns("session_length_pie_plot"))
    )
  )
}

plot_session_length_pie <- function(input, output, session, comparePlayers) {
  
  output$session_length_pie_plot <- renderPlotly({
    validate(need(comparePlayers(), "No data."))
    
    trainingTime = comparePlayers() %>%
      distinct(profileID, sessionLength) %>%
      group_by(sessionLength) %>%
      count()
    
    fig <- plot_ly(trainingTime, labels = ~paste("Session length :", sessionLength, "min"), values = ~n, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(n, 'player(s)'),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = FALSE)
    fig <- fig %>% layout(title = 'Distribution : Popularity by session length',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(fig)
  })
  
}