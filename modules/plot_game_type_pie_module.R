plot_game_type_pie_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(class="vis-plot",
             plotlyOutput(ns("game_type_pie_plot"))
    )
  )
}

plot_game_type_pie <- function(input, output, session, comparePlayers) {
  
  output$game_type_pie_plot <- renderPlotly({
    validate(need(comparePlayers(), "No data."))
    
    gamemodes = comparePlayers() %>%
      distinct(profileID, gameType) %>%
      group_by(gameType) %>%
      count()
    
    fig <- plot_ly(gamemodes, labels = ~paste("GameType :", gameType), values = ~n, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(n, 'player(s)'),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = FALSE)
    fig <- fig %>% layout(title = 'Distribution : Popularity by game type',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(fig)
  })
  
}