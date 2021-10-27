plot_circle_amount_pie_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(class="vis-plot",
             plotlyOutput(ns("circle_amount_pie_plot"))
    )
  )
}

plot_circle_amount_pie <- function(input, output, session, comparePlayers) {
  
  output$circle_amount_pie_plot <- renderPlotly({
    validate(need(comparePlayers(), "No data."))
    
    difficulties = comparePlayers() %>%
      distinct(profileID, circleAmount) %>%
      group_by(circleAmount) %>%
      count()
    
    fig <- plot_ly(difficulties, labels = ~paste("Circle amount :", circleAmount, "points"), values = ~n, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(n, 'player(s)'),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = FALSE)
    fig <- fig %>% layout(title = 'Distribution : Popularity by circle amount',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(fig)
  })
  
}