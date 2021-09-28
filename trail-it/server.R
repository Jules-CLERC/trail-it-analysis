#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$nb_players <- renderText({
        paste("Number of players : ", nbPlayers)
    })

    output$dates_player_table <- renderDataTable(
        datesPlayers %>% filter(playerNameID == input$dates_player_select) %>% select(date)
        )

    output$time_minutes_players_graph<- renderPlotly({
        plot_ly() %>%
            add_trace(data = nbTimeGamePlayers, x=~playerNameID, y=~nbMinutes, type = 'bar', mode = 'lines')
    })

    output$time_days_players_graph<- renderPlotly({
        plot_ly() %>%
            add_trace(data = nbTimeGamePlayers, x=~playerNameID, y=~nbDays, type = 'bar', mode = 'lines')
    })

    output$time_frequencies_players_graph<- renderPlotly({
        plot_ly() %>%
            add_trace(data = nbTimeGamePlayers, x=~nbDays/nbMinutes, y=~playerNameID, type = 'bar', mode = 'lines')
    })
    
    output$nb_players_stroke <- renderText({
        paste("Number of players stroke : ", strokePlayers %>% count())
    })

    output$improve_players_table <- renderDataTable(
        improveReactionTime
    )
})
