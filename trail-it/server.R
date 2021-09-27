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
        datesPlayers %>% filter(playerName == input$dates_player_select) %>% select(date)
        )
    
    output$time_minutes_players_graph<- renderPlotly({
        plot_ly() %>% 
            add_trace(data = nbTimeGamePlayers, x=~playerName, y=~nbMinutes, type = 'bar', mode = 'lines')
    })
    
    output$time_days_players_graph<- renderPlotly({
        plot_ly() %>% 
            add_trace(data = nbTimeGamePlayers, x=~playerName, y=~nbDays, type = 'bar', mode = 'lines')
    })
    
    output$nb_players_stroke <- renderText({ 
        paste("Number of players stroke : ", nbStrokePlayers)
    })
    
    output$improve_players_table <- renderDataTable(
        improveReactionTime
    )
})
