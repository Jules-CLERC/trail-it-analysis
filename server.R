shinyServer(function(input, output) {
    output$nb_players <- renderText({
        paste("Number of players : ", nbPlayers)
    })

    output$dates_player_table <- renderDataTable(
        datesPlayers %>% filter(playerNameID == input$dates_player_select) %>% select(date)
        )

    output$time_minutes_players_graph<- renderPlotly({
        plot_ly() %>%
            add_trace(data = nbTimeGamePlayers, x=~playerNameID, y=~nbMinutes, type = 'bar', mode = 'lines', name=~stroke)
    })

    output$time_days_players_graph<- renderPlotly({
        plot_ly() %>%
            add_trace(data = nbTimeGamePlayers, x=~playerNameID, y=~nbDays, type = 'bar', mode = 'lines', name=~stroke)
    })
    
    output$time_year_by_month_players_graph<- renderPlotly({
        plot_ly() %>%
            add_trace(data = nbTimeGamePlayers, x=~nbYears / nbMonths, y=~playerNameID, type = 'bar', mode = 'lines', name=~isImprove)
    })
    
    output$nb_players_stroke <- renderText({
        paste("Number of players stroke : ", strokePlayers %>% count())
    })

    output$improve_players_table <- renderDataTable(
        improveReactionTime
    )
})
