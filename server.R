shinyServer(function(input, output) {
    #init reactives values
    r_D <- reactiveValues(df = NULL)
    r_currentPlayer <- reactiveValues(df = NULL, trigger = 0)
    r_listPlayers <- reactiveValues(df = NULL)
    r_datesPlayers <- reactiveValues(df = NULL)
    r_strokesPlayers <- reactiveValues(df = NULL)
    r_improvesPlayers <- reactiveValues(df = NULL)
    r_timesGamesPlayers <- reactiveValues(df = NULL)
    
    #reactives values
    D <- callModule(data_import, "data_import_sql")
    listPlayers <- callModule(data_list_players, "data_list_players", 
                                reactive(r_D$df))
    improvesPlayers <- callModule(data_improves_players, "data_improves_players", 
                                reactive(r_D$df))
    strokesPlayers <- callModule(data_strokes_players, "data_strokes_players", 
                                reactive(r_D$df),
                                reactive(r_listPlayers$df))
    datesPlayers <- callModule(data_dates_players, "data_dates_players", 
                                reactive(r_D$df),
                                reactive(r_listPlayers$df))
    timesGamesPlayers <- callModule(data_times_games_players, "data_times_games_players", 
                                reactive(r_D$df),
                                reactive(r_listPlayers$df),
                                reactive(r_datesPlayers$df),
                                reactive(r_strokesPlayers$df),
                                reactive(r_improvesPlayers$df))
    callModule(data_change_player, "data_change_player",
                                reactive(r_listPlayers$df),
                                r_currentPlayer)
    
    #call pages
    callModule(page_profile_information, "page_profile_information",
               reactive(r_listPlayers$df),
               reactive(r_strokesPlayers$df))
    callModule(page_improves_players, "page_improves_players",
               reactive(r_listPlayers$df),
               reactive(r_improvesPlayers$df))
    callModule(page_dates_players, "page_dates_players",
               reactive(r_listPlayers$df),
               reactive(r_datesPlayers$df))
    callModule(page_times_players, "page_times_players",
               reactive(r_timesGamesPlayers$df))
    
    #observeEvents
    observeEvent(D$trigger, {
        req(D$trigger > 0)
        r_D$df <- D$df
    })
    observeEvent(listPlayers$trigger, {
        req(listPlayers$trigger > 0)
        r_listPlayers$df <- listPlayers$df
    })
    observeEvent(strokesPlayers$trigger, {
        req(strokesPlayers$trigger > 0)
        r_strokesPlayers$df <- strokesPlayers$df
    })
    observeEvent(datesPlayers$trigger, {
        req(datesPlayers$trigger > 0)
        r_datesPlayers$df <- datesPlayers$df
    })
    observeEvent(improvesPlayers$trigger, {
        req(improvesPlayers$trigger > 0)
        r_improvesPlayers$df <- improvesPlayers$df
    })
    observeEvent(timesGamesPlayers$trigger, {
        req(timesGamesPlayers$trigger > 0)
        r_timesGamesPlayers$df <- timesGamesPlayers$df
    })
    observeEvent(input$changeCurrentPlayer, {
        insertUI(selector = "#changeCurrentPlayer", where = "afterEnd",
                 ui = showModal(modalDialog(data_change_player_UI("data_change_player"), easyClose = TRUE)))
    })
    observeEvent(r_currentPlayer$trigger, {
        req(r_currentPlayer$trigger > 0)
        print(r_currentPlayer$df)
    })
})
