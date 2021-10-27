shinyServer(function(input, output) {
    #init reactives values
    r_D <- reactiveValues(df = NULL)
    r_listPlayers <- reactiveValues(df = NULL)
    r_datesPlayers <- reactiveValues(df = NULL)
    r_strokesPlayers <- reactiveValues(df = NULL)
    r_improvesPlayers <- reactiveValues(df = NULL)
    r_timesGamesPlayers <- reactiveValues(df = NULL)
    r_currentPlayer <- reactiveValues(df = NULL)
    
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
    currentPlayer <- callModule(data_select_player, "data_select_player", 
                                reactive(r_listPlayers$df),
                                reactive(r_currentPlayer$df))
    
    #call pages
    callModule(page_individual_profile_information, "page_individual_profile_information",
               reactive(r_D$df),
               reactive(r_currentPlayer$df),
               reactive(r_listPlayers$df),
               reactive(r_timesGamesPlayers$df),
               reactive(r_datesPlayers$df))
    callModule(page_individual_performance_session, "page_individual_performance_session",
               reactive(r_currentPlayer$df),
               reactive(r_D$df))
    callModule(page_individual_performance_over_time, "page_individual_performance_over_time",
               reactive(r_currentPlayer$df),
               reactive(r_D$df))
    callModule(page_trends_statistics_players, "page_trends_statistics_players",
               reactive(r_D$df))
    callModule(page_trends_performance_players, "page_trends_performance_players",
               reactive(r_currentPlayer$df),
               reactive(r_D$df),
               reactive(r_listPlayers$df))
    
    #observeEvents
    observeEvent(D$trigger, {
        req(D$trigger > 0)
        r_D$df <- D$df
        
        # TODO : Check the players of the milo course
        # test1 = r_D$df %>%
        #     filter(substring(playerName, 1, 4) == "MPC-") %>%
        #     filter(date == "2021-10-15") %>%
        #     distinct(playerName)
        # view(test1)
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
    observeEvent(currentPlayer$trigger, {
        req(currentPlayer$trigger > 0)
        r_currentPlayer$df <- currentPlayer$df
    })
})
