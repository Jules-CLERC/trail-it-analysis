shinyServer(function(input, output) {
    #init reactives values
    r_D <- reactiveValues(df = NULL)
    r_listPlayers <- reactiveValues(df = NULL)
    r_currentPlayer <- reactiveValues(df = NULL)
    
    #reactives values
    D <- callModule(data_import, "data_import_sql")
    listPlayers <- callModule(data_list_players, "data_list_players", 
                                reactive(r_D$df))
    currentPlayer <- callModule(data_select_player, "data_select_player", 
                                reactive(r_listPlayers$df),
                                reactive(r_currentPlayer$df))
    
    #call pages
    callModule(page_individual_profile_information, "page_individual_profile_information",
               reactive(r_D$df),
               reactive(r_currentPlayer$df))
    callModule(page_individual_performance_session, "page_individual_performance_session",
               reactive(r_currentPlayer$df),
               reactive(r_D$df))
    callModule(page_trends_statistics_players, "page_trends_statistics_players",
               reactive(r_D$df),
               reactive(r_currentPlayer$df))
    callModule(page_trends_performance_players, "page_trends_performance_players",
               reactive(r_currentPlayer$df),
               reactive(r_D$df))
    
    observeEvent(D, {
        r_D$df <- D
    })
    observeEvent(listPlayers$trigger, {
        req(listPlayers$trigger > 0)
        r_listPlayers$df <- listPlayers$df
    })
    observeEvent(currentPlayer$trigger, {
        req(currentPlayer$trigger > 0)
        r_currentPlayer$df <- currentPlayer$df
    })
})
