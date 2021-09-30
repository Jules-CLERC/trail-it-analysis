shinyServer(function(input, output) {
    D <- callModule(data_import, "data_import_sql")
    #Wait for D
    observeEvent(D$trigger, {
        listPlayers <- callModule(create_list_players, "list_players", D$variable)
        #Wait for listPlayers
        observeEvent(listPlayers$trigger, {
            callModule(calc_nb_players, "nb_players", listPlayers$variable)
            datesPlayers <- callModule(calc_dates_players, "dates_players", D$variable, listPlayers$variable)
            strokesPlayers <- callModule(calc_strokes_players, "strokes_players", D$variable, listPlayers$variable)
            improveReactionTime <- callModule(calc_improve_players, "improve_players", D$variable, listPlayers$variable)
            #Wait for datesPlayers
            observeEvent(datesPlayers$trigger, {
                #Wait for strokesPlayers
                observeEvent(strokesPlayers, {
                    callModule(calc_times_players, "times_players", D$variable, listPlayers$variable, datesPlayers$variable, strokesPlayers$variable, improveReactionTime$variable)
                })
            })
        })
    })
})
