shinyUI(fluidPage(

    titlePanel("Trail-it data"),
    
    mainPanel(
        h1("data analysis"),
        br(),
        calc_nb_players_UI("nb_players"),
        calc_dates_players_UI("dates_players"),
        calc_strokes_players_UI("strokes_players"),
        calc_times_players_UI("times_players"),
        calc_improve_players_UI("improve_players")
    )
))
