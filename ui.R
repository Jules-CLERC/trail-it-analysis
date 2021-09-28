shinyUI(fluidPage(

    titlePanel("Trail-it data"),
    
    mainPanel(
        h1("data analysis"),
        br(),

        h2("How many players are there in total?"),
        textOutput("nb_players"),

        h2("On which dates did the players play?"),
        selectInput("dates_player_select",
                    label = "Choose a player",
                    choices = listPlayers["playerNameID"],
                    selected = listPlayers["playerNameID"][0]),
        dataTableOutput('dates_player_table'),

        h2("For how much time did each player play? (number of days, minutes)"),
        h3('number of minutes'),
        plotlyOutput("time_minutes_players_graph"),
        h3('number of days'),
        plotlyOutput("time_days_players_graph"),
        h3('frequency year / month'),
        plotlyOutput("time_year_by_month_players_graph"),

        h2("How many of the players are stroke patients?"),
        textOutput("nb_players_stroke"),

        h2("Did the players improve their overall reaction time?"),
        dataTableOutput('improve_players_table')
    )
))
