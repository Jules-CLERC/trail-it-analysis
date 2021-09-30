shinyUI(
    dashboardPage(
        dashboardHeader(
            title = "Trail-it analysis"
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Home", tabName = "Home", icon = icon("home")),
                menuItem("Dates by player", tabName = "Dates_players", icon = icon("calendar")),
                menuItem("Statistics times", tabName = "Times_players", icon = icon("clock")),
                menuItem("Improvements", tabName = "Improve_players", icon = icon("chart-line"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem("Home",
                    calc_nb_players_UI("nb_players"),
                    calc_strokes_players_UI("strokes_players")
                ),
                tabItem("Dates_players",
                    calc_dates_players_UI("dates_players")
                ),
                tabItem("Times_players",
                        calc_times_players_UI("times_players")
                ),
                tabItem("Improve_players",
                        calc_improve_players_UI("improve_players")
                )
            )
        )
    )
)
