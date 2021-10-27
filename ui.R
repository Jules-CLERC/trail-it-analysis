shinyUI(
    fluidPage(
        includeCSS("custom.css"),
        useShinyjs(),
        tags$header(fluidRow(
            column(1, div(class="text-center",img(src='icon.jpeg', style="height:50px;"))),
            column(1, data_import_UI("data_import_sql")),
            column(10, data_select_player_UI("data_select_player"))
        )),
        
        titlePanel("Trail-it analysis"),
        
        navlistPanel(fluid= FALSE, widths=c(1,11), well = FALSE, id = "overall-nav",
                     tabPanel(title = div(class="text-center", img(src='nav_individual.svg', style="max-width:100%;"),tags$br(),"Individual"),
                              navlistPanel(id = "analysisChooser", well= FALSE, widths=c(2,10), fluid = FALSE,
                                           tabPanel(value = "Profile information", id = "ProfileInformation", HTML("Profile information<br><small>See overview information.</small>"),
                                                    div(class="main-content", page_individual_profile_information_UI("page_individual_profile_information"))
                                           ),
                                           tabPanel(value = "Performance session", id = "PerformanceSession", HTML("Performance session<br><small>See overview information.</small>"),
                                                    div(class="main-content", page_individual_performance_session_UI("page_individual_performance_session"))
                                           ),
                                           tabPanel(value = "Performance over time", id = "PerformanceOverTime", HTML("Performance over time<br><small>See overview information.</small>"),
                                                    div(class="main-content", page_individual_performance_over_time_UI("page_individual_performance_over_time"))
                                           ),
                              )
                     ),
                     tabPanel(title = div(class="text-center", img(src='nav_trends.svg', style="max-width:100%;"),tags$br(),"Trends"),
                              navlistPanel(id = "analysisChooser", well= FALSE, widths=c(2,10), fluid = FALSE,
                                           tabPanel(value = "Statistics Players", id = "StatisticsPlayers", HTML("Statistics Players<br><small>See overview information.</small>"),
                                                    div(class="main-content", page_trends_statistics_players_UI("page_trends_statistics_players"))
                                           ),
                                           tabPanel(value = "Performance Players", id = "PerformancePlayers", HTML("Performance Players<br><small>See overview information.</small>"),
                                                    div(class="main-content", page_trends_performance_players_UI("page_trends_performance_players"))
                                           )
                              )
                     )
        ),
        
        tags$footer()
    ))