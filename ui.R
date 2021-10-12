shinyUI(
    fluidPage(
        includeCSS("custom.css"),
        useShinyjs(),
        tags$header(fluidRow(
            column(1, div(class="text-center",img(src='icon.jpeg', style="height:50px;"))),
            column(1, data_import_UI("data_import_sql")),
            column(1, actionButton("changeCurrentPlayer", "Change current player"))
        )),
        
        titlePanel("Trail-it analysis"),
        
        navlistPanel(fluid= FALSE, widths=c(1,11), well = FALSE, id = "overall-nav",
                     tabPanel(title = div(class="text-center", img(src='nav_trends.svg', style="max-width:100%;"),tags$br(),"Profile information"),
                              page_profile_information_UI("page_profile_information")
                     ),
                     tabPanel(title = div(class="text-center", img(src='nav_trends.svg', style="max-width:100%;"),tags$br(),"Dates by player"),
                              page_dates_players_UI("page_dates_players")
                     ),
                     tabPanel(title = div(class="text-center", img(src='nav_trends.svg', style="max-width:100%;"),tags$br(),"Statistics times"),
                              page_times_players_UI("page_times_players")
                     ),
                     tabPanel(title = div(class="text-center", img(src='nav_trends.svg', style="max-width:100%;"),tags$br(),"Improvements"),
                              page_improves_players_UI("page_improves_players")
                     ),
                     tabPanel(title = div(class="text-center", img(src='nav_individual.svg', style="max-width:100%;"),tags$br(),"Individual"),
                              navlistPanel(id = "analysisChooser", well= FALSE, widths=c(2,10), fluid = FALSE,
                                           tabPanel(value = "Profile information", id = "ProfileInformation", HTML("Profile information<br><small>See overview information.</small>"),
                                                    div(class="main-content", h1("under construction"))
                                           ),
                                           tabPanel(value = "Performance session", id = "PerformanceSession", HTML("Performance session<br><small>See overview information.</small>"),
                                                    div(class="main-content", h1("under construction"))
                                           ),
                                           tabPanel(value = "Performance over time", id = "PerformanceOverTime", HTML("Performance over time<br><small>See overview information.</small>"),
                                                    div(class="main-content", h1("under construction"))
                                           ),
                              )
                     ),
                     tabPanel(title = div(class="text-center", img(src='nav_trends.svg', style="max-width:100%;"),tags$br(),"Trends"),
                              tabPanel(value = "Under Construction", id = "TrendsOverview", strong("Player Overview"), icon=icon('user'),
                                       div(class="main-content", tags$p("Under Construction.."))
                              ),
                     )
        ),
        
        tags$footer()
    ))