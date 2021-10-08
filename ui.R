shinyUI(
    fluidPage(
        
        includeCSS("custom.css"),
        useShinyjs(),
        
        tags$header(fluidRow(
            column(1,div(class="text-center",img(src='whack_icon.svg', id="whack-logo"))),
            column(8,data_import_UI("data_import_sql"))
        )),
        
        navlistPanel(id = "pageChooser", well= FALSE, widths=c(2,10),
                     tabPanel("Profile information", page_profile_information_UI("page_profile_information")),
                     tabPanel("Dates by player", page_dates_players_UI("page_dates_players")),
                     tabPanel("Statistics times", page_times_players_UI("page_times_players")),
                     tabPanel("Improvements", page_improves_players_UI("page_improves_players"))
        ),
        
        tags$footer()
    ))