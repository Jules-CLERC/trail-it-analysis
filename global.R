###Libraries
library(shiny)
library(shinyjs)
library(RMySQL)
library(tidyverse)
library(plotly)
library(lubridate)

###Options
options("digits.secs"=6)

###modules
#datas
source("modules/data_import_module.R", local = T)
source("modules/data_select_player_module.R", local = T)
source("modules/data_change_player_module.R", local = T)
source("modules/data_select_row_player_module.R", local = T)
source("modules/data_list_players_module.R", local = T)
source("modules/data_dates_players_module.R", local = T)
source("modules/data_strokes_players_module.R", local = T)
source("modules/data_improves_players_module.R", local = T)
source("modules/data_times_games_players_module.R", local = T)
#pages
source("modules/page_individual_profile_information_module.R", local = T)
source("modules/page_individual_performance_session_module.R", local = T)
source("modules/page_individual_performance_over_time_module.R", local = T)
source("modules/page_trends_statistics_players_module.R", local = T)
source("modules/page_trends_performance_players_module.R", local = T)
#plots
source("modules/plot_minutes_by_player_module.R", local = T)
source("modules/plot_days_by_player_module.R", local = T)
source("modules/plot_frequency_year_by_month_player_module.R", local = T)
source("modules/plot_time_player_module.R", local = T)
source("modules/plot_reaction_time_player_over_time_module.R", local = T)
source("modules/plot_game_type_pie_module.R", local = T)
source("modules/plot_circle_amount_pie_module.R", local = T)
source("modules/plot_session_length_pie_module.R", local = T)
source("modules/plot_compare_performance_players_module.R", local = T)


