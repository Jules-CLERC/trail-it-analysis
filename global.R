###Libraries
library(shiny)
library(shinyjs)
library(RMySQL)
library(tidyverse)
library(plotly)
library(lubridate)
library(htmlwidgets)

###Options
options("digits.secs"=6)

###modules
#datas
source("modules/data_import_module.R", local = T)
source("modules/data_select_player_module.R", local = T)
source("modules/data_change_player_module.R", local = T)
source("modules/data_select_row_player_module.R", local = T)
source("modules/data_list_players_module.R", local = T)
#pages
source("modules/page_individual_profile_information_module.R", local = T)
source("modules/page_individual_performance_session_module.R", local = T)
source("modules/page_trends_statistics_players_module.R", local = T)
source("modules/page_trends_performance_players_module.R", local = T)
#modals
source("modules/modal_individual_profile_dates_module.R", local = T)
#plots
source("modules/plot_progress_bar_module.R", local = T)
source("modules/plot_performance_per_level_module.R", local = T)
source("modules/plot_timeline_module.R", local = T)
source("modules/plot_age_median_module.R", local = T)
source("modules/plot_training_reasons_module.R", local = T)
source("modules/plot_circle_amount_module.R", local = T)
source("modules/plot_session_length_module.R", local = T)
source("modules/plot_group_performance_comparison_scatter_module.R", local = T)
source("modules/plot_group_performance_comparison_histogram_module.R", local = T)
source("modules/plot_individual_comparison_module.R", local = T)


