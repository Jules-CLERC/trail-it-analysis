#Import libraries
library(shiny)
library(shinydashboard)
library(shinycustomloader)
library(RMySQL)
library(tidyverse)
library(plotly)

#Options
options("digits.secs"=6)

#Import modules
source("modules/data_import_module.R", local = T)
source("modules/create_list_players_module.R", local = T)
source("modules/calc_nb_players_module.R", local = T)
source("modules/calc_dates_players_module.R", local = T)
source("modules/calc_strokes_players_module.R", local = T)
source("modules/calc_times_players_module.R", local = T)
source("modules/calc_improve_players_module.R", local = T)

