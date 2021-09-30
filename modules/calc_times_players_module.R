#UI function
calc_times_players_UI <- function(id) {
  ns <- NS(id)
  
  list(fluidRow(
    h2("For how much time did each player play? (number of days, minutes)"),
    h3('number of minutes'),
    plotlyOutput(ns("time_minutes_players_graph")),
    h3('number of days'),
    plotlyOutput(ns("time_days_players_graph")),
    h3('frequency year / month'),
    plotlyOutput(ns("time_year_by_month_players_graph"))
  ))
}

#server function
calc_times_players <- function(input, output, session, D, listPlayers, datesPlayers, strokesPlayers, improveReactionTime) {
  #For how much time did each player play? (number of years, months, days, minutes)
  #years
  nbYearsGamePlayers = datesPlayers %>%
    mutate(year = substr(date, 0, 4)) %>%
    distinct(profileID, year) %>%
    group_by(profileID) %>%
    count()
  names(nbYearsGamePlayers)[2] <- 'nbYears'
  #months
  nbMonthsGamePlayers = datesPlayers %>%
    mutate(month = substr(date, 0, 7)) %>%
    distinct(profileID, month) %>%
    group_by(profileID) %>%
    count()
  names(nbMonthsGamePlayers)[2] <- 'nbMonths'
  #days
  nbDaysGamePlayers = datesPlayers %>%
    group_by(profileID) %>%
    count()
  names(nbDaysGamePlayers)[2] <- 'nbDays'
  #minutes
  nbMinutesGamePlayers = D %>% 
    select(profileID, sessionLength) %>%
    group_by(profileID) %>%
    summarise(sum(sessionLength))
  names(nbMinutesGamePlayers)[2] <- 'nbMinutes'
  #merge all in nbTimeGamePlayers
  nbTimeGamePlayers = merge(nbDaysGamePlayers, nbMinutesGamePlayers, "profileID")
  nbTimeGamePlayers = merge(nbTimeGamePlayers, nbYearsGamePlayers, "profileID")
  nbTimeGamePlayers = merge(nbTimeGamePlayers, nbMonthsGamePlayers, "profileID")
  nbTimeGamePlayers = merge(nbTimeGamePlayers, listPlayers, "profileID")
  nbTimeGamePlayers = merge(nbTimeGamePlayers, strokesPlayers, "profileID")
  nbTimeGamePlayers = merge(nbTimeGamePlayers, improveReactionTime, "profileID")
  
  #Show the plots
  
  output$time_minutes_players_graph<- renderPlotly({
      plot_ly() %>%
          add_trace(data = nbTimeGamePlayers, x=~playerNameID, y=~nbMinutes, type = 'bar', name=~stroke)
  })

  output$time_days_players_graph<- renderPlotly({
      plot_ly() %>%
          add_trace(data = nbTimeGamePlayers, x=~playerNameID, y=~nbDays, type = 'bar', name=~stroke)
  })

  output$time_year_by_month_players_graph<- renderPlotly({
      plot_ly() %>%
          add_trace(data = nbTimeGamePlayers, x=~nbYears / nbMonths, y=~playerNameID, type = 'bar', name=~isImprove)
  })
}