#server function

data_times_games_players <- function(input, output, session, D, listPlayers, datesPlayers, strokesPlayers, improvesPlayers) {
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  
  toListen <- reactive({
    list(D(),listPlayers(), datesPlayers(), strokesPlayers(), improvesPlayers())
  })
  
  observeEvent(toListen(), {
    if(!is.null(D()) && !is.null(listPlayers()) && !is.null(datesPlayers()) && !is.null(strokesPlayers()) && !is.null(improvesPlayers())) {
      #For how much time did each player play? (number of years, months, days, minutes)
      #years
      nbYearsGamePlayers = datesPlayers() %>%
        mutate(year = substr(date, 0, 4)) %>%
        distinct(profileID, year) %>%
        group_by(profileID) %>%
        count()
      names(nbYearsGamePlayers)[2] <- 'nbYears'
      #months
      nbMonthsGamePlayers = datesPlayers() %>%
        mutate(month = substr(date, 0, 7)) %>%
        distinct(profileID, month) %>%
        group_by(profileID) %>%
        count()
      names(nbMonthsGamePlayers)[2] <- 'nbMonths'
      #days
      nbDaysGamePlayers = datesPlayers() %>%
        group_by(profileID) %>%
        count()
      names(nbDaysGamePlayers)[2] <- 'nbDays'
      #minutes
      nbMinutesGamePlayers = D() %>% 
        select(profileID, sessionLength) %>%
        group_by(profileID) %>%
        summarise(sum(sessionLength))
      names(nbMinutesGamePlayers)[2] <- 'nbMinutes'
      #merge all in nbTimeGamePlayers
      nbTimeGamePlayers = merge(nbDaysGamePlayers, nbMinutesGamePlayers, "profileID")
      nbTimeGamePlayers = merge(nbTimeGamePlayers, nbYearsGamePlayers, "profileID")
      nbTimeGamePlayers = merge(nbTimeGamePlayers, nbMonthsGamePlayers, "profileID")
      nbTimeGamePlayers = merge(nbTimeGamePlayers, listPlayers(), "profileID")
      nbTimeGamePlayers = merge(nbTimeGamePlayers, strokesPlayers(), "profileID")
      nbTimeGamePlayers = merge(nbTimeGamePlayers, improvesPlayers(), "profileID")
      
      toReturn$df <-  nbTimeGamePlayers
      toReturn$trigger <- toReturn$trigger + 1
    }
  })
  
  return(toReturn)
}
