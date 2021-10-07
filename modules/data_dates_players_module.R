#server function
data_dates_players <- function(input, output, session, D, listPlayers) {
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  
  toListen <- reactive({
    list(D(),listPlayers())
  })
  
  observeEvent(toListen(), {
    if(!is.null(D()) && !is.null(listPlayers())) {
      datesPlayers = D() %>%
        select(profileID, date) %>%
        distinct(profileID, date) %>%
        arrange(profileID, date)
      datesPlayers = merge(datesPlayers, listPlayers(), "profileID")
      toReturn$df <-  datesPlayers
      toReturn$trigger <- toReturn$trigger + 1
    }
  })
  
  return(toReturn)
  
}
