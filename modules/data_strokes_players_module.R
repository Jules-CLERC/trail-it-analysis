#server function
data_strokes_players <- function(input, output, session, D, listPlayers) {
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  
  toListen <- reactive({
    list(D(),listPlayers())
  })
  
  observeEvent(toListen(), {
    if(!is.null(D()) && !is.null(listPlayers())) {
      #How many of the players are stroke patients?
      strokesPlayers = D() %>% 
        select(profileID, trainingReason) %>%
        filter(!(trainingReason == "OtherReason")) %>%
        distinct(profileID) %>%
        mutate(stroke = "stroke")
      
      strokesPlayers = merge(strokesPlayers, listPlayers(), "profileID", all.y = TRUE)
      strokesPlayers = strokesPlayers[c(1,2)]
      strokesPlayers[is.na(strokesPlayers)] <- "no stroke"
      
      toReturn$df <-  strokesPlayers
      toReturn$trigger <- toReturn$trigger + 1
    }
  })
  
  return(toReturn)
}
