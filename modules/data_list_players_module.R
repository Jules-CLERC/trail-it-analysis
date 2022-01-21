#server function

#listPlayers contains :
#   - the player name
#   - the profile ID (which is unique)
#   - the player name ID (Which is a concatenate of the two attributes),
#   it's useful when 2 players are the same player name to do the differences in the GUI

data_list_players <- function(input, output, session, D) {
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )

  observeEvent(D(), {
    if(!is.null(D())) {

      listPlayers <- D() %>%
        group_by(profileID) %>%
        filter(sessionID == max(sessionID)) %>%
        summarise(
          playerName = first(playerName),
          playerNameID = paste(first(playerName), "(", first(profileID), ")"),
          Timestamp = last(Timestamp)
        )
      
      toReturn$df <-  listPlayers
      toReturn$trigger <- toReturn$trigger + 1
    }
  })

  return(toReturn)
}
