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
      listPlayers = D() %>%
        distinct(profileID, playerName) %>%
        mutate(playerNameID = paste(playerName, "(", profileID, ")"))
      toReturn$df <-  listPlayers
      toReturn$trigger <- toReturn$trigger + 1
    }
  })

  return(toReturn)
}
