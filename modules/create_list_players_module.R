#server function
create_list_players <- function(input, output, session, D) {
  
  listPlayers = D %>% 
    distinct(profileID, playerName) %>%
    mutate(playerNameID = paste(playerName, "(", profileID, ")"))

  #listPlayers contains :
  #   - the player name
  #   - the profile ID (which is unique)
  #   - the player name ID (Which is a concatenate of the two attributes), 
  #   it's useful when 2 players are the same player name to do the differences in the GUI
  
  toReturn <- reactiveValues(
    variable = listPlayers,
    variable_name = "listPlayers",
    trigger = 0
  )
  return(toReturn)
}