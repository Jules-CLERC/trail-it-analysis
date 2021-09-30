#server function
create_list_players <- function(input, output, session, D) {
  
  listPlayers = D %>% 
    distinct(profileID, playerName) %>%
    mutate(playerNameID = paste(playerName, "(", profileID, ")"))
  
  toReturn <- reactiveValues(
    variable = listPlayers,
    variable_name = "listPlayers",
    trigger = 0
  )
  return(toReturn)
}