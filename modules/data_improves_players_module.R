#server function
data_improves_players <- function(input, output, session, D) {
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  
  observeEvent(D(), {
    if(!is.null(D())) {
      #Did the players improve their overall reaction time?
      #TODO : change the calculation of the improve
      avgReactionTime = D() %>%
        select(profileID, date, sessionMedianReactionTime) %>%
        group_by(profileID) %>%
        summarise(avgReactionTime = mean(sessionMedianReactionTime))
      
      lastReactionTime = D() %>%
        select(profileID, date, time, sessionMedianReactionTime) %>%
        group_by(profileID) %>%
        mutate(Timestamp = paste(date,time),
               Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
        arrange(Timestamp) %>%
        filter(time == max(time)) %>%
        summarise(lastReactionTime = min(sessionMedianReactionTime))
      
      improveReactionTime = merge(avgReactionTime, lastReactionTime, "profileID") %>%
        mutate(isImprove = ifelse(avgReactionTime > lastReactionTime, "Improve", "Not improve"))
      improveReactionTime = improveReactionTime[c(1,4)]
      
      toReturn$df <-  improveReactionTime
      toReturn$trigger <- toReturn$trigger + 1
    }
  })
  
  return(toReturn)
  
}
