modal_individual_profile_dates_UI <- function(id) {
  ns = NS(id)
  list(
    actionButton(ns("showDatesPlayer"), "View history")
  )
}

modal_individual_profile_dates <- function(input, output, session, datesPlayer) {
  observeEvent(input$showDatesPlayer, {
    ns <- session$ns
    content <- list(
      fluidPage(
        HTML("<h3>Play history</h3>"),
        uiOutput(ns('tableDatesPlayer'))
      )
    )
    insertUI(selector = "#showDatesPlayer", where = "afterEnd",
             ui = showModal(modalDialog(content, easyClose = TRUE)))
  })
  
  output$tableDatesPlayer <- renderUI({
    toReturn = ""
    for (i in 1:nrow(datesPlayer)) {
      fullDate = datesPlayer[i, "Timestamp"][[1]]
      ymd = as.Date(fullDate, format = "%Y-%m-%d %H:%M:%OS")
      tmpRow = paste(paste0("<hr><p>",
                              wday(ymd, label = TRUE), ", ", 
                              ymd(ymd), " at ", 
                              format(strptime(fullDate, format = "%Y-%m-%d %H:%M:%OS"), '%H:%M'), "</p>")
      )
      toReturn = paste(toReturn, tmpRow)
    }
    return(HTML(toReturn))
  })
}