plot_age_median_UI <- function(id) {
  ns = NS(id)
  fluidRow(
    plotlyOutput(ns("age_median_graph"))
  )
}

plot_age_median <- function(input, output, session, data) {
  
  toReturn <- reactiveValues(df = NULL)
  
  output$age_median_graph <- renderPlotly({
    validate(need(data(), "No data."), errorClass = "vis")
    
    #Problem with some values
    #e.g "Under 13" -> 0-13
    #e.g "Unknown" -> delete value
    dataAgeMedian <- data() %>%
      distinct(profileID, ageGroup) %>%
      filter(ageGroup != "Unknown") %>%
      mutate(ageGroup = ifelse(ageGroup == "Under 13",
                               "0-13",
                               ageGroup))
    
    #Put values in number
    #e.g :
    # "0-13" -> 6.5
    #Use for calculate the median
    meanRange <- sapply(strsplit(dataAgeMedian$ageGroup, split = "-", fixed = TRUE), function(k) mean(as.numeric(k)))
    ageMedian <- median(meanRange)
    
    #Get the range translations
    #e.g :
    # "0-13" -> c(0,13)
    #" 18-24" -> c(18,24)
    listRange <- dataAgeMedian %>%
      distinct(ageGroup) %>%
      arrange(ageGroup) %>%
      mutate(range1 = sapply(strsplit(ageGroup, split = "-", fixed = TRUE), function(k) as.numeric(k)[1]), 
             range2 = sapply(strsplit(ageGroup, split = "-", fixed = TRUE), function(k) as.numeric(k)[2]))
    
    #select the range of the current ageMedian
    currentRange <- listRange %>%
      filter(range1 <= ageMedian) %>%
      filter(range2 >= ageMedian)
    
    toReturn$df <- currentRange[1,"ageGroup"]
    
    fig <- plot_ly(x = ~dataAgeMedian$ageGroup,
                   type = "histogram", height = 250)
    fig <- fig %>% layout(
      xaxis = list(title = ''))

    return(fig)
  })
  
  return(toReturn)
}