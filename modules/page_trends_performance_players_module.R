page_trends_performance_players_UI <- function(id) {
  ns = NS(id)
  mainPanel(width = 12,
            div(
              h1("Group Performance Comparison"),
              hr(),
              textOutput(ns("text_performance_comparison_scatter")),
              br(),
              plot_group_performance_comparison_scatter_UI(ns("plot_group_performance_comparison_scatter")),
              br(),
              textOutput(ns("text_performance_comparison_histogram")),
              br(),
              plot_group_performance_comparison_histogram_UI(ns("plot_group_performance_comparison_histogram")),
            ),
            div(
              h1("Individual Comparison"),
              hr(),
              plot_individual_comparison_UI(ns("plot_individual_comparison")),
            )
  )
}

#server function
page_trends_performance_players <- function(input, output, session, currentPlayer, D) {

  output$text_performance_comparison_scatter <- renderText({
    ui <- paste0("Patients are on average ", 
                       round(dataPerformanceComparisonScatter$meanPatientsReactionTime - dataPerformanceComparisonScatter$meanNonPatientsReactionTime, 1),
                       " seconds slower than the non-patient group and ", 
                       round(dataPerformanceComparisonScatter$meanPatientsReactionTime - dataPerformanceComparisonScatter$meanReference, 1),
                       " seconds slower compared to the reference.")
    return(ui)
  })
  
  output$text_performance_comparison_histogram <- renderText({
    if(!is.null(dataPerformanceComparisonHistogram$patientsAvgMinutesTotal) && !is.null(dataPerformanceComparisonHistogram$nonPatientsAvgMinutesTotal)) {
      ui <- paste0("Patients train on average for ",
                         round(dataPerformanceComparisonHistogram$patientsAvgMinutesTotal, 1),
                         " minutes in total, whereas non-patient train for ",
                         round(dataPerformanceComparisonHistogram$nonPatientsAvgMinutesTotal, 1),
                         " minutes in total.")
      return(ui)
    }
  })
  
  dataPerformanceComparisonScatter <- callModule(plot_group_performance_comparison_scatter, "plot_group_performance_comparison_scatter", D, currentPlayer)
  
  dataPerformanceComparisonHistogram <- callModule(plot_group_performance_comparison_histogram, "plot_group_performance_comparison_histogram", D, currentPlayer)
  
  callModule(plot_individual_comparison, "plot_individual_comparison", D, currentPlayer)
}
