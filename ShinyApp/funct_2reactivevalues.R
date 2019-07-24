function_reactiveValues <- function() {
  reactiveValues(
    
                 dataframe_initialisation = NULL,
                 dataframe_initialisationBis = NULL,
                 dataframe_fixing = NULL,
                 dataframe_withoutcolselected = NULL,
                 
                 columnSelected = NULL,
                 
                 df_types = NULL,
                 df_ranges = NULL,
                 
                 matrixBool = NULL,
                 
                 tabCosts = NULL,
                 validate = FALSE,
                 Results = FALSE,
                 
                 # Results
                 resultsTab = NULL
                 
  )
}