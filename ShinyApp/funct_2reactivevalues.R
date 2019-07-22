function_reactiveValues <- function() {
  reactiveValues(
    
                 dataframe_initialisation = NULL,
                 dataframe_initialisationBis = NULL,
                 
                 matrixBool = NULL,
                 matrixBoolInit = NULL,

                 dataframe_targetconfig = NULL,
                 
                 dataframe_withoutcolselected = NULL,

                 dataframe_dataqualityconfig = NULL,
                 dataframe_dataqualityconfigBis = NULL,
                 
                 df_types = NULL,
                 df_ranges = NULL,
                 nbRowRemovedConsistency = NULL,
                 
                 dataframe_costsconfig = NULL,
                 
                 dataframe_fixing = NULL,
                 
                 dataframe_results = NULL,
                 
                 columnSelected = NULL,
                 
                 tabCosts = NULL,
                 validate = FALSE,
                 
                 # Results
                 resultData = NULL, 
                 
                 accuracy = NULL, 
                 accuracyTab = NULL,
                 
                 sensitivity = NULL,
                 sensitivityTab = NULL,
                 
                 specificity = NULL,
                 specificityTab = NULL,
                 
                 resMissingValuesBarChart = NULL,
                 
                 # Initialisation df to compare
                 accuracySaved = NULL,
                 accuracyTabSaved = NULL,
                 
                 sensitivitySaved = NULL,
                 sensitivityTabSaved = NULL,
                 
                 specificitySaved = NULL,
                 specificityTabSaved = NULL,
                 
                 resultDataSaved = NULL,
                 
                 # Fixed df
                 fixingCost = NULL,
                 accuracyFixed = NULL,
                 accuracyTabFixed = NULL,
                 
                 sensitivityFixed = NULL,
                 sensitivityTabFixed = NULL,
                 
                 specificityFixed = NULL,
                 specificityTabFixed = NULL,
                 
                 resultDataFixed = NULL,
                 
                 tabDetailsCostsInit = NULL,
                 tabDetailsCostsDQ = NULL,
                 tabDetailsCostsFixed = NULL
                 
  )
}