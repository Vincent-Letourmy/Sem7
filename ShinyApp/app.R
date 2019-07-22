library(shiny)
require(shinydashboard)
library(e1071) # Naive Bayes
library(caret) 
library(dplyr)
library(plotly) # Plots
library(rhandsontable) # Edit table
library(pROC) # Accuracy, sensibility and specificity

source("funct_1UI.R")
source("funct_2reactivevalues.R")
source("funct_3initStep.R")
source("funct_4dataquality.R")
source("funct_5CVNaiveBayes.R")
source("funct_6costs.R")
source("funct_7results.R")
source("funct_8fixing.R")
source("funct_0downloadFile.R")
#source("funct_other.R")


ui <- dashboardPage(title = 'Costs test - Week 7', function.header(), function.sidebar(), function.body(), skin='red')


server <- function(input, output, session) {
    
    v <- function_reactiveValues()
    
    #__________________________________________________ Initialisation _____________________________________________________________________________________________________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Init Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### Upload File Button
    
    output$uploadbutton <- renderUI({
        actionButton("uploadbutton","Upload")
    })
    observeEvent(input$uploadbutton,{
        infile <- input$fileCSV
        if (is.null(infile)) return (NULL)
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile(infile$datapath, input$header , input$sep , input$quote)
    })
    
    
### Upload a demo button
    
    output$demobutton <- renderUI({
        actionButton("demobutton","Upload a Demo")
    })
    observeEvent(input$demobutton,{
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile("risk_factors_cervical_cancer_Original.csv", input$header , input$sep , input$quote)
    })
    
    
### Next tab button
    
    output$fromLoadToNextTab <- renderUI({
        if (is.null(v$dataframe_initialisation)) return (NULL)
        actionButton("fromLoadToNextTab", "Next")
    })
    observeEvent(input$fromLoadToNextTab, {
        updateTabsetPanel(session, "tabsetInitialisation", "typesranges")
        updateTabsetPanel(session, "tabsetinit", "typesranges")
    })
    

#---- TYPES AND RANGES -----------------------------#
    
    
### TYPES selection file
    
    output$selectionfileTypes <- renderUI({
        function.fileInput("fileCSVTypes")
    })
    
    
### Box Types
    
    output$parametersboxTypes <- function_parametersBox("headerTypes","sepTypes","quoteTypes",TRUE)

    
    
### TYPES upload file
    
    output$typesButton <- renderUI({
        infileTypes <- input$fileCSVTypes
        if (is.null(infileTypes)) return (NULL)
        actionButton("typesButton", "Upload Types")
    })
    observeEvent(input$typesButton,{
        infileTypes <- input$fileCSVTypes
        if (is.null(infileTypes)) return (NULL)
        v$df_types <- function.loadFile(infileTypes$datapath, input$headerTypes , input$sepTypes , input$quoteTypes)
    })
    
    
    
### RANGES selection file
    
    output$selectionfileRanges <- renderUI({
        function.fileInput("fileCSVRanges")
    })
    
    
### Box ranges
    
    output$parametersboxRanges <- function_parametersBox("headerRanges","sepRanges","quoteRanges",TRUE)
    
    
### RANGES upload file
    
    output$rangesButton <- renderUI({
        infileRanges <- input$fileCSVRanges
        if (is.null(infileRanges)) return (NULL)
        actionButton("rangesButton", "Upload Ranges")
    })
    observeEvent(input$rangesButton,{
        infileRanges <- input$fileCSVRanges
        if (is.null(infileRanges)) return (NULL)
        v$df_ranges <- function.loadFile(infileRanges$datapath, input$headerRanges , input$sepRanges , input$quoteRanges)
    })
    
    
### TYPES/RANGES next tab button
    
    output$fromRangesToNextButton <- renderUI({
        if (is.null(v$df_types) || is.null(v$df_ranges)) return(NULL)
        actionButton("fromRangesToNextButton","Next")
    })
    observeEvent(input$fromRangesToNextButton,{
        
        updateTabsetPanel(session,"tabsetInitialisation", "defineNas")
        updateTabsetPanel(session, "tabsetinit", "database")
    })
    
    
#-----------------------------------#
    
    
### Next Panel button
    
    output$fromInitToNextButton <- renderUI({
        if (is.null(v$dataframe_initialisation)) return (NULL)
        actionButton("fromInitToNextButton","Next step")
    })
    observeEvent(input$fromInitToNextButton,{
        v$dataframe_targetconfig <- v$dataframe_initialisation
        
        #!# Creation matrix boolean missing and inconsistencing values
        v$matrixBoolInit <- v$matrixBool <- function.matrixBooleanConsistency(v$dataframe_initialisation,v$df_types,v$df_ranges)
        #!#
        
        updateTabItems(session,"sidebarmenu", "targetconfig")
    })
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Init Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### Selection file
    
    output$selectionfile <- renderUI(
        function.fileInput("fileCSV")
    )
   
     
### Parameters box + ?/""/NA
    
    output$parametersbox <- function_parametersBox("header","sep","quote",FALSE)
    
    output$checkBoxInterogation <- renderUI({
        checkboxInput("interrogation", "?")
    })
    
    output$checkBoxEmpty <- renderUI({
        checkboxInput("empty", "\" \"")
    })
    
    output$checkBoxNa <- renderUI({
        checkboxInput("na", "NA")
    })
    
    
    
    
### Validate parameters button
    
    output$confirmNAs <- renderUI({
        actionButton("confirmNAs", "OK")
    })
    observeEvent(input$confirmNAs, {
        v$dataframe_initialisation <- v$dataframe_initialisationBis
        if (input$interrogation){
            for (col in names(v$dataframe_initialisation )) {
                column <- as.character(v$dataframe_initialisation[,col])
                v$dataframe_initialisation[,col] <-  ifelse(column == "", NA, column)
            }
        }
        if (input$na){
            for (col in names(v$dataframe_initialisation )) {
                column <- as.character(v$dataframe_initialisation[,col])
                v$dataframe_initialisation[,col] <-  ifelse(is.na(column), "", column)
            }
        }
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Init Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
### DataBase initial
    
    output$tabLoadedInitialisation <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
### Types table
    
    output$typesFile <- renderDataTable(
        v$df_types,
        options = list(scrollX = TRUE,pageLength = 5, searching = FALSE)
    )
    
    
### Ranges table
    
    output$rangesFile <- renderDataTable(
        v$df_ranges,
        options = list(scrollX = TRUE,pageLength = 10, searching = FALSE)
    )
    
    
    #____________________________________________________ Target Config __________________________________________________________________________________________________________________________________________#
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Target Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### Selection of target
    
    output$selectcolumn <- renderUI(
        function.selectionColumn(v$dataframe_initialisation)
    )
    observeEvent(input$selectcolumn,{
        v$columnSelected <- input$selectcolumn
    })
    
    
### Selection of fold for Naive Bayes
    
    output$foldselection <- renderUI({
        sliderInput("foldselection","Number of fold for Cross Validation (Naive Bayes)", 1,50,10)
    })
    
    
### Selection of other targets for removing
    
    output$checkBox <- renderUI({ 
        
        v$dataframe_withoutcolselected <- v$dataframe_targetconfig[,!names(v$dataframe_targetconfig)%in%v$columnSelected]
        newList <- rev(names(v$dataframe_withoutcolselected))
        checkboxGroupInput("targets",label = "Select target(s)", choices = newList)
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Target Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### Next tab
    
    output$nextButton <- renderUI({
        actionButton("nextButton","Next")
    })
    observeEvent(input$nextButton,{
        updateTabsetPanel(session, "tabSetTarget", "removecolumn")
    })
    
    
### Remove other targets button
    
    output$ValidCheckBox <- renderUI({
        actionButton("OK","Remove")
    })
    observeEvent(input$OK,{
        if (!is.null(input$targets)){
            
            list <- data.frame(Column = input$targets)
            v$dataframe_targetconfig <- v$dataframe_targetconfig[,!names(v$dataframe_targetconfig)%in%list$Column]
            v$matrixBoolInit <- v$matrixBool <- v$matrixBool[,!names(v$matrixBool)%in%list$Column]
        }
    })
    
    
### Next Step
    
    output$fromTargetToNextButton <- renderUI({
        actionButton("fromTargetToNextButton","Next Step")
    })
    observeEvent(input$fromTargetToNextButton,{
        v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfigBis <- v$dataframe_targetconfig
        v$resNAsBarChart <- function.barChartInconsistency(v$matrixBool)
        updateTabItems(session,"sidebarmenu", "dqconfig")
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Target Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### DataBase
    
    output$tabLoadedTargetConfig <- renderDataTable(
        v$dataframe_targetconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    
    #__________________________________________________ DataQuality Config _________________________________________________________________________________________________________________________________________#
    
    
### Selection pourcentage of missing or inconsisting values    

    output$pourcentageSelection <- renderUI(
        sliderInput("pourcentageSelection","Pourcentage of missing values max", 0,100,15)
    )
    
    
### Remove columns button
    
    output$removecolumnbutton <- renderUI({
        if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
        actionButton("removecolumnbutton","Remove")
    })
    observeEvent(input$removecolumnbutton,{
        v$dataframe_dataqualityconfig <- function.removeColumns(v$resNAsBarChart, v$dataframe_dataqualityconfigBis, input$pourcentageSelection, v$columnSelected)
        v$matrixBool <- v$matrixBool[,names(v$dataframe_dataqualityconfig)]
    })
    
    
### Next tab button : Remove columns -> Types Ranges config  # a changer
    
    output$fromRemoveColToNext <- renderUI({
        actionButton("fromRemoveColToNext","Filter according DQ config Files")
    })
    observeEvent(input$fromRemoveColToNext,{
        
        rowRemove <- function.removeConsistency(v$dataframe_dataqualityconfig,v$matrixBool)
        
        v$nbRowRemovedConsistency <- length(rowRemove)
        v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfig[! rownames(v$dataframe_dataqualityconfig)%in%rowRemove, ]
        v$matrixBool <- v$matrixBool[rownames(v$dataframe_dataqualityconfig),]
        
        updateTabsetPanel(session, "tabsetdqconfig", selected = "result")
        updateTabsetPanel(session, "tabset", selected = "database")
    })
    

    
    
### Infos nb Row removed
    
    output$infosRowRemoved <- renderUI({
        h2("Results of DQ config")
        h4("Number of rows removed : ",v$nbRowRemovedConsistency)
    })
    
    
### Download DQ config
    
    output$downloadDataDQconfig <- function.downloadFile(v$dataframe_dataqualityconfig)
    
    output$downloadButtonFixing <- renderUI({
        if(is.null(v$dataframe_dataqualityconfig)) return(NULL)
        downloadButton('downloadDataDQconfig', 'Download CSV')
    })
    
    
### Next Step button
    
    output$fromDQConfigToNextButton <- renderUI({
        if (is.null(v$dataframe_dataqualityconfig)) return (NULL)
        actionButton("fromDQConfigToNextButton","Next Step")
    })
    observeEvent(input$fromDQConfigToNextButton,{
        v$dataframe_costsconfig <- function.as_factor(v$dataframe_dataqualityconfig)
        v$tabCosts <- function.tabNaiveBayes(v$dataframe_costsconfig, v$columnSelected)
        updateTabItems(session,"sidebarmenu", "costsconfig")
    })
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DataQuality Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
### Bar chart missing and inconsistency values
    
    output$NAsBarChart <- renderPlotly({
        pourcent <- input$pourcentageSelection
        if (is.null(pourcent)) return(NULL)
        
        res <- sort(v$resNAsBarChart, decreasing = TRUE)
        col_names <- names(res)
        
        plot_ly(x = factor(col_names, levels = col_names), 
                y = res, 
                type = "bar",
                color = res > pourcent, colors = c("#132B43", "#56B1F7")
        ) %>% 
            layout(xaxis = list(title = "Column's name"),
                   yaxis = list(title = "Pourcentage of missing values"))
        
        
    })
    
    
### Data table
    
    output$tabLoadedDQconfig <- renderDataTable(
        v$dataframe_dataqualityconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )


    
### Tab 0/1 Consistency
    
    output$tabmatrix <- renderDataTable(
        v$matrixBooloeanMissingValues_Consistency,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    

    
    
   
    #____________________________________________________ Costs Config __________________________________________________________________________________________________________________________________________#
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Costs Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
### Database cost
    
    output$tabLoadedCostsConfig <- renderDataTable(
        v$dataframe_costsconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
### Cost tab editable
    
    output$costsTab <- renderRHandsontable({
        rhandsontable(v$tabCosts)
    })
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Costs Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
### Validate costs button
    
    output$validate <- renderUI(
        actionButton("validate","Validate"),
    )
    observeEvent(input$validate,{
        v$tabCosts <- function.saveDataInFile(input$costsTab, "MyData.csv")
        v$validate <- TRUE
    })
    
    
### Download costs button
    
    output$downloadButton <- renderUI({
        if (v$validate == FALSE) return(NULL)
        downloadButton('downloadData', 'Download Costs Tab')
    })
    output$downloadData <- function.downloadFile(v$tabCosts)
    
    
    
### Naive Bayes
    
    output$fromCostsToNextButton <- renderUI({
        if (is.null(v$dataframe_costsconfig) || v$validate == FALSE) return (NULL)
        actionButton("fromCostsToNextButton","Results")
    })
    observeEvent(input$fromCostsToNextButton,{
        
    #As factor to run naive Bayes
        v$dataframe_results <- v$dataframe_costsconfig
        v$dataframe_targetconfig <- function.as_factor(v$dataframe_targetconfig)
        
        
    # Naive Bayes INITIAL 
        resultats <- function.CVNaiveBayes(v$dataframe_targetconfig,input$selectcolumn,v$tabCosts,input$foldselection,v$df_ranges)
        
        div = nrow(v$dataframe_targetconfig)
        
        v$tabDetailsCostsInit <- data.frame(v$tabCosts, Total = function.tabCostsTotal(resultats$restab$cost,v$tabCosts$Cost, div))
        
        v$resultDataSaved = sum(resultats$restab$cost * v$tabCosts$Cost) * 5 / div
        
        v$accuracySaved <- mean(resultats$moy)
        v$accuracyTabSaved <- resultats$moy
        
        v$sensitivitySaved <- mean(resultats$sensitivity)
        v$sensitivityTabSaved <- resultats$sensitivity
        
        v$specificitySaved <- mean(resultats$specificity)
        v$specificityTabSaved <- resultats$specificity
        
        
        
        
    # Naive Bayes according DQ config #
        resultats <- function.CVNaiveBayes(v$dataframe_results,input$selectcolumn,v$tabCosts,input$foldselection,v$df_ranges)
        
        first <- (sum(resultats$restab$cost * v$tabCosts$Cost) * 5 )
        div <- nrow(v$dataframe_results)
        
        v$tabDetailsCostsDQ <- data.frame(v$tabCosts, Total = function.tabCostsTotal(resultats$restab$cost,v$tabCosts$Cost, div))
        
        v$resultData = first / div
        v$accuracy <- mean(resultats$moy)
        v$accuracyTab <- resultats$moy
        
        v$sensitivity <- mean(resultats$sensitivity)
        v$sensitivityTab <- resultats$sensitivity
        
        v$specificity <- mean(resultats$specificity)
        v$specificityTab <- resultats$specificity
        
        
        
        
        updateTabItems(session,"sidebarmenu", "results")
    })
    
    
    

    
    
    #________________________________________________________ Results  ____________________________________________________________________________________________#
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Results initial ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
### Accuracy init
    
    output$accuracyvalueSaved <- renderValueBox(
        function.statBoxWithConfInterval(v$accuracyTabSaved, v$accuracySaved, "Accuracy", "thumbs-up", "yellow")
    )
    
    
### Sensitivity init
    
    output$sensitivityvalueSaved <- renderValueBox({
        function.statBoxWithConfInterval(v$sensitivityTabSaved, v$sensitivitySaved, "Sensitivity", "list", "purple")
    })
    
    
### Specificity init
    
    output$specificityvalueSaved <- renderValueBox(
        function.statBoxWithConfInterval(v$specificityTabSaved, v$specificitySaved, "Specificity", "list", "purple")
    )
   
    
### Cost init
    
    output$costResultsValueSaved <- renderValueBox(
        function.costsResultsValue(v$resultDataSaved)
    )
    
    
### Cost details and box init
    
    output$tabDetailsInit <- renderTable(
        v$tabDetailsCostsInit
    )
    output$boxDetailsInit <- function.detailsBox("tabDetailsInit")
    

### Information init
    
    output$infodataSaved <- renderUI({
        comp <- function.nbInconsistenciesValues(v$matrixBoolInit)
        fluidRow(
            box(
                h4("Initial table : ", ncol(v$dataframe_targetconfig), " x ", nrow(v$dataframe_targetconfig), "  (columns x rows)"),
                h4("Missing and inconsisting values : ", comp)
            )
        )
    })
    
    
### Database init
    
    output$tabLoadedResultsSaved <- renderDataTable(
        v$dataframe_targetconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
### Accuracy bar chart and box init
    
    output$accuracyCVBarSaved <- renderPlotly (
        function.accuracyCVBarChart(v$accuracyTabSaved, v$accuracySaved, input$foldselection)
    )
    output$boxBarChartSaved <- renderUI(
        function.BarChartBox(v$accuracySaved, "accuracyCVBarSaved")
    )
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Results with DATA QUALITY config ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### Accuracy DQ
    
    output$accuracyvalue <- renderValueBox(
        function.statBoxWithConfInterval(v$accuracyTab, v$accuracy, "Accuracy", "thumbs-up", "yellow")
    )
    
    
### Sensitivity DQ
    
    output$sensitivityvalue <- renderValueBox(
        function.statBoxWithConfInterval(v$sensitivityTab, v$sensitivity, "Sensitivity", "list", "purple")
    )
    
    
### Specificity DQ
    
    output$specificityvalue <- renderValueBox(
        function.statBoxWithConfInterval(v$specificityTab, v$specificity, "Specificity", "list", "purple")
    )
    
### Cost DQ
    
    output$costresultsvalue <- renderValueBox(
        function.costsResultsValue(v$resultData)
    )
    
    
### Cost details and box DQ
    
    output$tabDetailsDQ <- renderTable(
        v$tabDetailsCostsDQ
    )
    output$boxDetailsDQ <- function.detailsBox("tabDetailsDQ")
    
    
### Information DQ
    
    output$infodata <- renderUI({
        comp <- function.nbInconsistenciesValues(v$matrixBool)
        fluidRow(
            box(
                h4("New table : ", ncol(v$dataframe_results), " x ", nrow(v$dataframe_results), "  (columns x rows)"),
                h4("Missing and inconsisting values : ", comp)
            )
        )
    })
    
    
### Database DQ
    
    output$tabLoadedResults <- renderDataTable(
        v$dataframe_results,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    

### Accuracy tab and box DQ
    
    output$accuracyCVbar <- renderPlotly (
        function.accuracyCVBarChart(v$accuracyTab, v$accuracy, input$foldselection)
    )
    output$boxBarChar <- renderUI(
        function.BarChartBox(v$accuracy, "accuracyCVbar")
    )
    
    
    #__________________________________________________________ Optional __________________________________________________________________________________________________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Optional : Fixing Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
#---- LOAD FILE ----#
    
    
### Selection file fixing
    
    output$selectionfileFixing <- renderUI({
        function.fileInput("fileCSVFixing")
    })
    
    
### Parameters box fixing
    
    output$parametersboxFixing <- function_parametersBox("headerFixing","sepFixing","quoteFixing",FALSE)
    
    
### Upload button fixing
    
    output$uploadbuttonFixing <- renderUI({
        actionButton("uploadbuttonFixing","Upload")
    })
    observeEvent(input$uploadbuttonFixing, {
        infile <- input$fileCSVFixing
        if (is.null(infile)) return (NULL)
        v$dataframe_fixing <- function.loadFile(infile$datapath, input$headerFixing , input$sepFixing , input$quoteFixing)
        
        
        
    })
    
    
### Next tab
    
    output$fromLoadToNext <- renderUI({
        if (is.null(v$dataframe_fixing) || v$validate == FALSE) return (NULL)
        actionButton("fromLoadToNext","Next")
    })
    observeEvent(input$fromLoadToNext,{
        t <- TRUE
        tryCatch(
            #!# Remove other targets columns 
            v$dataframe_fixing <- v$dataframe_fixing[, names(v$matrixBoolInit)]
            ,
            error = function(cond){
                t = FALSE
            }
        )
        if (t){
            updateTabsetPanel(session,"tabsetfixing","fixing")
        }
    })
    
    
    
#---- Cost of fixing ----#
    
### Selection Cost Fixing
    
    output$costFixingSelection <- renderUI({
        numericInput("costFixingSelection", label = "Choose a cost of fixing one value", value = 3,min = 0,max = 100,step = 1)
    })
    
    
### Next Panel 
    
    output$fromLoadfixingToNextTab <- renderUI({
        actionButton("fromLoadfixingToNextTab","Next")
    })
    observeEvent(input$fromLoadfixingToNextTab, {
        #As factor to run naive Bayes
        v$dataframe_fixing <- function.as_factor(v$dataframe_fixing)
        
        
        # Naive Bayes Fixing 
        resultats <- function.CVNaiveBayes(v$dataframe_fixing,input$selectcolumn,v$tabCosts,input$foldselection,v$df_ranges)
        #v$resultDataFixed = sum(resultats$restab$cost * v$tabCosts$Cost) * 5
        
        div <- nrow(v$dataframe_fixing)
        
        v$tabDetailsCostsFixed <- data.frame(v$tabCosts, Total = function.tabCostsTotal(resultats$restab$cost,v$tabCosts$Cost, div))
        v$resultDataFixed <- sum(v$tabDetailsCostsFixed$Total) / div

        v$fixingCost <- function.nbInconsistenciesValues(v$matrixBoolInit) * input$costFixingSelection
            
        v$accuracyFixed <- mean(resultats$moy)
        v$accuracyTabFixed <- resultats$moy
        
        v$sensitivityFixed <- mean(resultats$sensitivity)
        v$sensitivityTabFixed <- resultats$sensitivity
        
        v$specificityFixed <- mean(resultats$specificity)
        v$specificityTabFixed <- resultats$specificity
        
        
        updateTabItems(session, "sidebarmenu", "resultsfixing")
    })
    
    
#---- Main Panel ----#
    
### Database fixing
    
    output$tabfixing <- renderDataTable(
        v$dataframe_fixing,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Optional : Results fixing Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### Accuracy
    
    output$accuracyvalueFixed <- renderValueBox(
        function.statBoxWithConfInterval(v$accuracyTabFixed, v$accuracyFixed, "Accuracy", "thumbs-up", "yellow")
    )
    
    
### Sensitivity 
    
    output$sensitivityvalueFixed <- renderValueBox(
        function.statBoxWithConfInterval(v$sensitivityTabFixed, v$sensitivityFixed, "Sensitivity", "list", "purple")
    )
    
    
### Specificity 
    
    output$specificityvalueFixed <- renderValueBox(
        function.statBoxWithConfInterval(v$specificityTabFixed, v$specificityFixed, "Specificity", "list", "purple")
    )
    
    
### Cost
    
    output$costResultsValueFixed <- renderValueBox(
        function.costsResultsValueFixed(v$resultDataFixed, v$fixingCost)
    )
    
    
### Cost Details tab
    
    output$tabDetailsFixed <- renderTable(
        v$tabDetailsCostsFixed
    )
    
    output$boxDetailsFixed <- function.detailsBox("tabDetailsFixed")
    
    
### Information
    
    output$infodataFixed <- renderUI({
        fluidRow(
            h4("Fixed table : ", ncol(v$dataframe_fixing), " x ", nrow(v$dataframe_fixing), "  (columns x rows)")
        )
    })
    
    
### Database
    
    output$tabLoadedResultsFixed <- renderDataTable(
        v$dataframe_fixing,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
### Accuracy bar chart and box
    
    output$accuracyCVBarFixed <- renderPlotly (
        function.accuracyCVBarChart(v$accuracyTabFixed, v$accuracyFixed, input$foldselection)
    )
    output$boxBarChartFixed <- renderUI(
        function.BarChartBox(v$accuracyFixed, "accuracyCVBarFixed")
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
