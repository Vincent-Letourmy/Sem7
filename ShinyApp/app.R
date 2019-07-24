library(shiny)
require(shinydashboard)
library(e1071) # Naive Bayes
library(caret) 
library(dplyr)
library(plotly) # Plots
library(rhandsontable) # Edit table
library(pROC) # Accuracy, sensibility and specificity
library(shinycssloaders)

source("funct_0downloadFile.R")
source("funct_1UI.R")
source("funct_2reactivevalues.R")
source("funct_3initStep.R")
source("funct_4dataquality.R")
source("funct_5CVNaiveBayes.R")
source("funct_6costs.R")
source("funct_7results.R")
source("funct_8fixing.R")
source("funct_loopResults.R")
#source("funct_other.R")


ui <- dashboardPage(title = 'Costs test - Week 7', function.header(), function.sidebar(), function.body(), skin='red')


server <- function(input, output, session) {
    
    v <- function_reactiveValues()
    
#________________________________________________________ Initialisation _____________________________________________________________________________________________________________________________________________#
    
    
# Upload file °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Selection file
    
    output$selectionfile <- renderUI({
        function.fileInput("fileCSV","")
    })
    
    
    ### Box parameters
    
    output$parametersbox <- function_parametersBox("header","sep","quote",FALSE)
    
    
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
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile("../CSV/risk_factors_cervical_cancer_Original.csv", input$header ,"," , input$quote)
    })
    
    
    ### Next tab button
    
    output$fromLoadToNextTab <- renderUI({
        if (is.null(v$dataframe_initialisation)) return (NULL)
        actionButton("fromLoadToNextTab", "Next")
    })
    observeEvent(input$fromLoadToNextTab, {
        updateTabsetPanel(session, "tabsetInitialisation", "defineNas")
    })
    
    
    
# Define NAs  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Check boxes ?/""/NA
    
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
                v$dataframe_initialisation[,col] <-  ifelse(column == "?", "", column)
            }
        }
        if (input$na){
            for (col in names(v$dataframe_initialisation )) {
                column <- as.character(v$dataframe_initialisation[,col])
                v$dataframe_initialisation[,col] <-  ifelse(is.na(column), "", column)
            }
        }
        
    })
    
    
    
    ### Next tab button
    
    output$fromDefineToNextTab <- renderUI({
        actionButton("fromDefineToNextTab", "Next")
    })
    observeEvent(input$fromDefineToNextTab, {
        updateTabsetPanel(session, "tabsetInitialisation", "optional")
    })
    
    
# Upload file Optional °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Selection file Optional
    
    output$selectionfileOptional <- renderUI({
        function.fileInput("fileCSVOptional", "Fixed")
    })
    
    
    ### Box parameters
    
    output$parametersboxOptional <- function_parametersBox("headerOptional","sepOptional","quoteOptional",FALSE)
    
    
    ### Upload File Button Optional
    
    output$uploadbuttonOptional <- renderUI({
        actionButton("uploadbuttonOptional","Upload")
    })
    observeEvent(input$uploadbuttonOptional,{
        infile <- input$fileCSVOptional
        if (is.null(infile)) return (NULL)
        v$dataframe_fixing <- function.loadFile(infile$datapath, input$headerOptional , input$sepOptional , input$quoteOptional)
        updateTabsetPanel(session, "tabsetinit", "databaseFixed")
    })
    
    
    
    ### Selection Cost Fixing
    
    output$costFixingSelection <- renderUI({
        numericInput("costFixingSelection", label = "Choose a cost of fixing one value", value = 3,min = 0,max = 100,step = 1)
    })
    
    
    
    
# DataBase initial
    
    output$tabLoadedInitialisation <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
# Match of fixing d
    
    output$matchFixing <- renderValueBox({
        function.matching(v$dataframe_initialisation,v$dataframe_fixing, "Fixed DB")
    })
    
    
# Database fixing
    
    output$tabfixing <- renderDataTable(
        v$dataframe_fixing,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
# Skip button °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$skipOptional <- renderUI({
        if (!is.null(v$dataframe_fixing)) return (NULL)
        actionButton("skipOptional","SKIP")
    })
    observeEvent(input$skipOptional,{
        updateTabItems(session,"sidebarmenu", "dqconfig")
    })
    
    
# Next Panel button °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$fromInitToNextButton <- renderUI({
        if (is.null(v$dataframe_fixing)) return (NULL)
        actionButton("fromInitToNextButton","Next step")
    })
    observeEvent(input$fromInitToNextButton,{
        updateTabItems(session,"sidebarmenu", "dqconfig")
    })
    
    
    
#_______________________________________________________ DQ Config __________________________________________________________________________________________________________________________________________#
    

# Upload Types File  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### TYPES selection file
    
    output$selectionfileTypes <- renderUI({
        function.fileInput("fileCSVTypes", "Types")
    })
    
    
    ### Box Types
    
    output$parametersboxTypes <- function_parametersBox("headerTypes","sepTypes","quoteTypes",TRUE)
    
    
# Upload Ranges File  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### RANGES selection file
    
    output$selectionfileRanges <- renderUI({
        function.fileInput("fileCSVRanges","Ranges")
    })
    
    
    ### Box ranges
    
    output$parametersboxRanges <- function_parametersBox("headerRanges","sepRanges","quoteRanges",TRUE)
    
    
    
# TYPES/RANGES upload file  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$typesrangesButton <- renderUI({
        infileRanges <- input$fileCSVRanges
        infileTypes <- input$fileCSVTypes
        if (is.null(infileRanges) || is.null(infileTypes)) return (NULL)
        actionButton("typesrangesButton", "Upload Types/Ranges")
    })
    observeEvent(input$typesrangesButton,{
        infileRanges <- input$fileCSVRanges
        infileTypes <- input$fileCSVTypes
        if (is.null(infileRanges) || is.null(infileTypes)) return (NULL)
        v$df_types <- function.loadFile(infileTypes$datapath, input$headerTypes , input$sepTypes , input$quoteTypes)
        v$df_ranges <- function.loadFile(infileRanges$datapath, input$headerRanges , input$sepRanges , input$quoteRanges)
    })
    
    
    output$typesrangesDemo <- renderUI({
        actionButton("typesrangesDemo", "Demo")
    })
    observeEvent(input$typesrangesDemo,{
        v$df_types <- function.loadFile("../CSV/TypesDataOriginal.csv", input$headerTypes , input$sepTypes , input$quoteTypes)
        v$df_ranges <- function.loadFile("../CSV/RangesDataOriginal.csv", input$headerRanges , input$sepRanges , input$quoteRanges)
    })
    
    
# Matches
    
    output$matchTypes <- renderValueBox({
        function.matching(v$dataframe_initialisation,v$df_types, "Types")
    })
    
    output$matchRanges <- renderValueBox({
        function.matching(v$dataframe_initialisation,v$df_ranges, "Ranges")
    })
    
    
# Types and Ranges tables  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$typesFile <- renderDataTable(
        v$df_types,
        options = list(scrollX = TRUE,pageLength = 5, searching = FALSE)
    )
    
    output$rangesFile <- renderDataTable(
        v$df_ranges,
        options = list(scrollX = TRUE,pageLength = 10, searching = FALSE)
    )
    
    
    
# TYPES/RANGES next panel button  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$fromRangesToNextButton <- renderUI({
        if (is.null(v$df_types) || is.null(v$df_ranges)) return(NULL)
        actionButton("fromRangesToNextButton","Next")
    })
    observeEvent(input$fromRangesToNextButton,{
        
        # Matrix boolean Consistencies values
        
        v$matrixBool <- function.matrixBooleanConsistency(v$dataframe_initialisation, v$df_types, v$df_ranges)
        
        updateTabItems(session, "sidebarmenu", "naivebayesconfig")
    })
    
    
#________________________________________________________ Naive Bayes Config _________________________________________________________________________________________________________________________________________#
    
    
# Choice of naive bayes parameters (target and fold) °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Selection of target
    
    output$selectcolumn <- renderUI(
        function.selectionColumn(v$dataframe_initialisation)
    )
    observeEvent(input$selectcolumn,{
        v$columnSelected <- input$selectcolumn
    })
    
    ### Check column
    
    output$noMV <- renderValueBox({
        col <- v$dataframe_initialisation[,v$columnSelected]
        t <- FALSE
        for (val in col) {
            if (is.na(val) || val == "") t <- TRUE
        }
        if (t) valueBox(value = v$columnSelected , subtitle = "Inconsistencies are detected", icon = icon("thumbs-down",lib='font-awesome'), color = "red",width = "4000px")
        else valueBox(value = v$columnSelected, subtitle = "No inconsistencies", icon = icon("thumbs-up",lib='font-awesome'), color = "green",width = "4000px")

    })
    
    
    ### Selection of fold for Naive Bayes
    
    output$foldselection <- renderUI({
        sliderInput("foldselection","Number of fold for Cross Validation", 1,50,10)
    })
    
    
    ### Next tab
    
    output$fromTargetTonextTabButton <- renderUI({
        actionButton("fromTargetTonextTabButton","Next")
    })
    observeEvent(input$fromTargetTonextTabButton,{
        updateTabsetPanel(session, "tabSetTarget", "removecolumn")
    })
    
    
    
# Remove other targets °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Selection of other targets for removing
    
    output$checkBoxOtherTargets <- renderUI({ 
        
        v$dataframe_withoutcolselected <- v$dataframe_initialisation[,!names(v$dataframe_initialisation)%in%v$columnSelected]
        newList <- rev(names(v$dataframe_withoutcolselected))
        checkboxGroupInput("targets",label = "Select target(s)", choices = newList)
    })
    
    
    ### Remove other targets button
    
    output$ValidCheckBox <- renderUI({
        actionButton("OK","Remove")
    })
    observeEvent(input$OK,{
        if (!is.null(input$targets)){
            
            list <- data.frame(Column = input$targets)
            v$dataframe_initialisation <- v$dataframe_initialisation[,!names(v$dataframe_initialisation)%in%list$Column]
            v$dataframe_fixing <- v$dataframe_fixing[,!names(v$dataframe_fixing)%in%list$Column]
        }
    })
    
    
# DataBase Naive Bayes config °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$tabLoadedTargetConfig <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
# Next Panel button °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$fromTargetToNextButton <- renderUI({
        actionButton("fromTargetToNextButton","Next Step")
    })
    observeEvent(input$fromTargetToNextButton,{
        
        v$dataframe_initialisation <- function.as_factor(v$dataframe_initialisation)
        v$tabCosts <- function.tabNaiveBayes(v$dataframe_initialisation, v$columnSelected)
        updateTabItems(session,"sidebarmenu", "costsconfig")
    })
    
   
#_______________________________________________________ Costs Config __________________________________________________________________________________________________________________________________________#
    
    
# Creation costs tab °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Costs tab editable
    
    output$costsTab <- renderRHandsontable({
        rhandsontable(v$tabCosts)
    })
    
    
    
    ### Validate costs button
    
    output$validate <- renderUI(
        actionButton("validate","Validate"),
    )
    observeEvent(input$validate,{
        v$tabCosts <- function.saveDataInFile(input$costsTab, "MyData.csv")
        v$validate <- TRUE
    })
    
    
    ### Download costs button
    
    output$downloadCostsButton <- renderUI({
        if (v$validate == FALSE) return(NULL)
        downloadButton('downloadData', 'Download Costs Tab')
    })
    output$downloadData <- function.downloadFile(v$tabCosts)
    
    
# Database cost °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$tabLoadedCostsConfig <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
# Naive Bayes LOOP °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    
    ### Next panel button
    
    output$fromCostsToNextButton <- renderUI({
        if (v$validate == FALSE) return (NULL)
        actionButton("fromCostsToNextButton","Results")
    })
    observeEvent(input$fromCostsToNextButton,{
        #As factor to run naive Bayes
        if (! is.null(v$dataframe_fixing) ) v$dataframe_fixing <- function.as_factor(v$dataframe_fixing)
        #v$df_types <- v$df_types[,names(v$dataframe_initialisation)]
        #v$df_ranges <- v$df_ranges[,names(v$dataframe_initialisation)]
        v$matrixBool <- v$matrixBool[,names(v$dataframe_initialisation)]
        updateTabItems(session,"sidebarmenu", "results")
    })
    
    
#________________________________________________________ Results  ____________________________________________________________________________________________#
    

# Results °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    
    output$results <- renderTable ({
        
        # LOOP
        
        v$resultsTab <- function.loopResults(
            
            v$dataframe_initialisation
            ,v$dataframe_fixing
            ,v$matrixBool
            ,v$tabCosts
            ,v$columnSelected
            ,v$df_ranges
            ,input$foldselection )
        
        
    },
    rownames = TRUE,
    striped = TRUE,
    hover = TRUE
    )
    
    output$boxPlotCost <- renderUI({
        box(title = "Cost line chart",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("plotCost")
            )
    })
    
    output$plotCost <- renderPlotly({
        x <- rownames(v$resultsTab)
        plot_ly(v$resultsTab,x = factor(x,levels = x), y = ~v$resultsTab$Cost, type = "scatter", mode = "lines")
    })
    
    output$boxPlotAccuracy <- renderUI({
        box(title = "Accuracy line chart",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("plotAccuracy")
        )
    })
    
    output$plotAccuracy <- renderPlotly({
        x <- rownames(v$resultsTab)
        plot_ly(v$resultsTab,x = factor(x,levels = x), y = ~v$resultsTab$Accuracy, type = "scatter", mode = "lines")
    })
    
    output$boxPlotSensitivity <- renderUI({
        box(title = "Sensitivity line chart",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("plotSensitivity")
        )
    })
    
    output$plotSensitivity <- renderPlotly({
        x <- rownames(v$resultsTab)
        plot_ly(v$resultsTab,x = factor(x,levels = x), y = ~v$resultsTab$Sensitivity, type = "scatter", mode = "lines")
    })
    
    output$boxPlotSpecificity <- renderUI({
        box(title = "Specificity line chart",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("plotSpecificity")
        )
    })
    
    output$plotSpecificity <- renderPlotly({
        x <- rownames(v$resultsTab)
        plot_ly(v$resultsTab,x = factor(x,levels = x), y = ~v$resultsTab$Specificity, type = "scatter", mode = "lines")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
