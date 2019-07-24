
function.header <- function() {
  dashboardHeader(title = "Naive Bayes")
}

function.sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Initialisation", tabName = "initialisation"),
      menuItem("Data Quality Config", tabName = "dqconfig"),
      menuItem("Naive Bayes Config",tabName = "naivebayesconfig"),
      menuItem("Costs Config", tabName = "costsconfig"),
      menuItem("Results", tabName = "results"),
      menuItem("Website", icon = icon("send",lib='glyphicon'), 
               href = "https://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29")
    )
  )
}

function.body <- function(){
  dashboardBody(
    tabItems(
      
#________________________________________________________ Initialisation _______________________________________________________________________________________#
      
      tabItem(
        tabName = "initialisation",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Initialisation"),
            
            tabsetPanel(
              id = "tabsetInitialisation",
              
              tabPanel(
                "Upload your file",
                value = "load",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfile"),
                      uiOutput("parametersbox"),
                      fluidRow(
                        column(6, uiOutput("uploadbutton")),
                        column(6, uiOutput("demobutton"))
                      ),
                      tags$br()
                  )
                ),
                uiOutput("fromLoadToNextTab")
              ),
              
              tabPanel(
                "Define NAs",
                value = "defineNas",
                tags$br(),
                fluidRow(
                    box(width = 12,
                        status = "primary",
                        title = "Define NAs",
                        solidHeader = TRUE,
                        uiOutput("checkBoxInterogation"),
                        uiOutput("checkBoxEmpty"),
                        uiOutput("checkBoxNa"),
                        uiOutput("confirmNAs")
                    ),
                    uiOutput("fromDefineToNextTab")
                )
              ),
              
              tabPanel(
                "Optional",
                value = "optional",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfileOptional"),
                      uiOutput("parametersboxOptional"),
                      fluidRow(
                        column(6, uiOutput("uploadbuttonOptional"))
                      ),
                      uiOutput("costFixingSelection")
                  ),
                  uiOutput("fromInitToNextButton"),
                  uiOutput("skipOptional")
                )
                
              )
            )
          ),
          mainPanel(
            tabsetPanel(
              id = "tabsetinit",
              
              tabPanel(
                "Database",
                value = "database",
                dataTableOutput("tabLoadedInitialisation")
              ),
              
              tabPanel(
                "Optional",
                value = "databaseFixed",
                fluidRow(valueBoxOutput("matchFixing")),
                dataTableOutput("tabfixing")
                
              )
              
            )
          )
        )
      ),

      
#________________________________________________________ DataQuality Config _______________________________________________________________________________________#
        
      tabItem(
        tabName = "dqconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Data quality Config"),
            
              fluidRow(
                box(width = 12,
                    uiOutput("selectionfileTypes"),
                    uiOutput("parametersboxTypes")
                ),
                box(width = 12,
                    uiOutput("selectionfileRanges"),
                    uiOutput("parametersboxRanges")
                ),
                uiOutput("typesrangesButton"),
                uiOutput("typesrangesDemo"),
                tags$br(),
                uiOutput("fromRangesToNextButton")
                )
              
            
          ),
          mainPanel(
            fluidRow(valueBoxOutput("matchTypes"))
            ,
            dataTableOutput("typesFile"),
            fluidRow(valueBoxOutput("matchRanges"))
            ,
            dataTableOutput("rangesFile")
          )
        )
        ),
  
  
  
#__________________________________________________________ Naive Bayes Config _________________________________________________________________________________________#
        
      tabItem(
        tabName = "naivebayesconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Naive Bayes Config"),
            
            tabsetPanel(
              id = "tabSetTarget",
              
              tabPanel(
                "Target",
                value = "column",
                tags$br(),
                fluidRow(
                  box(
                    width = 12,
                    uiOutput("selectcolumn"),
                    tags$hr(),
                    uiOutput("foldselection"),
                    uiOutput("fromTargetTonextTabButton")
                  )
                )
              ),
              
              tabPanel(
                "Remove other targets",
                value = "removecolumn",
                tags$br(),
                box(width = 12,
                    uiOutput("checkBoxOtherTargets"),
                    uiOutput("ValidCheckBox")
                ),
                uiOutput("fromTargetToNextButton")
              )
              
            )
          ),
          mainPanel(
            fluidRow(valueBoxOutput("noMV")),
            dataTableOutput("tabLoadedTargetConfig")
          )
        )
      ), 
        
        
#__________________________________________________________ Costs Config _________________________________________________________________________________________#
        
      tabItem(
        tabName = "costsconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Costs Config"),
            tags$br(),
            
            tabsetPanel(
              id = "tabsetcosts",
              
              tabPanel(
                "Prediction",
                value = "prediction",
                fluidRow(
                  box(width = 12,
                      helpText("Editable table : Choose costs and validate"),
                      rHandsontableOutput("costsTab"),
                      tags$br(),
                      uiOutput("validate"),
                      uiOutput("downloadCostsButton")
                  ),
                  tags$hr(),
                  uiOutput("fromCostsToNextButton")
                )
              )
            )
          ),
          
          mainPanel(
            dataTableOutput("tabLoadedCostsConfig")
          )
          
        )
      ),
        
#_____________________________________________________________ Results ___________________________________________________________________________________________#
        
      tabItem(
        tabName = "results",
        fluidPage(
          box(title = "Results",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              column(12, align = "center",
                withSpinner(tableOutput("results"))
              )
          )
          ,
          tags$br(),
          uiOutput("boxPlotCost"),
          uiOutput("boxPlotAccuracy"),
          uiOutput("boxPlotSensitivity"),
          uiOutput("boxPlotSpecificity")
        )
      )
    )
  )
}