shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Your Name Here"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             
             tabsetPanel(
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NullGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("naomit","dummy")),
                                 bsTooltip(id = "GlmnetPreprocess", 
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "PlsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RpartPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               ),
               tabPanel("Ridge",
                        verbatimTextOutput(outputId="RidgeModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RidgePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit', 'dummy')),
                                 bsTooltip(id = "RidgePreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RidgeGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RidgeGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RidgeMetrics"),
                        hr(),
                        plotOutput(outputId = "RidgeModelPlots"),
                        verbatimTextOutput(outputId = "RidgeRecipe"),
                        verbatimTextOutput(outputId = "RidgeModelSummary2")
               ),
               tabPanel("Boosted Linear Model",
                        verbatimTextOutput(outputId = "BLMModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "BLMPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected =c('dummy','naomit')),
                                 bsTooltip(id = "BLMPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "BLMGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BLMGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BLMMetrics"),
                        hr(),
                        plotOutput(outputId = "BLMModelPlots"),
                        verbatimTextOutput(outputId = "BLMRecipe"),
                        verbatimTextOutput(outputId = "BLMModelSummary2"),
               ),
               tabPanel("Elasticnet",
                        verbatimTextOutput(outputId = "ENETModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "ENETPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit','dummy')),
                                 bsTooltip(id = "ENETPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "ENETGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "ENETGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "ENETMetrics"),
                        hr(),
                        plotOutput(outputId = "ENETModelPlots"),
                        verbatimTextOutput(outputId = "ENETRecipe"),
                        verbatimTextOutput(outputId = "ENETModelSummary2"),
               ),
               tabPanel("Bayesian GLM",
                        verbatimTextOutput(outputId = "BGLMModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "BGLMPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "BGLMPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "BGLMGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BGLMGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BGLMMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "BGLMRecipe"),
                        verbatimTextOutput(outputId = "BGLMModelSummary2"),
               ),
               tabPanel("Cubist",
                        verbatimTextOutput(outputId = "CubistModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "CubistPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit','dummy')),
                                 bsTooltip(id = "CubistPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "CubistGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "CubistGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "CubistMetrics"),
                        hr(),
                        plotOutput(outputId = "CubistModelPlots"),
                        verbatimTextOutput(outputId = "CubistRecipe"),
                        verbatimTextOutput(outputId = "CubistModelSummary2"),
               ),
               tabPanel("PCR Model",
                        verbatimTextOutput(outputId = "PCRModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PCRPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "PCRPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PCRGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PCRGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PCRMetrics"),
                        hr(),
                        plotOutput(outputId = "PCRModelPlots"),
                        verbatimTextOutput(outputId = "PCRRecipe"),
                        verbatimTextOutput(outputId = "PCRModelSummary2")
               ),
               tabPanel("svmLinear",
                        verbatimTextOutput(outputId = "SVMLKModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "SVMLKPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit', 'dummy')),
                                 bsTooltip(id = "SVMLKPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "SVMLKGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "SVMLKGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "SVMLKMetrics"),
                        hr(),
                        plotOutput(outputId = "SVMLKModelPlots"),
                        verbatimTextOutput(outputId = "SVMLKRecipe"),
                        verbatimTextOutput(outputId = "SVMLKModelSummary2"),
               ),
               tabPanel("Least Angle Regression",
                        verbatimTextOutput(outputId = "LARModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "LARPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit', 'dummy')),
                                 bsTooltip(id = "LARPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "LARGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "LARGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "LARMetrics"),
                        hr(),
                        plotOutput(outputId = "LARModelPlots"),
                        verbatimTextOutput(outputId = "LARRecipe"),
                        verbatimTextOutput(outputId = "LARModelSummary2"),
               ),
               tabPanel("Linear Regression with Stepwise Selection",
                        verbatimTextOutput(outputId = "LRSSModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "LRSSPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit','dummy')),
                                 bsTooltip(id = "LRSSPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "LRSSGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "LRSSGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "LRSSMetrics"),
                        hr(),
                        plotOutput(outputId = "LRSSModelPlots"),
                        verbatimTextOutput(outputId = "LRSSRecipe"),
                        verbatimTextOutput(outputId = "LRSSModelSummary2"),
               ),
               tabPanel("Bayesian Ridge Regression",
                        verbatimTextOutput(outputId = "BridgeModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "BridgePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit','dummy')),
                                 bsTooltip(id = "BridgePreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "BridgeGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BridgeGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BridgeMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "BridgeRecipe"),
                        verbatimTextOutput(outputId = "BridgeModelSummary2"),
               ),
               tabPanel("Penalized LR",
                        verbatimTextOutput(outputId = "PLRModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PLRPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit','dummy')),
                                 bsTooltip(id = "PLRPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PLRGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PLRGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PLRMetrics"),
                        hr(),
                        plotOutput(outputId = "PLRModelPlots"),
                        verbatimTextOutput(outputId = "PLRRecipe"),
                        verbatimTextOutput(outputId = "PLRModelSummary2"),
               ),
               tabPanel("Model Rules",
                        verbatimTextOutput(outputId = "MRulesModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "MRulesPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('naomit','dummy')),
                                 bsTooltip(id = "MRulesPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "MRulesGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "MRulesGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "MRulesMetrics"),
                        hr(),
                        plotOutput(outputId = "MRulesModelPlots"),
                        verbatimTextOutput(outputId = "MRulesRecipe"),
                        verbatimTextOutput(outputId = "MRulesModelSummary2"),
               )
               
######################################################### maintenance point ####################################################
               
             )
             ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             plotOutput(outputId = "TestPlot")
    )
  )
))
