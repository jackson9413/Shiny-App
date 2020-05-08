shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  ############################################################ NULL ########################################################
  
  
  
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  ##############################################################################  
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################  
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  
############################################################ GLMNET ########################################################
  
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ##############################################################################  
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  ############################################################################## 
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
############################################################ PLS ########################################################
  
  
    
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ##############################################################################  
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
############################################################ RPART ########################################################
  
  
    
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rpart")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ##############################################################################  
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel)
  })     
 
  
   
######################################################### Ridge ###############################################################
  
  
  
  
  ############################################################################## 
  getRidgeRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RidgePreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$RidgeGo,
    {
      method <- "ridge"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getRidgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], 'Ridge')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$RidgeModelSummary0 <- renderText({
    description("ridge")
  })
  
  ##############################################################################  
  output$RidgeMetrics <- renderTable({
    req(models$ridge)
    models$ridge$results[ which.min(models$ridge$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RidgeModelPlots <- renderPlot({
    req(models$ridge)
    plot(models$ridge)
  })
  
  ############################################################################## 
  output$RidgeRecipe <- renderPrint({
    req(models$ridge)
    models$ridge$recipe
  })  
  
  ############################################################################## 
  output$RidgeModelSummary2 <- renderPrint({
    req(models$ridge)
    print(models$ridge)
  })
  
  
  
######################################################## Boosted Linear Model #####################################################
  
  
  
  ###############################################################################
  getBLMRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BLMPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$BLMGo,
    {
      library(bst)
      library(plyr)
      library(dplyr)
      method <- "BstLm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getBLMRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                                         tuneLength=15)
        saveToRds(models[[method]], 'Boosted Linear Model')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$BLMModelSummary0 <- renderText({
    description("BstLm")
  })
  
  ##############################################################################  
  output$BLMMetrics <- renderTable({
    req(models$BstLm)
    models$BstLm$results[ which.min(models$BstLm$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BLMModelPlots <- renderPlot({
    req(models$BstLm)
    plot(models$BstLm)
  })
  
  ############################################################################## 
  output$BLMRecipe <- renderPrint({
    req(models$BstLm)
    models$BstLm$recipe
  })  
  
  ############################################################################## 
  output$BLMModelSummary2 <- renderPrint({
    req(models$BstLm)
    print(models$BstLm)
  }) 
  
  
  
######################################################### Elasticnet #################################################################
  
  
  
  
  ###############################################################################
  getENETRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$ENETPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$ENETGo,
    {
      library(elasticnet)
      method <- "enet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getENETRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], 'ENET')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$ENETModelSummary0 <- renderText({
    description("enet")
  })
  
  ##############################################################################  
  output$ENETMetrics <- renderTable({
    req(models$enet)
    models$enet$results[ which.min(models$enet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$ENETModelPlots <- renderPlot({
    req(models$enet)
    plot(models$enet)
  })
  
  ############################################################################## 
  output$ENETRecipe <- renderPrint({
    req(models$enet)
    models$enet$recipe
  })  
  
  ############################################################################## 
  output$ENETModelSummary2 <- renderPrint({
    req(models$enet)
    print(models$enet)
  })
  
  
  
######################################################### Baysian GLM  ###############################################################
  
  
  
  
  ###############################################################################
  getBGLMRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BGLMPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$BGLMGo,
    {
      library(arm)
      method <- "bayesglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getBGLMRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        saveToRds(models[[method]], 'bayesglm')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$BGLMModelSummary0 <- renderText({
    description("bayesglm")
  })
  
  ##############################################################################  
  output$BGLMMetrics <- renderTable({
    req(models$bayesglm)
    models$bayesglm$results[ which.min(models$bayesglm$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BGLMRecipe <- renderPrint({
    req(models$bayesglm)
    models$bayesglm$recipe
  })  
  
  ############################################################################## 
  output$BGLMModelSummary2 <- renderPrint({
    req(models$bayesglm)
    print(models$bayesglm)
  })
  
  
  
######################################################### Cubist #######################################################################
  
  
  
  ###############################################################################
  getCubistRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$CubistPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$CubistGo,
    {
      library(Cubist)
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getCubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], 'cubist')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$CubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  ##############################################################################  
  output$CubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$CubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  ############################################################################## 
  output$CubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  ############################################################################## 
  output$CubistModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })  
  
  
  
######################################################### PCR ##########################################################################
  
  
  
  
  ##############################################################################
  getPCRRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PCRPreprocess)
  })
  
  
  ##############################################################################
  observeEvent(
    input$PCRGo,
    {
      library(pls)
      method <- "pcr"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getPCRRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], 'pcr')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PCRModelSummary0 <- renderText({
    description("pcr")
  })
  
  ##############################################################################  
  output$PCRMetrics <- renderTable({
    req(models$pcr)
    models$pcr$results[ which.min(models$pcr$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PCRModelPlots <- renderPlot({
    req(models$pcr)
    plot(models$pcr)
  })     
  
  ############################################################################## 
  output$PCRRecipe <- renderPrint({
    req(models$pcr)
    models$pcr$recipe
  })  
  
  ############################################################################## 
  output$PCRModelSummary2 <- renderPrint({
    req(models$pcr)
    summary(models$pcr$finalModel)
  }) 
  
  
  
##################################################### Support Vector Machines with Linear Kernel ######################################
  
  
  
  
  ###############################################################################
  getSVMLKRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$SVMLKPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$SVMLKGo,
    {
      library(kernlab)
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getSVMLKRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),tuneLength = 15)
        saveToRds(models[[method]], 'svmLinear')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$SVMLKModelSummary0 <- renderText({
    description("svmLinear")
  })
  
  ##############################################################################  
  output$SVMLKMetrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$SVMLKModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })
  
  ############################################################################## 
  output$SVMLKRecipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  ############################################################################## 
  output$VMLKModelSummary2 <- renderPrint({
    req(models$svmLinear)
    print(models$svmLinear)
  }) 
  
  
  
######################################################### Least Angle Regression #########################################################
  
  
  
  
  ###############################################################################
  getLARRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$LARPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$LARGo,
    {
      library(lars)
      method <- "lars"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getLARRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], 'lars')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$LARModelSummary0 <- renderText({
    description("lars")
  })
  
  ##############################################################################  
  output$LARMetrics <- renderTable({
    req(models$lars)
    models$lars$results[ which.min(models$lars$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$LARModelPlots <- renderPlot({
    req(models$lars)
    plot(models$lars)
  })
  
  ############################################################################## 
  output$LARRecipe <- renderPrint({
    req(models$lars)
    models$lars$recipe
  })  
  
  ############################################################################## 
  output$LARModelSummary2 <- renderPrint({
    req(models$lars)
    print(models$lars)
  }) 

  
  
######################################################### Linear Regression with Stepwise Selection ########################################
  
  
  
  
  ###############################################################################
  getLRSSRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$LRSSPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$LRSSGo,
    {
      library(leaps)
      method <- "leapSeq"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getLRSSRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength=15)
        saveToRds(models[[method]], 'leapSeq')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$LRSSModelSummary0 <- renderText({
    description("leapSeq")
  })
  
  ##############################################################################  
  output$LRSSMetrics <- renderTable({
    req(models$leapSeq)
    models$leapSeq$results[ which.min(models$leapSeq$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$LRSSModelPlots <- renderPlot({
    req(models$leapSeq)
    plot(models$leapSeq)
  })
  
  ############################################################################## 
  output$LRSSRecipe <- renderPrint({
    req(models$leapSeq)
    models$leapSeq$recipe
  })  
  
  ############################################################################## 
  output$LRSSModelSummary2 <- renderPrint({
    req(models$leapSeq)
    print(models$leapSeq)
  })  
  

  
######################################################### Bayesian Ridge Regression #######################################################
  
  
  
  
  ###############################################################################
  getBridgeRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BridgePreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$BridgeGo,
    {
      library(monomvn)
      method <- "bridge"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getBridgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        saveToRds(models[[method]], 'bridge')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$BridgeModelSummary0 <- renderText({
    description("bridge")
  })
  
  ##############################################################################  
  output$BridgeMetrics <- renderTable({
    req(models$bridge)
    models$bridge$results[ which.min(models$bridge$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BridgeRecipe <- renderPrint({
    req(models$bridge)
    models$bridge$recipe
  })  
  
  ############################################################################## 
  output$BridgeModelSummary2 <- renderPrint({
    req(models$bridge)
    print(models$bridge)
  })  
  
  
  
######################################################### Penalized Linear Regression #####################################################
  
  
  
  
  ###############################################################################
  getPLRRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PLRPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$PLRGo,
    {
      library(penalized)
      method <- "penalized"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getPLRRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength=15)
        saveToRds(models[[method]], 'penalized')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$PLRModelSummary0 <- renderText({
    description("penalized")
  })
  
  ##############################################################################  
  output$PLRMetrics <- renderTable({
    req(models$penalized)
    models$penalized$results[ which.min(models$penalized$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PLRModelPlots <- renderPlot({
    req(models$penalized)
    plot(models$penalized)
  })
  
  ############################################################################## 
  output$PLRRecipe <- renderPrint({
    req(models$penalized)
    models$penalized$recipe
  })  
  
  ############################################################################## 
  output$PLRModelSummary2 <- renderPrint({
    req(models$penalized)
    print(models$penalized)
  })  

  
  
######################################################### Model Rules #####################################################
  
  
  
  
  ###############################################################################
  getMRulesRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$MRulesPreprocess)
  })
  
  
  ###############################################################################
  observeEvent(
    input$MRulesGo,
    {
      library(RWeka)
      method <- "M5Rules"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        set.seed(123)
        models[[method]] <- caret::train(getMRulesRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                                         tuneGrid=expand.grid(pruned=c("Yes", "No"), smoothed=c("Yes", "No")))
        saveToRds(models[[method]], 'M5Rules')
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  ###############################################################################
  output$MRulesModelSummary0 <- renderText({
    description("M5Rules")
  })
  
  ##############################################################################  
  output$MRulesMetrics <- renderTable({
    req(models$M5Rules)
    models$M5Rules$results[ which.min(models$M5Rules$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$MRulesModelPlots <- renderPlot({
    req(models$M5Rules)
    plot(models$M5Rules)
  })
  
  ############################################################################## 
  output$MRulesRecipe <- renderPrint({
    req(models$M5Rules)
    models$M5Rules$recipe
  })  
  
  ############################################################################## 
  output$MRulesModelSummary2 <- renderPrint({
    req(models$M5Rules)
    print(models$M5Rules)
  })    
  
  
######################################################### maintenance point ####################################################
  
          
  
  
  
  
  
  
  
#####################################################################################################################  
  
  
    
  
  
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "Null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)

    
})
