# lw_upload_and_analyze module for iQuaCalc dashboard
# v. 2, with left-side data entry and highcharter



corUploadModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidPage(
      
      tabsetPanel(
        
        tabPanel(title = 'Gallery'
          
        ),
        
        tabPanel(title = 'Upload Data',
                 
                 fluidRow(
                   
                   column(width = 4,
                          
                          fileInput(ns('sample_data_upload'), 
                                    'Choose a .csv file',
                                    accept = c("text/csv", 
                                               "text/comma-separted-values,text/plain", 
                                               ".csv")
                          ),
                          
                          tags$hr(),
                          
                          hidden(checkboxGroupInput(ns('vars'), 
                                                    'Check exactly two variables', 
                                                    choices = c(0)
                          )),
                          
                          fluidRow(
                            
                            column(width = 6,
                                   hidden(radioButtons(ns('rb_length_options'),
                                                       'Length Units',
                                                       choices = c('cm', 'mm', 'inches'),
                                                       selected = 'mm',
                                                       inline = F))
                            ),
                            
                            column(width = 6,
                                   hidden(radioButtons(ns('rb_wbar_options'),
                                                       'Weight Units',
                                                       choices = c('grams', 'mg', 'lbs'),
                                                       selected = 'grams',
                                                       inline = F))
                                   
                            )
                            
                          ),
                          
                          DT::dataTableOutput(ns('sample_data_dt'))
                   ),
                   
                   # plots...
                   column(width = 8,
                          
                          fluidRow(
                            
                            column(width = 6,
                                   
                                   plotOutput(ns('sample_data_plot_raw'), height = '200px')
                            ),
                            
                            column(width = 6,
                                   
                                   plotOutput(ns('sample_data_plot_transformed'), height = '200px')
                            )
                            
                          ),
                          
                          hr(),
                          
                          fluidRow(
                            
                            tabsetPanel(
                              
                              # type = 'pills',
                              
                              tabPanel(title = 'Predictions',
                                       
                                       fluidRow(
                                         
                                         column(width = 5,
                                                
                                                h4('Single Prediction', align = 'center'),
                                                
                                                hidden(numericInput(ns('prediction_in_1'), 
                                                                    'Enter length to predict weight', 
                                                                    value = 1)),
                                                
                                                htmlOutput(ns('linear_prediction_out_1'))
                                                
                                                # htmlOutput(ns('non_linear_prediction_out'))
                                         ),
                                         
                                         column(width = 7,
                                                
                                                # h3('Predictions', align = 'center'),
                                                
                                                DT::dataTableOutput(ns('linear_prediction_dt'))
                                         )
                                         
                                       ) # END fluidRow 'Predictions'
                              ),
                              
                              tabPanel(title = 'Results',
                                       
                                       fluidRow(
                                         
                                         column(width = 6,
                                                
                                                h4('Non-linear Analysis Results', align = 'center'),
                                                htmlOutput(ns('lw_non_linear_results'))
                                         ),
                                         
                                         column(width = 6,
                                                
                                                h4('Linear Analysis Results', align = 'center'),
                                                htmlOutput(ns('lw_linear_results'))
                                         )
                                         
                                       )
                              ),   # END tabPanel 'Results'
                              
                              tabPanel(title = 'Diagnostics',
                                       
                                       fluidRow(
                                         
                                         column(width = 6,
                                                
                                                h4('Non-linear Analysis Diagnostics', align = 'center')
                                                # htmlOutput(ns('lw_non_linear_diagnostics'))
                                         ),
                                         
                                         column(width = 6,
                                                
                                                h4('Linear Analysis Diagnostics', align = 'center')
                                                # htmlOutput(ns('lw_linear_diagnostics'))
                                         )
                                         
                                       )
                              )   # END tabPanel 'Diagnostics'
                              
                            )   # END tabsetPanel
                          )  # END fluidRow with prediction & diagnostic tabPanels
                          
                   )  # END column #2
                 )
        )  # END tabPanel(title = 'Upload Data'
        
      )  # END OUTER tabsetPanel
      
    )  # END fluidPage
    
  )  # END tagList
}



corUploadModule <- function(input, output, session, st) {
  
  
  uploaded_data <- reactive({
    
    # NB: *** HIDE ----
    shinyjs::hide('prediction_in_1')
    shinyjs::hide('prediction_in_2')
    shinyjs::hide('prediction_in_3')
    
    
    inFile <- input$sample_data_upload
    
    if(is.null(inFile))
      return(NULL)
    
    my_data <- read.csv(inFile$datapath, 
                        header = T, 
                        sep = ',', 
                        quote = "'")
    
    # NB: *** SHOW vars et al. ----
    shinyjs::show('vars')
    shinyjs::show('rb_length_options')
    shinyjs::show('rb_wbar_options')
    
    my_data
    
  })
  
  
  # selectedData() ----
  
  selectedData <- reactive({
    
    req(uploaded_data(),
        input$vars %in% names(uploaded_data()), # NB: --CRITICAL--, else freeze when load new data
        length(input$vars) == 2)
    
    my_selectedData <- uploaded_data()[ , input$vars]
    
    # NB: *** change to work with uniform colnames ***
    colnames(my_selectedData) <- c('length', 'wbar')
    
    my_selectedData
    
  })
  
  
  observe(priority = 1, {
    
    updateCheckboxGroupInput(session, 'vars', choices = c(names(uploaded_data())))
    
    freezeReactiveValue(input, 'vars')
    
  })
  
  
  # updateNumericInput ----
  
  observeEvent(selectedData(), {
    
    updateNumericInput(session, 'prediction_in_1', 
                       label = paste0('Enter a length from ', 
                                      min(selectedData()$length), ' to ',
                                      max(selectedData()$length), ' mm'),
                       value = round(mean(selectedData()$length), 2)
    )
    
    freezeReactiveValue(input, 'prediction_in_1')
    
    
    updateNumericInput(session, 'prediction_in_2', 
                       label = paste0('Enter a length from ', 
                                      min(selectedData()$length), ' to ',
                                      max(selectedData()$length)),
                       value = round(mean(selectedData()$length), 2)
    )
    
    freezeReactiveValue(input, 'prediction_in_2')
    
    
    updateNumericInput(session, 'prediction_in_3', 
                       label = paste0('Enter a length from ', 
                                      min(selectedData()$length), ' to ',
                                      max(selectedData()$length)),
                       value = round(mean(selectedData()$length), 2)
    )
    
    freezeReactiveValue(input, 'prediction_in_3')
    
  })
  
  
  # plot RAW data ----
  
  output$sample_data_plot_raw <- renderPlot({
    
    req(selectedData(),
        ncol(selectedData()) == 2)
    
    my_df <- selectedData()
    
    x <- my_df$length
    y <- my_df$wbar
    
    x_name <- names(my_df)[1]
    y_name <- names(my_df)[2]
    
    x_name <- paste0(x_name, ' (', input$rb_length_options, ')')
    y_name <- paste0(y_name, ' (', input$rb_wbar_options, ')')
    
    my_df %>% ggplot(aes(x = x, y = y)) + 
      geom_point() +
      labs(y = y_name, x = x_name, title = 'Raw Data with non-linear fit') +
      # geom_smooth(method = 'gam', col = 'red')
      geom_smooth(col = 'red')
    
  })
  
  
  # plot LN_TRANSFORMED data ----
  
  output$sample_data_plot_transformed <- renderPlot({
    
    req(selectedData(),
        ncol(selectedData()) == 2)
    
    my_df <- log(selectedData())
    
    x <- my_df$length
    y <- my_df$wbar
    
    x_name <- paste0('ln(', names(my_df)[1], ')')
    y_name <- paste0('ln(', names(my_df)[2], ')')
    
    
    # NB: *** SHOW prediction_in ----
    shinyjs::show('prediction_in_1')
    shinyjs::show('prediction_in_2')
    shinyjs::show('prediction_in_3')
    
    
    my_df %>% ggplot(aes(x = x, y = y)) + 
      geom_point() +
      labs(y = y_name, x = x_name, title = 'Log-transformed Data with linear fit') + 
      stat_smooth(method = 'lm', col = 'red')
    
  })
  
  
  # run LINEAR ln-transformed regression ----
  
  getDataFor_lm <- reactive({
    
    req(selectedData(),
        ncol(selectedData()) == 2)
    
    my_data <- log(selectedData())
    
    # my_formula <- as.formula(paste(names(my_data)[1], names(my_data)[2], sep = ' ~ '))
    # my_regression <- lm(formula = my_formula, data = my_data)
    
    my_regression <- lm(formula = wbar ~ length, data = my_data)
    
    my_regression
    
  })
  
  
  # run NON-LINEAR regression ----
  
  getDataFor_non_linear <- reactive({
    
    req(selectedData(),
        ncol(selectedData()) == 2)
    
    # my_formula <- as.formula(paste(names(my_data)[1], names(my_data)[2], sep = ' ~ '))
    # my_regression <- lm(formula = my_formula, data = my_data)
    
    my_regression_non_linear <- nls(formula = wbar ~ a * length^b, 
                                    start = list(a = 0.0001, b = 3),
                                    data = selectedData())
    
    # predictNLS(my_regression_non_linear)
    
    # bc <- bootCase(my_regression_non_linear)
    # print(head(b))
    
    my_regression_non_linear
    
  })
  
  
  # PREDICTION output ----
  
  output$linear_prediction_out_1 <- renderUI({
    
    req(input$prediction_in_1,
        getDataFor_lm())
    
    new_data <- data.frame(length = log(input$prediction_in_1))
    
    p <- predict(getDataFor_lm(), new_data, interval = 'predict')
    
    # back-transform to coefficients of the power function
    
    # cat('\n-----------------------\n')
    RSE <- summary(getDataFor_lm())$sigma  # Residual Standard Error
    cf_baskerville <- exp((RSE^2) / 2)     # Baskerville's correction factor
    # cat('cf_baskerville: ', cf_baskerville, '\n')
    # # with back-transforming bias correction, Baskerville ~ 0.4 % lower than his
    # cat('-----------------------\n')
    
    # print(exp(getDataFor_lm()$coefficients[1]) * exp(new_data)^getDataFor_lm()$coefficients[2])
    # print(getDataFor_lm()$coefficients[2])
    
    # cat('95% prediction interval for wbar = ', exp(p[1]), ' +/-', exp(p[3]) - exp(p[1]), '\n')
    # cat('95% prediction interval for wbar = ',
    #     cf_baskerville * exp(p[1]), ' +/-',
    #     cf_baskerville * (exp(p[3]) - exp(p[1])), '\n')
    
    
    str1 <- paste0('For ', input$prediction_in_1, ' mm, the 95% prediction interval for wbar is ')
    str2 <- paste0(round(cf_baskerville * exp(p[1]), 3), ' +/-', 
                   round(cf_baskerville * (exp(p[3]) - exp(p[1])), 3))
    
    # HTML(paste(tags$h2(str1), tags$h2(str2), tags$h2(str3), tags$br(), sep = '<br/>'))
    HTML(paste(tags$h4(str1), tags$h3(str2), sep = '<br/>'))
    
  })
  
  output$linear_prediction_out_2 <- renderUI({
    
    req(input$prediction_in_2,
        getDataFor_lm())
    
    new_data <- data.frame(length = log(input$prediction_in_2))
    
    p <- predict(getDataFor_lm(), new_data, interval = 'predict')
    
    # back-transform to coefficients of the power function
    RSE <- summary(getDataFor_lm())$sigma  # Residual Standard Error
    cf_baskerville <- exp((RSE^2) / 2)     # Baskerville's correction factor
    
    str1 <- paste0('For length = ', input$prediction_in_2, ', the 95% prediction interval for wbar is ')
    str2 <- paste0(round(cf_baskerville * exp(p[1]), 3), ' +/-', 
                   round(cf_baskerville * (exp(p[3]) - exp(p[1])), 3))
    
    HTML(paste(tags$h4(str1), tags$h3(str2), sep = '<br/>'))
    
  })
  
  output$linear_prediction_out_3 <- renderUI({
    
    req(input$prediction_in_3,
        getDataFor_lm())
    
    new_data <- data.frame(length = log(input$prediction_in_3))
    
    p <- predict(getDataFor_lm(), new_data, interval = 'predict')
    
    # back-transform to coefficients of the power function
    RSE <- summary(getDataFor_lm())$sigma  # Residual Standard Error
    cf_baskerville <- exp((RSE^2) / 2)     # Baskerville's correction factor
    
    str1 <- paste0('For length = ', input$prediction_in_3, ', the 95% prediction interval for wbar is ')
    str2 <- paste0(round(cf_baskerville * exp(p[1]), 3), ' +/-', 
                   round(cf_baskerville * (exp(p[3]) - exp(p[1])), 3))
    
    HTML(paste(tags$h4(str1), tags$h3(str2), sep = '<br/>'))
    
  })
  
  
  
  output$non_linear_prediction_out <- renderUI({
    
    req(input$prediction_in_1,
        getDataFor_non_linear())

    new_data <- data.frame(length = input$prediction_in_1)
    
    p_confidence <- predictNLS(getDataFor_non_linear(), new_data, interval = 'confidence')
    p_prediction <- predictNLS(getDataFor_non_linear(), new_data, interval = 'prediction')
    
    cat('\n\n============================================\n')
    cat('p_confidence\n')
    print(p_confidence$summary)
    cat('\n\np_prediction\n')
    print(p_prediction$summary)
    cat('\n============================================\n\n\n')

    PROP1 <- predictNLS(getDataFor_non_linear(), newdata = new_data, interval = 'prediction')
    PRED1 <- predict(getDataFor_non_linear(), newdata = new_data)
    nls_summary <- PROP1$summary
    cat('\n------------------------\n')
    # cat('names(PROP1$summary)...\n')
    print(nls_summary[5])
    # print(class(nls_summary[11]))
    print(nls_summary[5][1])      # "Sim.2.5%"
    # print(class(nls_summary[11][1]))
    print(nls_summary[6][1])      # "Sim.97.5%"\
    cat('\n------------------------\n')
    cat('input$prediction_in_1 = ', input$prediction_in_1, '\n')
    cat('              PRED1 = ', PRED1, '\n')
    cat('\n------------------------\n\n')
    
    
    # back-transform to coefficients of the power function
    
    # cat('\n-----------------------\n')
    # RSE <- summary(getDataFor_lm())$sigma  # Residual Standard Error
    # cf_baskerville <- exp((RSE^2) / 2)     # Baskerville's correction factor
    # cat('cf_baskerville: ', cf_baskerville, '\n')
    # # with back-transforming bias correction, Baskerville ~ 0.4 % lower than his
    # cat('-----------------------\n')
    
    # print(exp(getDataFor_lm()$coefficients[1]) * exp(new_data)^getDataFor_lm()$coefficients[2])
    # print(getDataFor_lm()$coefficients[2])
    
    # cat('95% prediction interval for wbar = ', exp(p[1]), ' +/-', exp(p[3]) - exp(p[1]), '\n')
    # cat('95% prediction interval for wbar = ', 
    #     cf_baskerville * exp(p[1]), ' +/-', 
    #     cf_baskerville * (exp(p[3]) - exp(p[1])), '\n')
    
    str1 <- paste0('For length = ', input$prediction_in_1, ', the 95% prediction interval for wbar is ')
    str2 <- paste0(round(PRED1, 3), ' +/-', round(nls_summary[6][1] - nls_summary[5][1], 3))

    # HTML(paste(tags$h2(str1), tags$h2(str2), tags$h2(str3), tags$br(), sep = '<br/>'))
    HTML(paste(tags$h4(str1), tags$h3(str2), sep = '<br/>'))

  })
  
  
  predictionsData <- reactive({
    
    req(getDataFor_lm(),
        selectedData())
    
    RSE <- summary(getDataFor_lm())$sigma  # Residual Standard Error
    cf_baskerville <- exp((RSE^2) / 2)     # Baskerville's correction factor
    
    new_data_vector <- data.frame(length = log(seq(min(selectedData()$length),
                                                   max(selectedData()$length), 
                                                   0.5)))
    
    p_vector <- predict(getDataFor_lm(), new_data_vector, interval = 'predict')
    
    my_length <- formatC(round(seq(min(selectedData()$length), max(selectedData()$length), 0.5), 2), 
                         format='f', digits=2)
    
    my_wbar <- formatC(round(cf_baskerville * exp(p_vector), 3), 
                         format='f', digits=3)
    
    data <- data.frame(length = my_length,
                       wbar = my_wbar)
    
    # print(data)
    
    data
    
  })
  
  # render Predictions DT ----
  
  output$linear_prediction_dt <- DT::renderDataTable({
    
    req(predictionsData())
    
    
    datatable( predictionsData(),
               
               colnames = c('length (mm)', 'wBar (g)', 'lower', 'upper'),
               
               rownames = F,
               
               options = list(dom = 'tip',
                              # 'bSort' = F,
                              # 'bInfo' = F,
                              pageLength = 5,

                              # # columnDefs = list(list(targets = 2, visible = F)),
                              columnDefs = list(list(className = 'dt-right', targets = 0:3))

                              # initComplete = JS(
                              #   "function(settings, json) {",
                              #   "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '#000'});",
                              #   "}")
               ) 
    )
    
    
  })
  
  
  
  # RESULTS output ----
  
  output$lw_non_linear_results <- renderUI({
    
    req(getDataFor_non_linear())
    
    nls_stats <- summary(getDataFor_non_linear())
    
    print(names(nls_stats))
    print(nls_stats)
    
    alpha <- formatC(round(nls_stats$coefficients[1], 8), format='f', digits=8)
    beta  <- formatC(round(nls_stats$coefficients[2], 4), format='f', digits=4)
    
    RSE <- nls_stats$sigma  # Residual Standard Error
    
    # HTML(paste0('w = ', alpha, 'L', tags$sup(beta))) # OK, but can't incraese font size
    # HTML(paste0('w = ', alpha, tags$h3('L'), tags$sup(beta)))  # NO -- imposes breaks
    # HTML(tags$h3(paste0('w = ', alpha, 'L', tags$sup(beta))))  # NO -- ERROR: argumemt is not a character vector
    tags$h3(HTML(paste0(tags$strong('w'), ' = ', 
                        alpha, 
                        tags$strong('L'), 
                        tags$sup(beta),
                        '<hr>', 'Residual Standard Error: ', RSE)), 
            align = 'center')    # BINGO!!
    
  })
  
  
  output$lw_linear_results <- renderUI({
    
    req(getDataFor_lm())
    
    lm_stats <- summary(getDataFor_lm())
    
    print(names(lm_stats))
    print(lm_stats)
    
    alpha <- formatC(round(exp(lm_stats$coefficients[1]), 8), format='f', digits=8)
    beta  <- formatC(round(lm_stats$coefficients[2], 4), format='f', digits=4)
    
    RSE   <- formatC(round(lm_stats$sigma, 4), format='f', digits=4)
    
    tags$h3(HTML(paste0(tags$strong('w'), ' = ', 
                        alpha, 
                        tags$strong('L'), 
                        tags$sup(beta),
                        '<hr>', 'Residual Standard Error: ', RSE)), 
            align = 'center')    # BINGO!!
    
  })
  
  
  
  # DIAGNOSTICS output ----
  
  output$lw_non_linear_diagnostics <- renderUI({
    
  })
  
  
  output$lw_linear_diagnostics <- renderUI({
    
  })
  
}