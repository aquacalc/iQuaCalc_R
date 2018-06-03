# lw_upload_and_analyze module for iQuaCalc dashboard
# v. 2, with left-side data entry and highcharter


# http://onlinelibrary.wiley.com/doi/10.1111/jai.12269/abstract
# Length–weight relationship and condition factor of wild, grow-out and ‘loose-shell affected’ giant tiger shrimp, 
# Penaeus monodon (Fabricius, 1798) (Decapoda: Penaeidae)
# A. Gopalakrishnan, M. Rajkumar, M. M. Rahman, J. Sun, P. J. Antony, B. A. Venmathi maran, J. P. Trilles
# J. Applied Ichthiology Volume 30, Issue 1 February 2014 Pages 251–253
#  
# NB: their "log" is common, not natural
# log10 W = −1.811 + 2.721 log10 L (r2 = 0.71) for healthy cultured shrimp
# log10 W = −1.444 + 2.485 log10 L (r2 = 0.91) for wild shrimp
# log10 W = −1.112 + 2.237 log10 L (r2 = 0.92) for loose-shell affected shrimp

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4977241/
# Length–weight relationship and condition factor of giant tiger shrimp, Penaeus monodon (Fabricius, 1798)
# from four breeding families
# Yundong Li, Falin Zhou, Zhenhua Ma, Jianhua Huang, Shigui Jiang, Qibin Yang, Tao Li, and Jian G. Qin
# Springerplus. 2016; 5(1): 1279
# BL (cm), ~6 - ~12; W (g), ~3 - ~30
# W = 0.0239 * BL^2.789 (R2 = 0.8977) in family S
# W = 0.0206 * BL^2.9107 (R2 = 0.9107) in family A
# W = 0.0211 * BL^2.831 (R2 = 0.8869) in family SA
# W = 0.0249 * BL^2.781 (R2 = 0.9159) in family AS

# https://www.researchgate.net/publication/232380493_Morphometric_relationship_of_length_and_weight_of_giant_tiger_prawn_Penaeus_monodon_according_to_life_stage_sex_and_source
# Morphometric relationship of length and weight of giant tiger prawn Penaeus monodon 
# according to life stage, sex and source
# 1998. Aquaculture 164(1): 67-75
# Primavera, J. H., F. D. Parado-Estepa, and J. Lebata
# mm and g
# W = (1.376 x 10^-3) * CL^2.756 : growout (Fig. 2)
# W = (1.195 x 10^-5) * BL^3.034 : growout (Fig. 3)
# W = (7.335 x 10^-6) * TL^3.091 : growout (Fig. 4)


# http://pubs.iclarm.net/Naga/FB_2986.pdf
# setiferus
# CL (CL!) in mm and W in g
# W = 0.00196 * TL^2.746

# http://texasseagrant.org/assets/uploads/publications/1982/82-806.pdf
# LENGTH-WEIGHT RELATIONS FOR SEVERAL SPECIES OF PENAEID SHRIMP CULTURED IN PONDS NEAR CORPUS CHRISTI, TEXAS
# David L. Hutchins, George W. Chamberlain, Jack C. Parker
# J. World Aquaculture Soc. Volume 10, Issue 1-4 March 1979  Pages 565–570
# ~10 - 170 mm
#  vann: log10(w) = -5.197 + 3.037 * log10(TL)
# ~30 - 70 mm
#  pink: log10(w) = -5.381 + 3.137 * log10(TL)


lwUploadModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      tabsetPanel(
        
        tabPanel(title = 'L/W Gallery',
                 
                 fluidRow(
                   
                   column(width = 6,
                          
                          fluidRow(
                            column(width = 6,
                                   
                                   tabsetPanel(id = ns('species_gallery'),
                                               
                                               tabPanel(title = 'Penaeids',
                                                        
                                                        wellPanel(style = 'padding: 3px;',
                                                                  
                                                                  radioButtons(ns('rb_gallery_spp_shrimp'), 
                                                                               'Choose species to display', 
                                                                               choices = c('L. vannamei',
                                                                                           'P. monodon',
                                                                                           'P. semisulcatus',
                                                                                           'P. duorarum'),
                                                                               selected = 'L. vannamei',
                                                                               inline = F
                                                                  )
                                                        )
                                               ),
                                               
                                               tabPanel(title = 'Salmonids',
                                                        
                                                        wellPanel(style = 'padding: 3px;',
                                                                  
                                                                  radioButtons(ns('rb_gallery_spp_salmon'), 
                                                                               'Choose species to display', 
                                                                               choices = c('Atlantic',
                                                                                           'Chinook',
                                                                                           'Coho',
                                                                                           'Pink'),
                                                                               selected = 'Chinook',
                                                                               inline = F
                                                                  )
                                                        )
                                               )
                                   )  # End penaeid/salmon tabsetPanel
                            ),
                            
                            column(width = 6,
                                   
                                   wellPanel(style = 'padding: 3px;',
                                     plotOutput(ns('l_w_plot_gallery'), height = '175px')
                                   )
                            )
                            
                          ),
                          
                          wellPanel(align = 'center', 
                                    style = 'padding: 3px;',
                                    htmlOutput(ns('gallery_ref_and_info'))
                          ),
                          
                          wellPanel(style = 'padding: 3px;',
                            
                            h4('Single Prediction', align = 'center'),
                            
                            splitLayout(cellWidths = c('50%', '5%', '45%'),
                                        
                                        numericInput(ns('prediction_in_gallery'), 
                                                     'Enter length to predict weight', 
                                                     value = 1,
                                                     step = 0.01),
                                        
                                        tags$h6(''),
                                        
                                        htmlOutput(ns('linear_prediction_out_gallery'))
                              
                            )
                          )
                          
                   ),  # End column 1/2 for L/W Gallery
                   
                   column(width = 6,
                          
                          DT::dataTableOutput(ns('length_weight_dt'))
                   )
                   
                 )  # END tabPanel 1's fluidRow
        ),  # End 1st tabPanel
        
        
        tabPanel(title = 'Upload L/W Data',
                 
                 fluidRow(
                   
                   column(width = 4,
                          
                          wellPanel(style = 'padding: 3px;',
                                    h5(align = 'center',
                                       'Upload a .csv file with 2 columns:'),
                                    h5(align = 'center',
                                       'length & weight, in that order'),
                                    
                                    fileInput(ns('sample_data_upload'), 
                                              '',
                                              accept = c("text/csv", 
                                                         "text/comma-separted-values,text/plain", 
                                                         ".csv")
                                    )
                          ),
                          
                          fluidRow(
                            
                            column(width = 6,
                                   hidden(radioButtons(ns('rb_length_options'),
                                                       'Length Units',
                                                       choices = c('cm', 'mm', 'in'),
                                                       selected = 'mm',
                                                       inline = T))
                            ),
                            
                            column(width = 6,
                                   hidden(radioButtons(ns('rb_wbar_options'),
                                                       'Weight Units',
                                                       choices = c('g', 'mg', 'lbs'),
                                                       selected = 'g',
                                                       inline = T))
                            )
                            
                          ),
                          
                          # imageOutput('shrimp_metrics'),
                          
                          # see: http://stackoverflow.com/questions/24212329/conditionalpanel-and-renderimage-shiny?rq=1
                          tags$img(src = 'Shrimp_Length_Graphic.png'),
                          # width = "270px", height = "145px"),
                          
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
                            
                            tabsetPanel(# type = 'pills',
                              
                              tabPanel(title = 'Predictions',
                                       
                                       fluidRow(
                                         
                                         column(width = 4,
                                                
                                                h4('Single Prediction', align = 'center'),
                                                
                                                hidden(numericInput(ns('prediction_in_1'), 
                                                                    'Enter length to predict weight', 
                                                                    value = 1)),
                                                
                                                htmlOutput(ns('linear_prediction_out_1'))
                                                
                                                # htmlOutput(ns('non_linear_prediction_out'))
                                         ),
                                         
                                         column(width = 8,
                                                
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
          
        ),  # End 2nd tabPanel
        
        
        tabPanel(title = 'Info',
                 
                 fluidRow(
                   
                   column(width = 8,
                          
                          wellPanel(
                            
                            style = "background-color: #000; color: white;",
                            
                            h5(tags$li('Most aquaculture produces edible biomass')),
                            
                            h5(tags$li('Product weight partly determines market value')),
                            
                            h5(tags$li('Estimating mean weight & biomass thus is essential for commercial success')),
                            
                            h5(tags$li('Length is more quickly & easily measured than weight')),
                            
                            h5(tags$li(HTML(paste0('Length covaries with weight as a power function: W = α*L', tags$sup('β'))))),
                            
                            h5(tags$li('Log transformation linearizes that function to an easily analyzed form: log(W) = log(α) + β*log(L)')),
                            
                            h5(tags$li(HTML('In such <em>allometric</em> relationships, weight scales roughly as the cube of length, so β ~= 3'))),
                            
                            h5(tags$li('Various length measurements may be used, as illustrated at right')),
                            
                            h5(tags$li('')),
                            
                            HTML('<li>Back-transforming the logarithmic the error ε has zero mean, constant variance, and is normally distributed</li><br>
                                 
                                 <li>... the natural (i.e., actual) scale requires back-transforming the logarithmic predictions. Because these transformations involve non-linearity, care must be taken during this step to avoid bias. Several correction factors</li>'
                            )
                            )
                            ), # END column #1/2 of 'Basics'
                   
                   column(width = 4,
                          
                          # tabs: shrimp, fish -- squid...
                          tabsetPanel(
                            
                            tabPanel(title = 'Shrimp',
                                     
                                     fluidRow(
                                       # imageOutput('shrimp_metrics'),
                                       wellPanel(
                                         # see: http://stackoverflow.com/questions/24212329/conditionalpanel-and-renderimage-shiny?rq=1
                                         tags$img(src = 'Shrimp_Length_Graphic.png')
                                         # width = "270px", height = "145px"),
                                       )
                                     ),
                                     
                                     fluidRow(
                                       
                                       wellPanel(
                                         HTML('<b>      TOTAL LENGTH</b>: tip of rostrum to tip of telson<br>'),
                                         HTML('<b>       BODY LENGTH</b>: ...to tip of telson<br>'),
                                         HTML('<b> CARAPACE LENGTH 1</b>: The distance from the anterior most part of the head (usually the rostrum) to the posterior most part of the carapace<br>'),
                                         HTML('<b> CARAPACE LENGTH 2</b>: Partial Carapace Length (PCL): Distance from the posterior margin of orbit to the posterior
                                              edge of the carapace <br>'),
                                         HTML('<i> Examples</i>: ...<br>')
                                         )
                                     )
                                     ),
                            
                            tabPanel(title = 'Fish',
                                     fluidRow(
                                       # imageOutput('shrimp_metrics'),
                                       
                                       # see: http://stackoverflow.com/questions/24212329/conditionalpanel-and-renderimage-shiny?rq=1
                                       tags$img(src = 'Fish_Length_Graphic.png')
                                       # width = "270px", height = "145px"),
                                     ),
                                     
                                     fluidRow(
                                       
                                       wellPanel(
                                         HTML('<b>    TOTAL LENGTH</b>: ...<br>'),
                                         HTML('<b>     FORK LENGTH</b>: ...<br>'),
                                         HTML('<b> STANDARD LENGTH</b>: ...<br>'),
                                         HTML('<i> Examples</i>: ...<br>')
                                       )
                                     )
                            ) # END tabPanel(title = 'Fish'
                            ) # END tabsetPanel for 'Shrimp' & 'Fish'
                          ) # END column #2/2 of 'Basics' tab
                   
                          ) # END outer fluidRow of 'Basics' tab
          
        )  # End 3rd tabPanel
        
      )  # END MAIN tabsetPanel
      
    )  # END MAIN fluidRow
    
  )  # END tagList
    
  # ----------------------------------------*
  
}



lwUploadModule <- function(input, output, session, st) {
  
  
  gallery_lw_data <- reactiveValues(vann_lw_data = NULL, 
                                    monodon_lw_data = NULL)
  
  
  # ' '----
  # >> L-W UPLOAD << ----
  
  
  uploaded_data <- reactive({
    
    # NB: *** HIDE ----
    shinyjs::hide('prediction_in_1')
    # shinyjs::hide('prediction_in_2')
    # shinyjs::hide('prediction_in_3')
    
    
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
    
    req(uploaded_data())
    
    my_selectedData <- uploaded_data()
    
    # NB: *** change to work with uniform colnames ***
    colnames(my_selectedData) <- c('length', 'wbar')
    
    my_selectedData
    
  })
  
  
  # observe(priority = 1, {
  #   
  #   updateCheckboxGroupInput(session, 'vars', choices = c(names(uploaded_data())))
  #   
  #   freezeReactiveValue(input, 'vars')
  #   
  # })
  
  
  # updateNumericInput Pred Upload ----
  
  observeEvent(selectedData(), {
    
    updateNumericInput(session, 'prediction_in_1', 
                       label = paste0('Enter a length from ', 
                                      min(selectedData()$length), ' to ',
                                      max(selectedData()$length), ' mm'),
                       value = round(mean(selectedData()$length), 2)
    )
    
    freezeReactiveValue(input, 'prediction_in_1')
    
    
    # updateNumericInput(session, 'prediction_in_2', 
    #                    label = paste0('Enter a length from ', 
    #                                   min(selectedData()$length), ' to ',
    #                                   max(selectedData()$length)),
    #                    value = round(mean(selectedData()$length), 2)
    # )
    # 
    # freezeReactiveValue(input, 'prediction_in_2')
    # 
    # 
    # updateNumericInput(session, 'prediction_in_3', 
    #                    label = paste0('Enter a length from ', 
    #                                   min(selectedData()$length), ' to ',
    #                                   max(selectedData()$length)),
    #                    value = round(mean(selectedData()$length), 2)
    # )
    # 
    # freezeReactiveValue(input, 'prediction_in_3')
    
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
    # shinyjs::show('prediction_in_2')
    # shinyjs::show('prediction_in_3')
    
    
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
    
    new_data <- data.frame(length = log(input$prediction_in_1)) # GOOD
    
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
  
  
  output$non_linear_prediction_out <- renderUI({
    
    req(input$prediction_in_1,
        getDataFor_non_linear())

    new_data <- data.frame(length = input$prediction_in_1)
    
    p_confidence <- predictNLS(getDataFor_non_linear(), new_data, interval = 'confidence')
    p_prediction <- predictNLS(getDataFor_non_linear(), new_data, interval = 'prediction')
    
    # cat('\n\n============================================\n')
    # cat('p_confidence\n')
    # print(p_confidence$summary)
    # cat('\n\np_prediction\n')
    # print(p_prediction$summary)
    # cat('\n============================================\n\n\n')

    PROP1 <- predictNLS(getDataFor_non_linear(), newdata = new_data, interval = 'prediction')
    PRED1 <- predict(getDataFor_non_linear(), newdata = new_data)
    nls_summary <- PROP1$summary
    # cat('\n------------------------\n')
    # cat('names(PROP1$summary)...\n')
    # print(nls_summary[5])
    # print(class(nls_summary[11]))
    # print(nls_summary[5][1])      # "Sim.2.5%"
    # print(class(nls_summary[11][1]))
    # print(nls_summary[6][1])      # "Sim.97.5%"
    # cat('\n------------------------\n')
    # cat('input$prediction_in_1 = ', input$prediction_in_1, '\n')
    # cat('              PRED1 = ', PRED1, '\n')
    # cat('\n------------------------\n\n')
    
    
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
  
  
  # predictionsData <- reactive ----
  
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
                         format='f', digits=1)
    
    my_wbar <- formatC(round(cf_baskerville * exp(p_vector), 3), 
                         format='f', digits=3)
    
    
    # prediction interval
    # matrix -> data.frame
    my_wbar <- as.data.frame.matrix(my_wbar,
    # my_wbar <- as.data.frame(as.table(my_wbar),  # NO!
                             stringsAsFactors = F)
    # narrow -> wide
    # my_wbar <- my_wbar %>% spread(Var2, Freq) 
    # %>% select(-Var1)
    
    
    range <- paste0(my_wbar$lwr, ' - ', my_wbar$upr)
    
    # coerce factor -> numeric
    my_wbar$fit <- as.numeric(my_wbar$fit)
    
    # compute count tails/lb, with 453.59237 g/lb & 61% tails
    c_tails_lb <- get_count_category(453.59237 / (0.61 * my_wbar$fit))
    
    # compute count tails/lb, with 453.59237 g/lb & 61% tails
    c_whole_kg <- get_count_category(1000.0 / as.numeric(my_wbar$fit))
    
    data <- data.frame(length     = my_length,
                       wbar       = my_wbar$fit,
                       range      = range,
                       c_tails_lb = c_tails_lb,
                       c_whole_kg = c_whole_kg,
                       
                       stringsAsFactors = F)
    
    data
    
  })
  
  
  # render Predictions DT ----
  
  output$linear_prediction_dt <- DT::renderDataTable({
    
    req(predictionsData())
    
    datatable( predictionsData(),
               
               colnames = c('length (mm)', 'wBar (g)', '95% prediction', 'tails/lb', 'whole/kg'),
               
               rownames = F,
               
               options = list(dom = 'tip',
                              
                              pageLength = 5,

                              columnDefs = list(list(className = 'dt-right',  targets = 0:1),
                                                list(className = 'dt-center', targets = 2:4))
               ) 
    )
    
  })
  
  
  
  # RESULTS output ----
  
  output$lw_non_linear_results <- renderUI({
    
    req(getDataFor_non_linear())
    
    nls_stats <- summary(getDataFor_non_linear())
    
    # print(names(nls_stats))
    # print(nls_stats)
    
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
  
  
  
  # IMAGE output ----
  
  output$shrimp_metrics <- renderImage({
    
    # filename <- normalizePath(file.path('www/Shrimp_Length_Graphic.tiff'))
    filename <- file.path('Shrimp_Length_Graphic.png')
    
    # list(src = 'Shrimp_Length_Graphic.tiff', alt = '?????')
    list(src = url('Shrimp_Length_Graphic.png'), alt = '?????')
    
  }, deleteFile = F)
  
  
  # '' ----
  # >> L-W GALLERY << ----
  
  # NB: @param length in which UNITS?----
  
  # wBar(*L) functions SHRIMP ----
  
  w_of_l_vann <- function(length) {
    return(10^(-5.197 + 3.037 * log10(length)))
  }
  
  
  w_of_l_monodon <- function(length) {
    return(10^(-1.811 + 2.721 * log10(length)))
  }
  
  
  w_of_l_semisulcatus <- function(length) {
    return(0.00196 * length^2.746)
  }
  
  
  w_of_l_duorarum <- function(length) {
    return(10^(-5.381 + 3.137 * log10(length)))
  }
  
  
  # wBar(*L) functions SALMON ----
  
  w_of_l_atlantic <- function(length) {
    return(10^(-4.47028 + 2.78090 * log10(length)))
  }
  
  w_of_l_chinook <- function(length) {
    return(10^(-5.31348 + 3.113913 * log10(length)))
  }
  
  w_of_l_coho <- function(length) {
    return(10^(-6.16900 + 3.42700 * log10(length)))
  }
  
  w_of_l_pink <- function(length) {
    return(10^(-4.737 + 2.877 * log10(length)))
  }
  
  
  
  # count-per-lb/kg sieve ----
  
  get_count_category <- function(rc) {
    
    ifelse(rc < 12, 'U-12', 
           ifelse(rc < 15, 'U-15', 
                  ifelse(rc <= 20, '16/20', 
                         ifelse(rc <= 25, '21/25', 
                                ifelse(rc <= 30, '26/30',
                                       ifelse(rc <= 30, '26/30',
                                              ifelse(rc <= 35, '31/35',
                                                     ifelse(rc <= 40, '36/40',
                                                            ifelse(rc <= 50, '41/50',
                                                                   ifelse(rc <= 60, '51/60',
                                                                          ifelse(rc <= 70, '61/70', '--'
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
                  )
           )
    )

  }
  
  
  # calc data SHRIMP ----
  
  # df vann - L & W data
  calc_vann_data <- reactive({
    
    # define length domain in MM
    l <- seq(10, 170)
    
    # calculate weight range
    w <- w_of_l_vann(l)
    
    # compute count tails/lb, with 453.59237 g/lb & 61% tails
    c <- 453.59237 / (0.61 * w)
    c <- get_count_category(c)
    
    # compute count whole/kg, with 453.59237 g/lb & 61% tails
    c_whole_kg <- get_count_category(1000.0 / w)
      
    # format weight & count
    # w <- as.numeric(formatC(round(w, 2), format='f', digits=2))
    w <- formatC(round(w, 2), format='f', digits=2)
    
    # gallery_lw_data$vann_lw_data <- data.frame(length  = l,
    #                                            weight  = w,
    #                                            count   = c,
    #                                            count_2 = c_whole_kg)
    #                                            # species = 'L. vannamei')
    
    
    # NB: AVOID Warning in bind_rows_(x, .id) : binding character and factor vector, coercing into character vector ----
    # see: https://stackoverflow.com/questions/30203299/bind-rows-in-dplyr-throwing-unusual-error
    # see: https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information/3418192#3418192
    
    # OR...use TIBBLE... ----
    gallery_lw_data$vann_lw_data <- tibble(length  = as.numeric(l),
                                           weight  = w,
                                           count   = c,
                                           count_2 = c_whole_kg)
  })
  
  
  # df monodon - L & W data ----
  calc_monodon_data <- reactive({
    
    # define length domain in CM
    l <- seq(1, 20)
    
    # calculate weight range
    w <- w_of_l_monodon(l)
    
    # compute count, with 453.59237 g/lb & 61% tails
    c <- 453.59237 / (0.61 * w)
    c <- get_count_category(c)
    
    # compute count whole/kg, with 453.59237 g/lb & 61% tails
    c_whole_kg <- get_count_category(1000.0 / w)
    
    # format weight & count
    # w <- as.numeric(formatC(round(w, 2), format='f', digits=2))
    w <- formatC(round(w, 2), format='f', digits=2)
    
    
    gallery_lw_data$monodon_lw_data <- tibble(length  = as.numeric(l),
                                              weight  = w,
                                              count   = c,
                                              count_2 = c_whole_kg)
  })
  
  
  # df semisulcatus - L & W data ----
  calc_semisulcatus_data <- reactive({
    
    # define length domain in MM (NB: CL)
    l <- seq(1, 45)
    
    # calculate weight range
    w <- w_of_l_semisulcatus(l)
    
    # compute count, with 453.59237 g/lb & 61% tails
    c <- 453.59237 / (0.61 * w)
    c <- get_count_category(c)
    
    # compute count whole/kg, with 453.59237 g/lb & 61% tails
    c_whole_kg <- get_count_category(1000.0 / w)
    
    # format weight & count
    # w <- as.numeric(formatC(round(w, 2), format='f', digits=2))
    w <- formatC(round(w, 2), format='f', digits=2)
    
    
    gallery_lw_data$semisulcatus_lw_data <- tibble(length  = as.numeric(l),
                                                   weight  = w,
                                                   count   = c,
                                                   count_2 = c_whole_kg)
  })
  
  
  # df duorarum - L & W data ----
  calc_duorarum_data <- reactive({
    
    # define length domain in MM
    l <- seq(30, 70)
    
    # calculate weight range
    w <- w_of_l_duorarum(l)
    
    # compute count, with 453.59237 g/lb & 61% tails
    c <- 453.59237 / (0.61 * w)
    c <- get_count_category(c)
    
    # compute count whole/kg, with 453.59237 g/lb & 61% tails
    c_whole_kg <- get_count_category(1000.0 / w)
    
    # format weight & count
    # w <- as.numeric(formatC(round(w, 2), format='f', digits=2))
    w <- formatC(round(w, 2), format='f', digits=2)
    
    
    gallery_lw_data$duorarum_lw_data <- tibble(length  = as.numeric(l),
                                               weight  = w,
                                               count   = c,
                                               count_2 = c_whole_kg)
  })
  
  
  # LBS & OZ ----
  lbs_to_oz_and_lbs <- function(my_lbs) {
    
    pounds <- floor(my_lbs)
    
    ounces <- round((my_lbs - pounds) * 16, 1)
    
    # Chinook at 566 mm: 566,	1813.64,	22.28,	**3 lbs 16 oz**
    pounds <- ifelse(16 == ounces, pounds + 1, pounds)
    ounces <- ifelse(16 == ounces,        0.0, ounces)
    
    formatC(ounces, format='f', digits=1)
    
    return(paste0(pounds, ' lbs ', ounces, ' oz'))
  }
  
  
  # calc data SALMON ----
  
  # df atlantic - L & W data ----
  calc_atlantic_data <- reactive({
    
    # define length domain in MM
    l <- seq(200, 600)
    
    # calculate weight range
    w <- w_of_l_atlantic(l)
    
    
    # compute inches from mm
    l_inches <- 0.039370079 * l
    
    # compute lbs/oz
    wbar_lbs <- 0.0022046226 * w
    
    l_inches <- formatC(round(l_inches, 2), format='f', digits=2)
    
    wbar_lbs <- lbs_to_oz_and_lbs(wbar_lbs)
    # wbar_lbs <- formatC(round(wbar_lbs, 2), format='f', digits=2)
    
    # format weight & count
    # w <- as.numeric(formatC(round(w, 2), format='f', digits=2))
    w <- formatC(round(w, 2), format='f', digits=2)
    
    gallery_lw_data$atlantic_lw_data <- tibble(length  = as.numeric(l),
                                               weight  = w,
                                               inches  = l_inches,
                                               lbs     = wbar_lbs)
    # species = 'L. vannamei')
  })
  
  # df chinook- L & W data ----
  calc_chinook_data <- reactive({
    
    # define length domain in MM
    l <- seq(200, 600)
    
    # calculate weight range
    w <- w_of_l_chinook(l)
    
    
    # compute inches from mm
    l_inches <- 0.039370079 * l
    
    # compute lbs/oz
    wbar_lbs <- 0.0022046226 * w
    
    l_inches <- formatC(round(l_inches, 2), format='f', digits=2)
    
    wbar_lbs <- lbs_to_oz_and_lbs(wbar_lbs)
    # wbar_lbs <- formatC(round(wbar_lbs, 2), format='f', digits=2)
    
    # format weight & count
    w <- formatC(round(w, 2), format='f', digits=2)
    
    gallery_lw_data$chinook_lw_data <- tibble(length  = as.numeric(l),
                                              weight  = w,
                                              inches  = l_inches,
                                              lbs     = wbar_lbs)
  })
  
  # df coho- L & W data ----
  calc_coho_data <- reactive({
    
    # define length domain in MM
    l <- seq(200, 600)
    
    # calculate weight range
    w <- w_of_l_coho(l)
    
    
    # compute inches from mm
    l_inches <- 0.039370079 * l
    
    # compute lbs/oz
    wbar_lbs <- 0.0022046226 * w
    
    l_inches <- formatC(round(l_inches, 2), format='f', digits=2)
    
    wbar_lbs <- lbs_to_oz_and_lbs(wbar_lbs)
    # wbar_lbs <- formatC(round(wbar_lbs, 2), format='f', digits=2)
    
    # format weight & count
    w <- formatC(round(w, 2), format='f', digits=2)
    
    gallery_lw_data$coho_lw_data <- tibble(length  = as.numeric(l),
                                           weight  = w,
                                           inches  = l_inches,
                                           lbs     = wbar_lbs)
  })
  
  # df pink- L & W data ----
  calc_pink_data <- reactive({
    
    # define length domain in MM
    l <- seq(200, 600)
    
    # calculate weight range
    w <- w_of_l_pink(l)
    
    
    # compute inches from mm
    l_inches <- 0.039370079 * l
    
    # compute lbs/oz
    wbar_lbs <- 0.0022046226 * w
    
    l_inches <- formatC(round(l_inches, 2), format='f', digits=2)
    
    wbar_lbs <- lbs_to_oz_and_lbs(wbar_lbs)
    # wbar_lbs <- formatC(round(wbar_lbs, 2), format='f', digits=2)
    
    # format weight & count
    w <- formatC(round(w, 2), format='f', digits=2)
    
    gallery_lw_data$pink_lw_data <- tibble(length  = as.numeric(l),
                                           weight  = w,
                                           inches  = l_inches,
                                           lbs     = wbar_lbs)
  })
  
  
  
  # DT - gallery ----
  
  output$length_weight_dt <-  DT::renderDataTable({
    
    req(gallery_lw_data_to_plot_shrimp(),
        gallery_lw_data_to_plot_salmon())
    
    
    if('Penaeids' == input$species_gallery) {
      data <- gallery_lw_data_to_plot_shrimp()
      my_col_names <- c(data$x_axis_label, 'wBar (g)', 'tails/lb', 'whole/kg')
    } else {
      data <- gallery_lw_data_to_plot_salmon()
      my_col_names <- c(data$x_axis_label, 'wBar (g)', 'inches', 'lbs & oz')
    }
    
    datatable( data$df,
               
               colnames = my_col_names,
               
               rownames = F,
               
               # selection = list(mode = 'multiple', selected = colnames(c(1, 3))),
               
               options = list(dom = 'tip',
                              
                              pageLength = 12,
                              
                              columnDefs = list(list(className = 'dt-right',  targets = 0:1),
                                                list(className = 'dt-center', targets = 2:3))
               ) 
    )
    
  })
  
  
  # SHRIMP data to display ----
  
  gallery_lw_data_to_plot_shrimp <- reactive({
    
    req(input$rb_gallery_spp_shrimp)
    
    spp_to_plot <- data.frame(length  = double(),
                              weight  = character(),
                              count   = character(),
                              count_2 = character(),
                              
                              stringsAsFactors = F)
    
    
    if('L. vannamei' == input$rb_gallery_spp_shrimp) {
      
      # current_spp_data <- calc_vann_data()
      spp_to_plot <- bind_rows(spp_to_plot, calc_vann_data())
      
      
      spp_and_formula <- '<i>L. vannamei</i>: <b>W = 0.000006353309 TL<sup>3.037</sup></b>'
      reference <- 'Hutchins <i>et al.</i> 1979. Length-weight Relations for Several Species of 
      Penaeid Shrimp Cultured in Ponds Near Corpus Christi, Texas J. World Aquaculture Soc. 10 (1-4): 565–570'
      
      chart_title <- 'L. vannamei'
      
      l_metric <- 'Total Length'
      l_units  <- 'mm'
      w_metric <- 'Weight'
      w_units  <- 'g/ind'
      
      x_axis_label <- paste0(l_metric, ' (', l_units, ')')
      y_axis_label <- paste0(w_metric, ' (', w_units, ')')
    }
    
    else if('P. monodon' == input$rb_gallery_spp_shrimp) {
      
      # current_spp_data <- calc_monodon_data()
      spp_to_plot <- bind_rows(spp_to_plot, calc_monodon_data())
      
      spp_and_formula <- '<i>P. monodon</i>: <b>W = 0.01545254 BL<sup>2.721</sup></b>'
      reference <- 'Gopalakrishnan <i>et al.</i> 2014. Length–weight relationship and condition factor of wild, grow-out and ‘loose-shell affected’ giant tiger shrimp, 
      <i>Penaeus monodon</i> J. Applied Ichthiology 30(1): 251–253'
      
      chart_title <- 'P. monodon'
      
      l_metric <- 'Body Length'
      l_units  <- 'cm'
      w_metric <- 'Weight'
      w_units  <- 'g/ind'
      
      x_axis_label <- paste0(l_metric, ' (', l_units, ')')
      y_axis_label <- paste0(w_metric, ' (', w_units, ')')
    }
    
    else if('P. semisulcatus' == input$rb_gallery_spp_shrimp) {
      
      # current_spp_data <- calc_semisulcatus_data()
      spp_to_plot <- bind_rows(spp_to_plot, calc_semisulcatus_data())
      
      spp_and_formula <- '<i>P. semisulcatus</i>: <b>W = 0.00196 CL<sup>2.746</sup></b>'
      reference <- 'Matthews, C. P. and M. Samuels. 1991. Growth, Mortality, and Length-Weight Parameters for Some Kuwaiti Fish and Shrimp FishByte 30(1): 30-33'
      
      chart_title <- 'P. semisulcatus'
      
      l_metric <- 'Carapace Length'
      l_units  <- 'mm'
      w_metric <- 'Weight'
      w_units  <- 'g/ind'
      
      x_axis_label <- paste0(l_metric, ' (', l_units, ')')
      y_axis_label <- paste0(w_metric, ' (', w_units, ')')
    }
    
    else if('P. duorarum' == input$rb_gallery_spp_shrimp) {
      
      # current_spp_data <- calc_duorarum_data()
      spp_to_plot <- bind_rows(spp_to_plot, calc_duorarum_data())
      
      spp_and_formula <- '<i>P. duorarum</i>: <b>W = 0.000004159106 TL<sup>3.137</sup></b>'
      reference <- 'Hutchins <i>et al.</i> 1979. Length-weight Relations for Several Species of 
      Penaeid Shrimp Cultured in Ponds Near Corpus Christi, Texas J. World Aquaculture Soc. 10 (1-4): 565–570'
      
      chart_title <- 'P. duorarum'
      
      l_metric <- 'Total Length'
      l_units  <- 'mm'
      w_metric <- 'Weight'
      w_units  <- 'g/ind'
      
      x_axis_label <- paste0(l_metric, ' (', l_units, ')')
      y_axis_label <- paste0(w_metric, ' (', w_units, ')')
    }
    
    # ----------------------------------*
    
    # w <- current_spp_data$weight
    # 
    # # compute count, with 453.59237 g/lb & 61% tails
    # c <- 453.59237 / (0.61 * w)
    # c <- get_count_category(c)
    # 
    # # compute count whole/kg, with 453.59237 g/lb & 61% tails
    # c_whole_kg <- get_count_category(1000.0 / w)
    # 
    # # format weight & count
    # w <- as.numeric(formatC(round(w, 2), format='f', digits=2))
    # 
    # 
    # gallery_lw_data$duorarum_lw_data <- data.frame(length  = l,
    #                                                weight  = w,
    #                                                count   = c,
    #                                                count_2 = c_whole_kg)
    # 
    # spp_to_plot <- bind_rows(spp_to_plot, current_spp_data)
    
    # ----------------------------------*
    
    
    spp_data <- list(df = spp_to_plot,
                     spp_and_formula = spp_and_formula,
                     reference       = reference,
                     chart_title     = chart_title,
                     x_axis_label    = x_axis_label,
                     y_axis_label    = y_axis_label,
                     l_metric        = l_metric,
                     l_units         = l_units,
                     w_metric        = w_metric,
                     w_units         = w_units
                     )
      
  })
  
  
  
  # SALMON data to display ----
  
  gallery_lw_data_to_plot_salmon <- reactive({
    
    req(input$rb_gallery_spp_salmon)
    
    spp_to_plot <- data.frame(length  = double(),
                              weight  = character(),
                              inches  = character(),
                              lbs     = character(),
                              
                              stringsAsFactors = F)
    
    
    if('Atlantic' == input$rb_gallery_spp_salmon) {
      
      # current_spp_data <- calc_vann_data()
      spp_to_plot <- bind_rows(spp_to_plot, calc_atlantic_data())
      
      spp_and_formula <- '<i>Salmo salar</i>: <b>W = 0.00003386258 TL<sup>2.78090</sup></b>'
      reference <- 'Dexter, J. L., Jr. 1991. Gull Lake as a broodstock lake for landlocked Atlantic salmon (Salmo
salar). Michigan Department of Natural Resources, Fisheries Technical Report 91-8, Ann
Arbor'
      
      chart_title <- 'Atlantic Salmon'
      
      l_metric <- 'Total Length'
      l_units  <- 'mm'
      w_metric <- 'Weight'
      w_units  <- 'g/ind'
      
      x_axis_label <- paste0(l_metric, ' (', l_units, ')')
      y_axis_label <- paste0(w_metric, ' (', w_units, ')')
    }
    
    else if('Chinook' == input$rb_gallery_spp_salmon) {
      
      # current_spp_data <- calc_monodon_data()
      spp_to_plot <- bind_rows(spp_to_plot, calc_chinook_data())
      
      spp_and_formula <- '<i>Oncorhynchus tshawytscha</i>: <b>W = 0.000004858699 BL<sup>3.113913</sup></b>'
      reference <- 'Wesley, J. 1996. Age and growth of chinook salmon from the eastern Lake Michigan sport fishery.
M. S. thesis, University of Michigan, Ann Arbor.'
      
      chart_title <- 'Chinook Salmon'
      
      l_metric <- 'Total Length'
      l_units  <- 'mm'
      w_metric <- 'Weight'
      w_units  <- 'g/ind'
      
      x_axis_label <- paste0(l_metric, ' (', l_units, ')')
      y_axis_label <- paste0(w_metric, ' (', w_units, ')')
    }
    
    else if('Coho' == input$rb_gallery_spp_salmon) {
      
      # current_spp_data <- calc_semisulcatus_data()
      spp_to_plot <- bind_rows(spp_to_plot, calc_coho_data())
      
      spp_and_formula <- '<i>Oncorhynchus kisutch</i>: <b>W = 0.0000006776415 CL<sup>3.42700</sup></b>'
      reference <- 'Rakoczy, G. P. 1996. Unpublished data on length and weight from Michigan\'s Great Lakes sport
      fishery survey, various years. Charlevoix Fisheries Research Station.'
      
      chart_title <- 'Coho Salmon'
      
      l_metric <- 'Total Length'
      l_units  <- 'mm'
      w_metric <- 'Weight'
      w_units  <- 'g/ind'
      
      x_axis_label <- paste0(l_metric, ' (', l_units, ')')
      y_axis_label <- paste0(w_metric, ' (', w_units, ')')
    }
    
    else if('Pink' == input$rb_gallery_spp_salmon) {
      
      # current_spp_data <- calc_duorarum_data()
      spp_to_plot <- bind_rows(spp_to_plot, calc_pink_data())
      
      spp_and_formula <- '<i>Oncorhynchus gorbuscha</i>: <b>W = 0.00001832314 TL<sup>2.877</sup></b>'
      reference <- 'Wagner, W. C. 1985. Size, age, and fecundity of pink salmon in Michigan. Michigan Department of
Natural Resources, Fisheries Research Report 1933, Ann Arbor.'
      
      chart_title <- 'Pink Salmon'
      
      l_metric <- 'Total Length'
      l_units  <- 'mm'
      w_metric <- 'Weight'
      w_units  <- 'g/ind'
      
      x_axis_label <- paste0(l_metric, ' (', l_units, ')')
      y_axis_label <- paste0(w_metric, ' (', w_units, ')')
    }
    
    
    spp_data <- list(df = spp_to_plot,
                     spp_and_formula = spp_and_formula,
                     reference       = reference,
                     chart_title     = chart_title,
                     x_axis_label    = x_axis_label,
                     y_axis_label    = y_axis_label,
                     l_metric        = l_metric,
                     l_units         = l_units,
                     w_metric        = w_metric,
                     w_units         = w_units
    )
    
  })
  
  
  # updateNumericInput Pred Gallery ----
  # NB: need dynamic units -- 'mm', 'cm'
  # observeEvent(c(input$rb_gallery_spp_shrimp, input$species_gallery), {
  observeEvent(c(input$species_gallery,
                 input$rb_gallery_spp_shrimp,
                 input$rb_gallery_spp_salmon), {
    
    req(gallery_lw_data_to_plot_shrimp(),
        gallery_lw_data_to_plot_salmon())
    
    if('Penaeids' == input$species_gallery) {
      data <- gallery_lw_data_to_plot_shrimp()
    } else {
      data <- gallery_lw_data_to_plot_salmon()
    }
    
    units <- data$x_axis_label
    my_label <- paste0('Enter ', units, ': ',
                       min(data$df$length), ' - ',
                       max(data$df$length))
    
    updateNumericInput(session, 'prediction_in_gallery', 
                       label = my_label,
                       value = round(mean(data$df$length), 2)
    )
    
    freezeReactiveValue(input, 'prediction_in_gallery')
    
  })
  
  
  # display reference & info ----
  
  output$gallery_ref_and_info <- renderUI({
    
    if('Penaeids' == input$species_gallery) {
      data <- gallery_lw_data_to_plot_shrimp()
    } else {
      data <- gallery_lw_data_to_plot_salmon()
    }
    
    str_1 <- data$spp_and_formula
    str_2 <- data$reference
    
    HTML(paste0(str_1, '<br>', str_2))
    
  })
  
  
  # display gallery prediction ----
  
  output$linear_prediction_out_gallery <- renderUI({
    
    req(input$prediction_in_gallery,
        gallery_lw_data_to_plot_shrimp(),
        gallery_lw_data_to_plot_salmon())
    
    if('Penaeids' == input$species_gallery) {
      
      # str_1 <- gallery_lw_data_to_plot_shrimp()$spp_and_formula
      # str_2 <- gallery_lw_data_to_plot_shrimp()$reference
      
      if('L. vannamei' == input$rb_gallery_spp_shrimp)
        ans <- w_of_l_vann(input$prediction_in_gallery)
      else if('P. monodon' == input$rb_gallery_spp_shrimp)
        ans <- w_of_l_monodon(input$prediction_in_gallery)
      else if('P. semisulcatus' == input$rb_gallery_spp_shrimp)
        ans <- w_of_l_semisulcatus(input$prediction_in_gallery)
      else if('P. duorarum' == input$rb_gallery_spp_shrimp)
        ans <- w_of_l_duorarum(input$prediction_in_gallery)
      
      # compute count tails/lb, with 453.59237 g/lb & 61% tails
      c <- 453.59237 / (0.61 * ans)
      c <- get_count_category(c)
      
      # compute count whole/kg, with 453.59237 g/lb & 61% tails
      c_whole_kg <- get_count_category(1000.0 / ans)
      
      ans <- formatC(round(ans, 2), format='f', digits=2)
      
      str1 <- paste0(tags$strong(input$prediction_in_gallery), ' ', tags$strong(gallery_lw_data_to_plot_shrimp()$l_units),
                     ' (', gallery_lw_data_to_plot_shrimp()$l_metric, ')<br><br>')
      str2 <- paste0(tags$strong(ans), ' ', tags$strong(gallery_lw_data_to_plot_shrimp()$w_units), '<br><br>')
      str3 <- paste0(tags$strong(c), ' ', tags$strong('tails/lb'), '<br><br>')
      str4 <- paste0(tags$strong(c_whole_kg), ' ', tags$strong('whole/kg'))
      
    } else {
      
      if('Atlantic' == input$rb_gallery_spp_salmon)
        ans <- w_of_l_atlantic(input$prediction_in_gallery)
      else if('Chinook' == input$rb_gallery_spp_salmon)
        ans <- w_of_l_chinook(input$prediction_in_gallery)
      else if('Coho' == input$rb_gallery_spp_salmon)
        ans <- w_of_l_coho(input$prediction_in_gallery)
      else if('Pink' == input$rb_gallery_spp_salmon)
        ans <- w_of_l_pink(input$prediction_in_gallery)
      
      my_inches     <- round(input$prediction_in_gallery * 0.039370079, 2)
      my_lbs_and_oz <- lbs_to_oz_and_lbs(ans * 0.0022046226)
      
      ans <- formatC(round(ans, 2), format='f', digits=2)
      
      str1 <- paste0(tags$strong(input$prediction_in_gallery), ' ', tags$strong(gallery_lw_data_to_plot_salmon()$l_units),
                     ' (', gallery_lw_data_to_plot_salmon()$l_metric, ')<br><br>')
      str2 <- paste0(tags$strong(ans), ' ', tags$strong(gallery_lw_data_to_plot_salmon()$w_units), '<br><br>')
      # str3 <- ''
      # str4 <- ''
      str3 <- paste0(tags$strong(my_inches), ' ', tags$strong('inches'), '<br><br>')
      str4 <- paste0(tags$strong(my_lbs_and_oz))
    }
    
    # if('L. vannamei' == input$rb_gallery_spp_shrimp)
    #   ans <- w_of_l_vann(input$prediction_in_gallery)
    # else if('P. monodon' == input$rb_gallery_spp_shrimp)
    #   ans <- w_of_l_monodon(input$prediction_in_gallery)
    # else if('P. semisulcatus' == input$rb_gallery_spp_shrimp)
    #   ans <- w_of_l_semisulcatus(input$prediction_in_gallery)
    # else if('P. duorarum' == input$rb_gallery_spp_shrimp)
    #   ans <- w_of_l_duorarum(input$prediction_in_gallery)
    # 
    # 
    # # compute count tails/lb, with 453.59237 g/lb & 61% tails
    # c <- 453.59237 / (0.61 * ans)
    # c <- get_count_category(c)
    # 
    # # compute count whole/kg, with 453.59237 g/lb & 61% tails
    # c_whole_kg <- get_count_category(1000.0 / ans)
    # 
    # 
    # ans <- formatC(round(ans, 2), format='f', digits=2)
    # 
    # str1 <- paste0(tags$strong(input$prediction_in_gallery), ' ', tags$strong(gallery_lw_data_to_plot_shrimp()$l_units),
    #                ' (', gallery_lw_data_to_plot_shrimp()$l_metric, ')<br><br>')
    # str2 <- paste0(tags$strong(ans), ' ', tags$strong(gallery_lw_data_to_plot_shrimp()$w_units), '<br><br>')
    # str3 <- paste0(tags$strong(c), ' ', tags$strong('tails/lb'), '<br><br>')
    # str4 <- paste0(tags$strong(c_whole_kg), ' ', tags$strong('whole/kg'))
    
    
    tags$h4(HTML(paste0(str1, str2, str3, str4)))
    
  })
  
  
  # PLOT gallery data ----
  
  output$l_w_plot_gallery <- renderPlot({
    
    if('Penaeids' == input$species_gallery) {
      data <- gallery_lw_data_to_plot_shrimp()
    } else {
      data <- gallery_lw_data_to_plot_salmon()
    }
    
    data$df %>% ggplot(aes(length, as.numeric(weight))) + 
      geom_line() +
      labs(y = data$y_axis_label, 
           x = data$x_axis_label, 
           title = data$chart_title)
    
  })
  
}