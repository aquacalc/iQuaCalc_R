# module for iQuaCalc dashboard


disinfectionModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 5,
             
             fluidRow(
               
               column(width = 5,
                      
                      numericInput(inputId = ns('totVolToDisinfect'), label = 'Volume to Disinfect',
                                   value = 40, min = 0, max = 1000, step = 0.5)
               ),
               
               column(width = 7,
                      
                      selectInput(inputId = ns('disinfectUnitsTot'), label = 'Volume Units',
                                  choices = volume.data$units, selected = 'cubic meters (m\U00B3)')
                 
               )
             ),
             
             hr(),
             
             selectInput(inputId = ns('disinfectUnitsToAdd'), label = 'Volume Units to Add',
                         choices = volume.data$units, selected = 'liters (L)')
      ),
      
      column( width = 7,
              
              box(
                width = NULL,
                title = 'Disinfectant Solutions',
                solidHeader = T,
                status = 'primary',
                
                sliderInput(ns("poStoSlider"), 
                            'Stock Solution (%) -- concentration of stored solution', 1, 75, 35, 0.1),
                hr(),
                
                sliderInput(ns("ppmSlider"), 
                            'Working Solution (ppm) -- desired final concentration', 0, 500, 10, 0.5)
                
              )    # ---- end box
      )            # ---- end column(8)
    ),             # ---- end fluidRow 1
    
    fluidRow(
      
      box(
        width = NULL,
        title = "Disinfectant \'Recipe\'",
        solidHeader = T,
        status = 'primary',
        htmlOutput(ns('result')),
        background = 'light-blue'
      )
    )    # /fluidRow 2
  )      # /tagList
}



disinfectionModule <- function(input, output, session) {
  
  volDisinfectantToAdd <- reactive ({
    # volume to add in ml/L
    volInMlPerL <- input$ppmSlider / (10 * input$poStoSlider)
    
    # calculate L (I.C. units) to add
    totalToDisinfectInLiters <- convert(input$totVolToDisinfect,
                                        input$disinfectUnitsTot,
                                        'liters (L)', volume.data)
    
    # convert total mL to add to selected volume units to add
    totDisinfectantToAdd <- convert(totalToDisinfectInLiters * volInMlPerL,
                                    'milliliters (ml)',
                                    input$disinfectUnitsToAdd, volume.data)
    
    totDisinfectantToAdd
  })
 
  
  output$result <- renderUI({
    
    # str1 <- paste0('ADD ',round(volDisinfectantToAdd(),2),' ',input$disinfectUnitsToAdd,
    #                ' of ',input$poStoSlider,'% stock solution')
    # str2 <- paste0('to ',input$totVolToDisinfect,' ',input$disinfectUnitsTot)
    # str3 <- paste0('to get a disinfectant concentration of ',input$ppmSlider,' ppm')
    
    str1 <- paste0('To disinfect ',input$totVolToDisinfect,' ',input$disinfectUnitsTot)
    str2 <- paste0('with a disinfectant concentration of ',input$ppmSlider,' ppm')
    str3 <- paste0('ADD ',round(volDisinfectantToAdd(),2),' ',input$disinfectUnitsToAdd,
                   ' of a ',input$poStoSlider,'% stock solution')
    
    HTML(paste(tags$h2(str1), tags$h2(str2), tags$h2(str3), tags$br(), sep = '<br/>')) 
  })
  
}