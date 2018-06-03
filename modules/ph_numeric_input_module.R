# pH module functions for
# "iQuaCalc (Lite) salinity module.R"


phNumericModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column( width = 12,
              
              numericInput(ns('ph_numeric_input'), 'pH (NBS)', 
                           min = 5.7, max = 10.5, 
                           value = 7.5, step = 0.01)
      )
    )
    
  )
}



phNumericModule <- function(input, output, session, icTemp, icSal, st) {
  
# phModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(ph_sl_init = -1,
                       ph_default = 7.5)
  
  
  observeEvent(input$ph_numeric_input, {

    if(rv$ph_sl_init < 0) {
      
      rv$ph_sl_init <- 1
      
      x <- session$ns('sl_ph_numeric_input')
      
      my_ph_value <- st()[[x]]
      
      if(length(my_ph_value) == 0)
        my_ph_value <- rv$ph_default
      
      updateNumericInput(session, 'ph_numeric_input', label = 'pH (NBS)',
                         min = 5.7, max = 10.5, 
                         value = my_ph_value, 
                         step = 0.01)
      
      freezeReactiveValue(input, "ph_numeric_input")
      
      updateStore(session, x, my_ph_value)
      
      return()
    }
    
    x <- session$ns('sl_ph_numeric_input')
    
    updateStore(session, x, input$ph_numeric_input)
    
    # freezeReactiveValue(input, "ph_numeric_input")
    
  })
  
  
  
  ph_current <- reactive({
    
    # req(
    #   icTemp(), 
    #   icSal(), 
    #   input$ph_numeric_input >= 5.7 && input$ph_numeric_input <= 10.5,
    #   cancelOutput = T
    # )
    
    validate(
      need(try(
        
        input$ph_numeric_input >= 5.7 && 
        input$ph_numeric_input <= 10.5
               ),
        
           "Please enter pH between 5.7 and 10.5")
    )
    
    
    sal <- icSal()$ic
    temp <- icTemp()$ic
    
    ph <- input$ph_numeric_input
    
    # ...cnvert to FREE for CarbCalc methods?
    icPh <- phNbsToPhFree(ph, sal, temp, 0)
    
    # as in "salinity_module.R," not just "as.data.frame(df)"
    ph_list <- list(df = NULL, 
                    ic = icPh,
                    val = ph, 
                    units = '(NBS)')
    
    ph_list
    
  })
  
  
  return(ph_current)
  
}

