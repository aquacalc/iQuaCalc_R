# pH module functions for
# "iQuaCalc (Lite) salinity module.R"


phModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column( width = 11,
              sliderInput(ns('phSlider_convert'), 'pH (NBS)', 
                          min = 5.7, max = 10.5, value = 7.5, step = 0.01)
      )
    )
    
  )
}



phModule <- function(input, output, session, icTemp, icSal, st) {
# phModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(ph_sl_init = -1,
                       ph_default = 7.5)
  
  
  # Observe SLIDER_INPUT input, store when changed
  # observeEvent(input$phSlider_convert, {
  #   
  #   cat('observeEvent(input$phSlider_convert...\n')
  #   
  #   if(rv$ph_sl_init < 0) {
  # 
  #     rv$ph_sl_init <- 1
  #     
  #     x <- session$ns('sl_phSlider_convert')
  # 
  #     my_ph_value <- st()[[x]]
  # 
  #     if(length(my_ph_value) == 0)
  #       my_ph_value <- rv$ph_default
  # 
  #     updateSliderInput(session, 'phSlider_convert', 'pH (NBS)',
  #                 min = 5.7, max = 10.5, value = my_ph_value, step = 0.01)
  # 
  #     updateStore(session, x, my_ph_value)
  # 
  #     return()
  #   }
  #   
  #   y <- session$ns('sl_phSlider_convert')
  #   
  #   my_ph_value <- st()[[y]]
  #   
  #   if(length(my_ph_value) == 0)
  #     my_ph_value <- rv$ph_default
  #   else
  #     my_ph_value <- input$phSlider_convert
  #     # my_ph_value <- isolate(input$phSlider_convert)
  #   
  #   # update slider value for current units
  #   updateStore(session, y, my_ph_value)
  #   
  #   # freezeReactiveValue(input, "phSlider_convert")
  #   
  # })
  
  
  
  observeEvent(input$phSlider_convert, {

    if(rv$ph_sl_init < 0) {
      
      rv$ph_sl_init <- 1
      
      x <- session$ns('sl_phSlider_convert')
      
      my_ph_value <- st()[[x]]
      
      if(length(my_ph_value) == 0)
        my_ph_value <- rv$ph_default
      
      updateSliderInput(session, 'phSlider_convert', 'pH (NBS)',
                        min = 5.7, max = 10.5, value = my_ph_value, step = 0.01)
      
      freezeReactiveValue(input, "phSlider_convert")
      
      updateStore(session, x, my_ph_value)
      
      return()
    }
    
    x <- session$ns('sl_phSlider_convert')
    
    updateStore(session, x, input$phSlider_convert)
    
    # freezeReactiveValue(input, "phSlider_convert")
    
  })
  
  
  
  ph_current <- reactive({
    
    req(
      icTemp(), 
      icSal(), 
      cancelOutput = T
    )
    
    
    sal <- icSal()$ic
    temp <- icTemp()$ic
    
    ph <- input$phSlider_convert
    
    # ...convert to FREE for CarbCalc methods?
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

