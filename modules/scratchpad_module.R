# Experimental Scratchpad
# for one text- (voice-) recognized input
# of the form ('5.43 m to ft')


scratchpadModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    tags$script('
                  $(document).on("keypress", function (e) {
                  Shiny.onInputChange("my_scratchpad-mydata_keypress", e.which);
                  });
                  '),
    
    fluidRow(
      
      column(width = 8,
             
             splitLayout(cellWidths = c('25%', '75%'),
                         
                         radioButtons(inputId = ns('rb_type'),
                                      label   = 'Choose a Conversion Type',
                                      choices = c('length', 'area', 'volume')),
                         
                         textAreaInput(inputId     = ns('my_scratch_input'),
                                       label       = 'Enter conversion of the form "5.3 m to ft"',
                                       placeholder = 'Enter one conversion per line')
             ),
             
             actionButton(inputId = ns('fire_conversion'),
                          label   = 'Convert!')
             
      ), # END column #1
      
      column(width = 4,
             tags$h4('What goes here? Explanation?', align = 'center'),
             
             htmlOutput(ns('conversion_result'))
      )
      
    )  # END fluidRow
  )  # END tagList
  
}


scratchpadModule <- function(input, output, session, st) {
  
  
  my_conversion <- eventReactive(input$mydata_keypress, {
  # my_conversion <- observeEvent(input$mydata_keypress, {
  # my_conversion <- eventReactive(input$mydata_keypress == 13, {
  # my_conversion <- eventReactive(c(input$mydata_keypress, input$fire_conversion), {
  # my_conversion <- reactive({
    
    # cat('\nin scratchpad_module.R, my_conversion...', input$mydata_keypress, '\n')
    
    if(input$mydata_keypress != 13) return()
    
    my_str <- input$my_scratch_input
    type   <- input$rb_type
    
    cat('input$my_scratch_input = ', my_str, '\n')
    cat('         input$rb_type = ', type, '\n')
    
    
    # [NB] isolate last entered line...
    # find last return ('\n)
    r <- nrow(str_locate_all(my_str, '\\n')[[1]])
    cat('\n r = ', r, '\n')
    
    if(r < 2) {
      
      # use that value to get location of last return, and shift one character right
      start_from <- str_locate_all(my_str, '\\n')[[1]][r] + 1
      last_entry <- str_sub(my_str, start_from, -1L)
      cat('\n\n==========================\n')
      cat('last_entry = ', last_entry, '\n')
      cat('==========================\n\n')
    } else {
      
      cat('First Entry \n')
    }
    
    
    num_newlines <- nrow(str_match_all(my_str, '\\n')[[1]]) + 1
    
    if(is.na(num_newlines)) {
      
      # num_newlines <- 1
      
      my_value <- str_extract_all(my_str, "([-+]?[\\d\\.]+)")[[1]]
      my_units <- str_extract_all(my_str, '([^\\d\\.\\+\\-])+')[[1]]
      
    } else {
      
      my_value <- str_extract_all(my_str, "([-+]?[\\d\\.]+)")[[1]][num_newlines]
      my_units <- str_extract_all(my_str, '([^\\d\\.\\+\\-])+')[[1]][num_newlines]
    }
    
    
    coef  <- as.numeric(my_value)
    
    cat('    coef = ', coef, '\n')
    cat('my_value = ', my_value, '\n')
    cat('my_units = ', my_units, '\n')
    
    
    # split on space(s)
    all_units <- str_split(trimws(my_units, 'both'), ' ')[[1]]
    all_units
    
    cat('all_units = ', all_units, '\n')
    
    # number of elements -- want 1st (FROM units) & last (TO units)
    num_unit_elements <- length(all_units)
    num_unit_elements
    
    from_units <- all_units[1]
    to_units   <- all_units[num_unit_elements]
    
    # see: https://stackoverflow.com/questions/9057006/getting-strings-recognized-as-variable-names-in-r
    type_data <- eval(as.name(paste0(type, '.data')))
    
    cat('from_units: ', from_units, '\n')
    cat('  to_units: ', to_units, '\n\n')
    
    # NB: identify type of from_units -- How?; then limit the set of to_units
    #     if not compatible, then raise an easily correctible error message...
    
    # FIRE on button?!
    ans <- convert(coef, from_units, to_units, type_data)
    
    cat('Convert: ', coef, ' ', from_units, ' -> ', ans, ' ', to_units, '\n\n\n')
    
    ans_str <- paste0(coef, ' ', from_units, ' -> ', ans, ' ', to_units)
    
    ans_str
    
  })
  
  
  output$conversion_result <- renderUI({
    
    HTML(paste0(tags$h4(my_conversion())))
    
  })
   
}