# conversion_display_module.R
# when user clicks a cell in a conversion table,
# display the appropriate conversion result in a box


conversionDisplayModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
      
      box(
        style = "padding: 3px 2px 1px 2px;",
        width = NULL,
        # title = "Click a table cell to display a conversion",
        solidHeader = T,
        status = 'primary',
        htmlOutput(ns('conversion_result')),
        background = 'light-blue'
      )
    
  ) # END tagList
  
} # END module UI function, conversionDisplayInput



conversionDisplayModule <- function(input, output, session, 
                                    inputVal, inputUnits,     # the value & units converting FROM
                                    df, info,                 # dataframe of all conversions & list of clicked cell coordinates
                                    cell_selected,            # matrix of coordinates of selected cell
                                    other_data,               # string of any relevant factors, such as pH (NBS) for UIA-N conversion
                                    st) {
  
  
  
  clicked_cell_data <- eventReactive(c(cell_selected(), 
                                       df()), {
    
    my_str <- 'click a value in a table cell to display a conversion'
    
    # cell_coords <- input$ammonia_convert_dt_cells_selected
    # info <- input$ammonia_convert_dt_cell_clicked
    
                                         
    if(is.null(info()$col)) {
      
      str <- my_str
      
    } else {
      
      num_cols <- ncol(df()$df) - 1   # "-1" because DT is 0-indexed javascript
      
      # if((info()$col + 1) != 5) {
      if((info()$col + 1) != num_cols) {
        
        cell_datum <-  df()$df[info()$row, info()$col + 1]
        
        cell_units <- paste0(df()$df[info()$row, num_cols], ' ', 
                             colnames(df()$df)[info()$col + 1])
        
        
        # cheap-and-very-dirty...
        # in_ammonia_nitrogen <- colnames(df()$df)[info()$col + 1]
        # if('UIAN' == in_ammonia_nitrogen)
        #   cell_units <- paste0(cell_units, 'UIA-N')
        # if('TAN' == in_ammonia_nitrogen)
        #   cell_units <- paste0(cell_units, 'TA-N')
        
        # cat('\nin conversion_display_module.R/clicked_cell_data()...\n')
        # cat('0. cell_datum = ', cell_datum, '\n')
        # cat('0. cell_units = ', cell_units, '\n')
        # print(colnames(df()$df)[info()$col + 1])
        
        # see: https://www.rdocumentation.org/packages/stringr/versions/1.1.0/topics/str_sub
        x <- "UIAN"
        y <- 'TAN'
        to_N <- 'A-'
        
        if(TRUE == grepl(x, cell_units) || TRUE == grepl(y, cell_units)) {
          
          str_sub(cell_units, -2, -2) <- to_N
        }
        
        # cat('1. cell_units = ', cell_units, '\n')
        # cat('==========\n\n')
        
        
        str <- paste0(cell_datum, ' ', cell_units)
        
        # cat('             str = ', str, '\n')
        # cat('word(str, 1, -2) = ', word(str, 1, -2), '\n')
        # cat('==========\n\n')
        
        str
        
      } else {
        
        str <- my_str
      }
    }
    
    str
    
  })
  
  
  output$conversion_result <- renderUI({
    
    str1 <- paste0(inputVal(),' ',
                   inputUnits(), ' = ')
    
    str2 <- clicked_cell_data()
    
    # cat('in conversion_display_module.R...', grepl('click', str2), '\n')
    # cat('                     other_data()', other_data(), '\n')
    
    if(TRUE == grepl('click', str2)) {
      
      HTML(paste(tags$h5(str2, style = "text-align: center;")))
      
    } else {
      
      if(!is.null(other_data())) {
        
        result_str <- word(str2, 1, -2)
        
        if(TRUE == grepl('UIA-N', str2) || 
           TRUE == grepl('TA-N', str2) || 
           TRUE == grepl('TA', str2) ||
           TRUE == grepl('UIA', str2)) {
          
          result_str <- str2
        }
           
        
        HTML(paste(tags$h4(str1, result_str, other_data(),
        # HTML(paste(tags$h4(str1, word(str2, 1, -2), other_data(),
                           
                           # see: https://shiny.rstudio.com/articles/css.html
                           style = "text-align: center;")
                   )
             )
        
      } else {
        
        # cat('str1 = ', str1, '\n')
        # cat('str2 = ', str2, '\n')
        # cat('str2 = ', word(str2, 1, -2), '\n')
        
        HTML(paste(tags$h4(str1, word(str2, 1, -2), tags$br(),    # last string, 'tags$br()', adds blank line
                           
                           # see: https://shiny.rstudio.com/articles/css.html
                           style = "text-align: center;")
                   )
             )
      }
    }
    
  })
  
}