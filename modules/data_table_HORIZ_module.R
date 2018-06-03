# datatable module functions for 
# ... HORIZONTALLY ... dispalyed data
# ex: TGP table
# "iQuaCalc (Lite) salinity module.R"


datatable_HORIZ_ModuleInput <- function(id, col_width, font_size) {
  
  ns <- NS(id)
  
  tagList(
    
    column(width = col_width,
           div(DT::dataTableOutput(ns('dt')), 
               style = paste0('font-size:', font_size, '%'))
    )
  )
}



datatable_HORIZ_Module <- function(input, output, session, my_df, my_rows) {
  
  
  dummy <- reactive({
    
    req(my_df())
    
    my_df()$df
    
  })
  
  
  proxy_dt_data = dataTableProxy(session$ns('dt'))
  # proxy_dt_data <- dataTableProxy('dt')
  
  # NB: replaceData doesn't work in module namespace
  # see: https://github.com/rstudio/DT/issues/359 for workaround 
  # observeEvent(dummy(), {
  observe({
    # cat('\n\n---- EVO! ----\n')
    # print(my_df()$df)
    # cat('\n---- EVO! ----\n\n')
    
    # replaceData(proxy_dt_data, dummy(), rownames = F, resetPaging = FALSE)
    
    # dataTableAjax(session, my_df()$df, rownames = F, outputId = 'dt')
    dataTableAjax(session, dummy(), rownames = F, outputId = 'dt')
    reloadData(proxy_dt_data, resetPaging = FALSE)
    
    # Sys.sleep(2)
  })
  
  
  output$dt <- DT::renderDataTable({
    
    # req(my_df(), cancelOutput = T)
    req(dummy())
    
    # datatable( isolate(my_df()$df),
    datatable( isolate(dummy()),
               # datatable( my_df()$df,
               
               rownames = F,
               
               options = list(dom = 't',
                              'bSort' = F,
                              'bInfo' = F,
                              pageLength = my_rows,
                              
                              # columnDefs = list(list(targets = 2, visible = F)),
                              
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '#000'});",
                                "}")
               ) 
    )
    # %>%
    #   # formatStyle(isolate(df_temp()$h), 'h',
    #   formatStyle('h', target = 'row',
    #               # backgroundColor = styleEqual(c(0, 1), c('#6699FF', '#FFFF66')),
    #               fontWeight = styleEqual(c(0, 1), c('normal', 'bold')))
    
  })
  
}

