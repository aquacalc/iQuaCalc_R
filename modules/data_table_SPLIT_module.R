# data_table_SPLIT_module  for
# "iQuaCalc (Lite).R"

# -- no UI --

# originally: split 15 rows of flow rate table into 3 5-row tables

datatable_split_Module <- function(input, output, session, 
                                   my_df,
                                   start_row, end_row) {
  
  split_df <- reactive({
    
    flow_rate_list_split <- list(df = my_df()$df[start_row:end_row, ], 
                                 ic = my_df()$ic,
                                 val = my_df()$val,
                                 units = my_df()$units)
    
    flow_rate_list_split

  })

  return(split_df)
  
}