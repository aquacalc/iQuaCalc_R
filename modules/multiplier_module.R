# MULTIPLIER module functions for
# "iQuaCalc (Lite).R"

# -- no UI --

# 'glues' ic output(s) of ...
#   * one length_module (to calc AREA as πr^2)
#   * two or three length_modules (to calc AREA or VOLUME, respectively)
#   * one length_ and one area_module (to calc VOLUME)
# ... to datatable_module for display resulting AREA or VOLUME in a datatable with all units

# OPTIONS: for AREA   ... 'area_l_by_w', 'area_radius_by_pi_r2'
#          for VOLUME ... 'volume_l_by_w_by_d', 'volume_height_by_radius_by_pi_r2', 'volume_height_by_area'  

# NB: icLength in METERS
# NB: icArea in HECTARES

multiplierModule <- function(input, output, session, 
                             factor_1, factor_2, factor_3,
                             calc_type) {
  
  
  # NB: icLength in METERS
  # NB: icArea in HECTARES
  
  my_df <- reactive({
    
    
    # ***  AREA ***
    
    if('area_l_by_w' == calc_type) {
      
      # p in SQUARE METERS
      p <- factor_1()$ic * factor_2()$ic
      
      icArea <- getInIcUnits(p, 'm²', area.data)
      
      df <- convertAll(icArea, 'ha', area.data)
      
    } else if('area_radius_by_pi_r2' == calc_type) {
      
      # p in SQUARE METERS
      p <-  pi * (factor_1()$ic)^2
      
      icArea <- getInIcUnits(p, 'm²', area.data)
      
      df <- convertAll(icArea, 'ha', area.data)
      
      
  # ***  VOLUME ***
      
    } else if('volume_l_by_w_by_d' == calc_type) {
      
      # p in CUBIC METERS
      p <- factor_1()$ic * factor_2()$ic * factor_3()$ic
      
      icVolume <- getInIcUnits(p, 'cubic meters (m\U00B3)', volume.data)
      
      df <- convertAll(icVolume, 'liters (L)', volume.data)
      
    } else if('volume_height_by_radius_by_pi_r2' == calc_type) {
      
      # p in CUBIC METERS
      p <- factor_1()$ic * pi * (factor_2()$ic)^2
      
      icVolume <- getInIcUnits(p, 'cubic meters (m\U00B3)', volume.data)
      
      df <- convertAll(icVolume, 'liters (L)', volume.data)
      
    } else {    # 'volume_height_by_area'
      
      # p in CUBIC METERS
      area_from_ha_to_m2 <- factor_2()$ic * 10000.0
      p <-  factor_1()$ic * area_from_ha_to_m2
      
      icVolume <- getInIcUnits(p, 'cubic meters (m\U00B3)', volume.data)
      
      df <- convertAll(icVolume, 'liters (L)', volume.data)
      
    }
    
    
    # format decimal values
    df <- round_values(df)
    
    num_rows <- nrow(df)
    
    hidden.col <- c(rep(0, num_rows))
    # hidden.col[idx_area] <- 1
    
    df <- cbind(df, h = hidden.col)
    
    area_or_volume_list <- list(df = df)
                      # ic = icArea,
                      # val = input$area_input,
                      # units = input$areaConvertUnits)
    
    # cat('\n === \n')
    # print(area_or_volume_list)
    # cat(' === \n\n')
    
    area_or_volume_list
    
  })
  
  
  return(my_df)
}