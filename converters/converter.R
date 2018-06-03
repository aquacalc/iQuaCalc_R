# converter.R used in iQuaCalc (Lite) Shiny app

# REDUCED script to validate salToIcUnits()
# AND add LocalStorage

# [MODIFIED 30-XI-2016]



# ---- abstract converter (get one converted value) ----

convert <- function(fromVal, fromUnits, toUnits, type) {
  
  fromIdx <- which(fromUnits == type$units)
  
  toIdx <- which(toUnits == type$units)
  
  icFromVal <-  fromVal / type$factors[fromIdx]
  
  return(icFromVal * type$factors[toIdx])
}


# ---- get value in I.C. units ----
getInIcUnits <- function(fromVal, fromUnits, type) {
  
  fromIdx <- which(fromUnits == type$units)
    
  toIdx <- which(type$icUnits == type$units)
    
  icFromVal <-  fromVal / type$factors[fromIdx]
  
  return(icFromVal)
}


# ---- convert to ALL units ----

convertAll <- function(fromVal, fromUnits, type) {
  
  # get the converted input in terms of Internal Calc units
  icResult <- getInIcUnits(fromVal, fromUnits, type)
  
  # compute the vector of all conversions
  allConversions <- c()
  
  for(f in type$factors) {
    
    # cat('which(f == 777.777):',type$units[which(type$factors == f)],'\n')
    if(f == 777.777 | f == 888.888 | f == 787.878) {
      
      if(777.777 == f)
        ans <- convertMetersToFtAndInches(icResult)[1]
      else if(888.888 == f)
        ans <- convertMetersToFtAndInches(icResult)[2]
      else # "787.878" see: global.R
        ans <- convertMetersToFtAndInches(icResult)[3]
      }
    else
      ans <- f * icResult
    
    allConversions <- c(allConversions, ans)
  }
  
  # df <- data.frame('units' = type$units, 'values' = round(allConversions, 6))
  df <- data.frame(vals  = allConversions,
                   units = type$units,
                   
                   stringsAsFactors = F)
  
  return(df)
}


# ---- General ROUNDING ----

# @paarm df_vals sent as df$vals fron, e.g., area_module
round_values <- function(df) {
  
  my_length <- length(df$vals)
  
  # format decimal df values ----
  
  for(idx in c(1:my_length)) {
    
    v <- as.numeric(df$vals[idx])
    
    # TOO SMALL ...
    if(v < 0.0001) { 
      df$vals[idx] <- formatC(v, format='e', digits=5)
    } else if (v < 0.001) {
      df$vals[idx] <- formatC(v, format='f', digits=4) 
    } else if(v < 0.01) {
      df$vals[idx] <- formatC(v, format='f', digits=3) 
      
      
      # TOO BIG ...
    } else if (v > 1000000) {
      df$vals[idx] <- formatC(v, format='e', digits=3)
    } else if (v > 10000) {
      df$vals[idx] <- formatC(v, format='f', digits=1)
    } else if (v > 100) {
      df$vals[idx] <- formatC(v, format='f', digits=2)
      
      # OK ...
    } else {
      df$vals[idx] <- formatC(v, format='f', digits=4)
    }
    
  }
  
  return(df)
}



# ---- Convert BIOMASS ----

#   fromVal: user-entered biomass value
# fromUnits: user-entered biomass units (e.g., kg/m2, lb/gal (US), etc.)
#      type: always 'biomass' to access conversion units and conversion factors
#   icDepth: user-entered depth (converted, if need be) in meters
convertBiomass <- function(fromVal, fromUnits, type, icDepth) {
  
  # are fromUnits per-unit-volume or per-unit-area?
  if(fromUnits %in% type$units[1:4]) {        # choice is per-unit-AREA
    
    # print('sent per-unit-AREA units')
    # convert to all AREA units
    areaConversions <- convertAll(fromVal, fromUnits, type)
    #     print(areaConversions)
    #     print(areaConversions$values[1:4])
    #     cat('-------------------\n')
    # convert to all volume units
    #     print(icDepth)
    #     print(areaConversions$values[1] / icDepth)
    #     print(type$units[5])
    
    # cat(areaConversions$values[1] / icDepth, type$units[5],'\n')
    volumeConversions <- convertAll(areaConversions$values[1] / icDepth, type$units[5], type)
    
    convertedBiomass <- c(areaConversions$values[1:4], volumeConversions$values[5:9])
    # cat('\n-------- from AREA -----------\n')
    print(convertedBiomass)
    
    return(convertedBiomass)
    
  } else{                                     # choice is per-unit-VOLUME
    
    # print('sent per-unit-VOLUME units')
    # convert to all VOLUME units
    volumeConversions <- convertAll(fromVal, fromUnits, type)
    
    areaConversions <- convertAll(volumeConversions$values[1] * icDepth, type$units[1], type)
    
    convertedBiomass <- c(areaConversions$values[1:4], volumeConversions$values[5:9])
    # cat('\n-------- from VOLUME -----------\n')
    print(convertedBiomass)
    
    return(convertedBiomass)
  }
}



# ---- Convert [ALK] ----

# from "AlkalinityField.m"
# convert input value to Internal Calc Units, meq/kg
alkToIcUnits <- function(alk, alkUnits, my_rho) {
  
  if('meq/kg (mmol/kg)' == alkUnits) {
    return(alk)
  }
  
  if('meq/L (mmol/L)' == alkUnits) {
    return(alk / my_rho)
  }
  
  # if('ppm-m CaCO3 (mg/kg)' == alkUnits) {
  if('mg/kg CaCO3' == alkUnits) {
    return(alk / 50.04345)
  }
  
  # if('ppm-v CaCO3 (mg/L)' == alkUnits) {
  if('mg/L CaCO3' == alkUnits) {
    return(alk / 50.04345 / my_rho)
  }
  
  if('dKH' == alkUnits) {
    return(alk / 2.8)
  }
  
}


# @alk in IC Units -- meq/kg
# @rho in IC Units -- g/L
alkToAllUnits <- function(alk, rho) {
  
  meq_L <- round(alk * rho, 4)
  ppm_m <- round(alk * 50.04345, 3)
  ppm_v <- round(alk * rho * 50.04345, 3)
  dkh <- round(alk * 2.8, 4)
  alk <- round(alk, 4)
  
  meq_L <- formatC(meq_L, format='f', digits=2)
  ppm_m <- formatC(ppm_m, format='f', digits=1)
  ppm_v <- formatC(ppm_v, format='f', digits=1)
  dkh <- formatC(dkh, format='f', digits=1)
  alk <- formatC(alk, format='f', digits=2)
  
  df <- data.frame(vals = c(alk, meq_L, ppm_m, ppm_v, dkh),
                   # units = alkUnitsList_short,
                   # units = c('meq/kg (mmol/kg)', 'meq/L (mmol/L)',
                   #           'ppm-m CaCO3 (mg/kg)', 'ppm-v CaCO3 (mg/L)',
                   #           'dKH'),
                   units = c('meq/kg (mmol/kg)', 'meq/L (mmol/L)',
                             'mg/kg CaCO3', 'mg/L CaCO3',
                             'dKH'),
                   
                   # NB: ** NEED ** "stringsAsFactors = FALSE" to quell...
                   # Warning in `[<-.factor`(`*tmp*`, idx_s, value = "<strong><i>33.00</i></strong>") :
                   # invalid factor level, NA generated
                   # when print
                   stringsAsFactors = FALSE
  )
  
  return(df)
}



# ---- Convert TAN ----


# tanUnitsList <- c('μg/L', 'μg/kg', 
#                   'mg/L', 'mg/kg', 
#                   'μmol/L', 'μmol/kg', 
#                   'mmol/L', 'mmol/kg')

# convert input value to Internal Calc Units, ... mg TA-N/L
# "*-N" vs "*" (e.g., TA-N vs TA)

# for UIA posto...
# see: CarbCalc.R, percentNh3ForTemp <- function(t, sal, ph)

tanToIcUnits <- function(tan, tanUnits, my_rho, uia_posto) {
  
  # @my_rho in IC Units, g/L
  # convert to mg/L
  my_rho <- 1000.0 * my_rho          # [mg/g]*[g/L] -> mg/L
  
  uia_fraction <- uia_posto / 100.0
  # ia_fraction <- 1 - uia_fraction
  
  n_fraction_un_ionized <- N / NH3
  n_fraction_ionized    <- N / NH4
  
  
  # ******************** TA-N, UIA-N, IA-N **********************
  
  # TA-N
  
  if('mg/L TA-N' == tanUnits) {
    return(tan)
  }
  
  if('mg/kg TA-N' == tanUnits) {
    return(tan * my_rho * 0.000001)  # [mg/kg]*[mg/L]*[kg/mg] -> mg/L
  }
  
  if('μg/L TA-N' == tanUnits) {
    return(tan * 0.001)              # [μg/L]*[mg/μg] -> mg/L
  }
  
  if('μg/kg TA-N' == tanUnits) {
    return(tan * 0.001 * my_rho * 0.000001) # [μg/kg]*[mg/μg] -> [mg/kg]...*[mg/L]*[kg/mg] -> mg/L
  }
  
  if('mmol/L TA-N' == tanUnits) {  # [mmol TA-N/L]*[mg N/mmol TA-N] -> mg TA-N/L
    return(tan * N)
  }
  
  if('mmol/kg TA-N' == tanUnits) {
    return(tan * N * my_rho * 0.000001)
  }
  
  if('μmol/L TA-N' == tanUnits) {  # [μmol TA-N/L]*[μg N/μmol TA-N]*[0.001 mg/μg] -> mg TA-N/L
    return(tan * N * 0.001)
  }
  
  if('μmol/kg TA-N' == tanUnits) {
    return(tan * N * 0.001 * my_rho * 0.000001)
  }
  
  
  # UIA-N
  
  if('mg/L UIA-N' == tanUnits) {
    return(tan / uia_fraction)
  }
  
  if('mg/kg UIA-N' == tanUnits) {
    return((tan / uia_fraction) * my_rho * 0.000001)
  }
  
  if('μg/L UIA-N' == tanUnits) {
    return(0.001 * tan / uia_fraction)
  }
  
  if('μg/kg UIA-N' == tanUnits) {
    return((0.001 * tan / uia_fraction) * my_rho * 0.000001)
  }
  
  if('mmol/L UIA-N' == tanUnits) {
    return((tan / uia_fraction) * N)
  }
  
  if('mmol/kg UIA-N' == tanUnits) {
    return((tan / uia_fraction) * N * my_rho * 0.000001)
  }
  
  if('μmol/L UIA-N' == tanUnits) {
    return(((tan * 0.001) / uia_fraction) * N)
  }
  
  if('μmol/kg UIA-N' == tanUnits) {
    return(((tan * 0.001) / uia_fraction) * N * my_rho * 0.000001)
  }
  
  
  # IA-N
  
  if('mg/L IA-N' == tanUnits) {
    return(tan / (1 - uia_fraction))
  }
  
  if('mg/kg IA-N' == tanUnits) {
    return((tan / (1 - uia_fraction)) * my_rho * 0.000001)
  }
  
  if('μg/L IA-N' == tanUnits) {
    return(0.001 * tan / (1 - uia_fraction))
  }
  
  if('μg/kg IA-N' == tanUnits) {
    return((0.001 * tan / (1 - uia_fraction)) * my_rho * 0.000001)
  }
  
  if('mmol/L IA-N' == tanUnits) {
    return((tan / (1 - uia_fraction)) * N)
  }
  
  if('mmol/kg IA-N' == tanUnits) {
    return((tan / (1 - uia_fraction)) * N * my_rho * 0.000001)
  }
  
  if('μmol/L IA-N' == tanUnits) {
    return(((tan * 0.001) / (1 - uia_fraction)) * N)
  }
  
  if('μmol/kg IA-N' == tanUnits) {
    return(((tan * 0.001) / (1 - uia_fraction)) * N * my_rho * 0.000001)
  }
  
  
  # ******************** TA, UIA, IA **********************
  
  # TA
  
  if('mg/L TA' == tanUnits) {          # [mg/L]*([% N/NH3]*[% UIA] + [% N/NH4]*[% IA]) -> mg N/L
    return(tan * (n_fraction_un_ionized * uia_fraction + 
                  n_fraction_ionized    * (1 - uia_fraction)))
  }
  
  if('mg/kg TA' == tanUnits) {
    return(tan * (n_fraction_un_ionized * uia_fraction + 
                  n_fraction_ionized    * (1 - uia_fraction)) * my_rho * 0.000001)
  }
  
  if('μg/L TA' == tanUnits) {
    return(0.001 * tan * (n_fraction_un_ionized * uia_fraction + 
                            n_fraction_ionized    * (1 - uia_fraction)))
  }
  
  if('μg/kg TA' == tanUnits) {
    return(0.001 * tan * (n_fraction_un_ionized * uia_fraction + 
                            n_fraction_ionized    * (1 - uia_fraction)) * my_rho * 0.000001)
  }
  
  if('mmol/L TA' == tanUnits) {
    # return(tan * (n_fraction_un_ionized * uia_fraction * NH3 + 
    #                 n_fraction_ionized    * (1 - uia_fraction) * NH4))
    return(tan * N)  # 1 mmol/L TA = 1 mmol/L TA-N, so [x mmol/L TA-N]*[14.00674 mg N/mmol N] -> mg/L TA-N
  }
  
  if('mmol/kg TA' == tanUnits) {
    # return(tan * (n_fraction_un_ionized * uia_fraction * NH3 + 
    #                 n_fraction_ionized * (1 - uia_fraction) * NH4) * my_rho * 0.000001)
    return(tan * N * my_rho * 0.000001)
  }
  
  if('μmol/L TA' == tanUnits) {
    # return(tan * (n_fraction_un_ionized * uia_fraction * NH3 + 
    #                 n_fraction_ionized    * (1 - uia_fraction) * NH4))
    return((tan * 0.001) * N)  # 1 mmol/L TA = 1 mmol/L TA-N, so [x mmol/L TA-N]*[14.00674 mg N/mmol N] -> mg/L TA-N
  }
  
  if('μmol/kg TA' == tanUnits) {
    # return(tan * (n_fraction_un_ionized * uia_fraction * NH3 + 
    #                 n_fraction_ionized * (1 - uia_fraction) * NH4) * my_rho * 0.000001)
    return((tan * 0.001) * N * my_rho * 0.000001)
  }
  
  
  # UIA
  
  if('mg/L UIA' == tanUnits) {
    return((tan / uia_fraction) * (n_fraction_un_ionized * uia_fraction + 
                                     n_fraction_ionized  * (1 - uia_fraction)))
  }
  
  if('mg/kg UIA' == tanUnits) {
    return((tan / uia_fraction) * (n_fraction_un_ionized * uia_fraction + 
                                     n_fraction_ionized  * (1 - uia_fraction)) * my_rho * 0.000001)
  }
  
  if('μg/L UIA' == tanUnits) {
    return(0.001 * (tan / uia_fraction) * (n_fraction_un_ionized * uia_fraction + 
                                     n_fraction_ionized  * (1 - uia_fraction)))
  }
  
  if('μg/kg UIA' == tanUnits) {
    return(0.001 * (tan / uia_fraction) * (n_fraction_un_ionized * uia_fraction + 
                                     n_fraction_ionized  * (1 - uia_fraction)) * my_rho * 0.000001)
  }
  
  if('mmol/L UIA' == tanUnits) {
    # return(((tan * NH3) / uia_fraction) * (n_fraction_un_ionized * uia_fraction + 
    #                                          n_fraction_ionized  * (1 - uia_fraction)))
    return((tan / uia_fraction) * N)
  }
  
  if('mmol/kg UIA' == tanUnits) {
    # return(((tan * NH3) / uia_fraction) * (n_fraction_un_ionized * uia_fraction + 
    #                                          n_fraction_ionized  * (1 - uia_fraction)) * my_rho * 0.000001)
    return((tan / uia_fraction) * N * my_rho * 0.000001)
  }
  
  if('μmol/L UIA' == tanUnits) {
    # return(((tan * NH3) / uia_fraction) * (n_fraction_un_ionized * uia_fraction + 
    #                                          n_fraction_ionized  * (1 - uia_fraction)))
    return(((tan * 0.001) / uia_fraction) * N)
  }
  
  if('μmol/kg UIA' == tanUnits) {
    # return(((tan * NH3) / uia_fraction) * (n_fraction_un_ionized * uia_fraction + 
    #                                          n_fraction_ionized  * (1 - uia_fraction)) * my_rho * 0.000001)
    return(((tan * 0.001) / uia_fraction) * N * my_rho * 0.000001)
  }
  
  
  # IA
  
  if('mg/L IA' == tanUnits) {
    return((tan / (1 - uia_fraction)) * (n_fraction_un_ionized * uia_fraction + 
                                     n_fraction_ionized  * (1 - uia_fraction)))
  }
  
  if('mg/kg IA' == tanUnits) {
    return((tan / (1 - uia_fraction)) * (n_fraction_un_ionized * uia_fraction + 
                                     n_fraction_ionized  * (1 - uia_fraction)) * my_rho * 0.000001)
  }
  
  if('μg/L IA' == tanUnits) {
    return(0.001 * (tan / (1 - uia_fraction)) * (n_fraction_un_ionized * uia_fraction + 
                                             n_fraction_ionized  * (1 - uia_fraction)))
  }
  
  if('μg/kg IA' == tanUnits) {
    return(0.001 * (tan / (1 - uia_fraction)) * (n_fraction_un_ionized * uia_fraction + 
                                             n_fraction_ionized  * (1 - uia_fraction)) * my_rho * 0.000001)
  }
  
  if('mmol/L IA' == tanUnits) {
    # return(((tan * NH4) / (1 - uia_fraction)) * (n_fraction_un_ionized * uia_fraction + 
    #                                          n_fraction_ionized  * (1 - uia_fraction)))
    return((tan / (1 - uia_fraction)) * N)
  }
  
  if('mmol/kg IA' == tanUnits) {
    # return(((tan * NH4) / (1 - uia_fraction)) * (n_fraction_un_ionized * uia_fraction + 
    #                                          n_fraction_ionized  * (1 - uia_fraction)) * my_rho * 0.000001)
    return((tan / (1 - uia_fraction)) * N * my_rho * 0.000001)
  }
  
  if('μmol/L IA' == tanUnits) {
    # return(((tan * NH4) / (1 - uia_fraction)) * (n_fraction_un_ionized * uia_fraction + 
    #                                          n_fraction_ionized  * (1 - uia_fraction)))
    return(((tan * 0.001) / (1 - uia_fraction)) * N)
  }
  
  if('μmol/kg IA' == tanUnits) {
    # return(((tan * NH4) / (1 - uia_fraction)) * (n_fraction_un_ionized * uia_fraction + 
    #                                          n_fraction_ionized  * (1 - uia_fraction)) * my_rho * 0.000001)
    return(((tan * 0.001) / (1 - uia_fraction)) * N * my_rho * 0.000001)
  }
  
}


# @rho in IC Units -- g/L
tanToAllUnits <- function(tan, rho, uia_posto, dec_places, num_digits) {
  
  # @rho in IC Units, g/L
  # convert to mg/L
  rho <- 1000.0 * rho          # [mg/g]*[g/L] -> mg/L
  
  uia_fraction <- uia_posto / 100.0
  
  n_fraction_un_ionized <- N / NH3
  n_fraction_ionized    <- N / NH4
  
  
  # ******************** TA-N, UIA-N, IA-N **********************
  
  mg_L_TAN    <- round(tan, dec_places)
  mg_kg_TAN   <- round(tan / (0.000001 * rho), dec_places)   # [mg/L] / ([kg/mg]*[mg/L])
  mmol_L_TAN  <- round(tan / N, dec_places)
  mmol_kg_TAN <- round(tan / (N * rho * 0.000001), dec_places)
  μg_L_TAN    <- round(tan * 1000.0, dec_places)
  μg_kg_TAN   <- round(tan * 1000.0 / (0.000001 * rho), dec_places)   # [mg/L]*[μg/mg] / ([kg/mg]*[mg/L])
  μmol_L_TAN  <- round((tan / N) * 1000.0, dec_places)
  μmol_kg_TAN <- round((tan / (N * rho * 0.000001)) * 1000.0, dec_places)
  
  mg_L_UIAN    <- round(tan * uia_fraction, dec_places)
  mg_kg_UIAN   <- round(tan * uia_fraction / (rho * 0.000001), dec_places)
  mmol_L_UIAN  <- round(tan * uia_fraction / N, dec_places)
  mmol_kg_UIAN <- round(tan * uia_fraction / (N * rho * 0.000001), dec_places)
  μg_L_UIAN    <- round(1000.0 * tan * uia_fraction, dec_places)
  μg_kg_UIAN   <- round(1000.0 * tan * uia_fraction / (rho * 0.000001), dec_places)
  μmol_L_UIAN  <- round((tan * uia_fraction / N) * 1000.0, dec_places)
  μmol_kg_UIAN <- round((tan * uia_fraction / (N * rho * 0.000001)) * 1000.0, dec_places)
  
  mg_L_IAN     <- round(tan * (1 - uia_fraction), dec_places)
  mg_kg_IAN    <- round(tan * (1 - uia_fraction) / (rho * 0.000001), dec_places)
  mmol_L_IAN   <- round(tan * (1 - uia_fraction) / N, dec_places)
  mmol_kg_IAN  <- round(tan * (1 - uia_fraction) / (N * rho * 0.000001), dec_places)
  μg_L_IAN     <- round(1000.0 * tan * (1 - uia_fraction), dec_places)
  μg_kg_IAN    <- round(1000.0 * tan * (1 - uia_fraction) / (rho * 0.000001), dec_places)
  μmol_L_IAN   <- round((tan * (1 - uia_fraction) / N) * 1000.0, dec_places)
  μmol_kg_IAN  <- round((tan * (1 - uia_fraction) / (N * rho * 0.000001)) * 1000.0, dec_places)
  
  
  mg_L_TAN    <- formatC(mg_L_TAN, format='f', digits=num_digits)
  mg_kg_TAN   <- formatC(mg_kg_TAN, format='f', digits=num_digits)
  mmol_L_TAN  <- formatC(mmol_L_TAN, format='f', digits=num_digits)
  mmol_kg_TAN <- formatC(mmol_kg_TAN, format='f', digits=num_digits)
  μg_L_TAN    <- formatC(μg_L_TAN, format='f', digits=num_digits)
  μg_kg_TAN   <- formatC(μg_kg_TAN, format='f', digits=num_digits)
  μmol_L_TAN  <- formatC(μmol_L_TAN, format='f', digits=num_digits)
  μmol_kg_TAN <- formatC(μmol_kg_TAN, format='f', digits=num_digits)
  
  mg_L_UIAN    <- formatC(mg_L_UIAN, format='f', digits=num_digits)
  mg_kg_UIAN   <- formatC(mg_kg_UIAN, format='f', digits=num_digits)
  mmol_L_UIAN  <- formatC(mmol_L_UIAN, format='f', digits=num_digits)
  mmol_kg_UIAN <- formatC(mmol_kg_UIAN, format='f', digits=num_digits)
  μg_L_UIAN    <- formatC(μg_L_UIAN, format='f', digits=num_digits)
  μg_kg_UIAN   <- formatC(μg_kg_UIAN, format='f', digits=num_digits)
  μmol_L_UIAN  <- formatC(μmol_L_UIAN, format='f', digits=num_digits)
  μmol_kg_UIAN <- formatC(μmol_kg_UIAN, format='f', digits=num_digits)
  
  mg_L_IAN    <- formatC(mg_L_IAN, format='f', digits=num_digits)
  mg_kg_IAN   <- formatC(mg_kg_IAN, format='f', digits=num_digits)
  mmol_L_IAN  <- formatC(mmol_L_IAN, format='f', digits=num_digits)
  mmol_kg_IAN <- formatC(mmol_kg_IAN, format='f', digits=num_digits)
  μg_L_IAN    <- formatC(μg_L_IAN, format='f', digits=num_digits)
  μg_kg_IAN   <- formatC(μg_kg_IAN, format='f', digits=num_digits)
  μmol_L_IAN  <- formatC(μmol_L_IAN, format='f', digits=num_digits)
  μmol_kg_IAN <- formatC(μmol_kg_IAN, format='f', digits=num_digits)
  
  
  # ******************** TA, UIA, IA **********************
  
  my_factor <- (n_fraction_un_ionized * uia_fraction + n_fraction_ionized * (1 - uia_fraction))
  
  mg_L_TA    <- round(tan / my_factor, dec_places)
  mg_kg_TA   <- round(tan / (my_factor * 0.000001 * rho), dec_places) 
  mmol_L_TA  <- round(tan / N, dec_places)
  mmol_kg_TA <- round(tan / (N * rho * 0.000001), dec_places)
  μg_L_TA    <- round(tan * 1000.0 / my_factor, dec_places)
  μg_kg_TA   <- round(tan * 1000.0 / (my_factor * 0.000001 * rho), dec_places) 
  μmol_L_TA  <- round((tan / N) * 1000.0, dec_places)
  μmol_kg_TA <- round((tan / (N * rho * 0.000001)) * 1000.0, dec_places) 
  
  mg_L_UIA    <- round(tan * uia_fraction / my_factor, dec_places)
  mg_kg_UIA   <- round(tan * uia_fraction / (my_factor * rho * 0.000001), dec_places)
  mmol_L_UIA  <- round(tan * uia_fraction / N, dec_places)
  mmol_kg_UIA <- round(tan * uia_fraction / (N * rho * 0.000001), dec_places)
  μg_L_UIA    <- round(1000.0 * tan * uia_fraction / my_factor, dec_places)
  μg_kg_UIA   <- round(1000.0 * tan * uia_fraction / (my_factor * rho * 0.000001), dec_places)
  μmol_L_UIA  <- round((tan * uia_fraction / N) * 1000.0, dec_places)
  μmol_kg_UIA <- round((tan * uia_fraction / (N * rho * 0.000001)) * 1000.0, dec_places)
  
  mg_L_IA     <- round(tan * (1 - uia_fraction) / my_factor, dec_places)
  mg_kg_IA    <- round(tan * (1 - uia_fraction) / (my_factor * rho * 0.000001), dec_places)
  mmol_L_IA   <- round(tan * (1 - uia_fraction) / N, dec_places)
  mmol_kg_IA  <- round(tan * (1 - uia_fraction) / (N * rho * 0.000001), dec_places)
  μg_L_IA     <- round(1000.0 * tan * (1 - uia_fraction) / my_factor, dec_places)
  μg_kg_IA    <- round(1000.0 * tan * (1 - uia_fraction) / (my_factor * rho * 0.000001), dec_places)
  μmol_L_IA   <- round((tan * (1 - uia_fraction) / N) * 1000.0, dec_places)
  μmol_kg_IA  <- round((tan * (1 - uia_fraction) / (N * rho * 0.000001)) * 1000.0, dec_places)
  
  
  
  mg_L_TA    <- formatC(mg_L_TA, format='f', digits=num_digits)
  mg_kg_TA   <- formatC(mg_kg_TA, format='f', digits=num_digits)
  mmol_L_TA  <- formatC(mmol_L_TA, format='f', digits=num_digits)
  mmol_kg_TA <- formatC(mmol_kg_TA, format='f', digits=num_digits)
  μg_L_TA    <- formatC(μg_L_TA, format='f', digits=num_digits)
  μg_kg_TA   <- formatC(μg_kg_TA, format='f', digits=num_digits)
  μmol_L_TA  <- formatC(μmol_L_TA, format='f', digits=num_digits)
  μmol_kg_TA <- formatC(μmol_kg_TA, format='f', digits=num_digits)
  
  mg_L_UIA    <- formatC(mg_L_UIA, format='f', digits=num_digits)
  mg_kg_UIA   <- formatC(mg_kg_UIA, format='f', digits=num_digits)
  mmol_L_UIA  <- formatC(mmol_L_UIA, format='f', digits=num_digits)
  mmol_kg_UIA <- formatC(mmol_kg_UIA, format='f', digits=num_digits)
  μg_L_UIA    <- formatC(μg_L_UIA, format='f', digits=num_digits)
  μg_kg_UIA   <- formatC(μg_kg_UIA, format='f', digits=num_digits)
  μmol_L_UIA  <- formatC(μmol_L_UIA, format='f', digits=num_digits)
  μmol_kg_UIA <- formatC(μmol_kg_UIA, format='f', digits=num_digits)
  
  mg_L_IA    <- formatC(mg_L_IA, format='f', digits=num_digits)
  mg_kg_IA   <- formatC(mg_kg_IA, format='f', digits=num_digits)
  mmol_L_IA  <- formatC(mmol_L_IA, format='f', digits=num_digits)
  mmol_kg_IA <- formatC(mmol_kg_IA, format='f', digits=num_digits)
  μg_L_IA    <- formatC(μg_L_IA, format='f', digits=num_digits)
  μg_kg_IA   <- formatC(μg_kg_IA, format='f', digits=num_digits)
  μmol_L_IA  <- formatC(μmol_L_IA, format='f', digits=num_digits)
  μmol_kg_IA <- formatC(μmol_kg_IA, format='f', digits=num_digits)
  
  
  
  df <- data.frame(vals = c(mg_L_TAN, mg_kg_TAN, 
                            mmol_L_TAN, mmol_kg_TAN,
                            μg_L_TAN, μg_kg_TAN, 
                            μmol_L_TAN, μmol_kg_TAN,
                            
                            mg_L_UIAN, mg_kg_UIAN, 
                            mmol_L_UIAN, mmol_kg_UIAN,
                            μg_L_UIAN, μg_kg_UIAN, 
                            μmol_L_UIAN, μmol_kg_UIAN,
                            # mg_L_IAN, mg_kg_IAN, 
                            # μg_L_IAN, μg_kg_IAN, 
                            # mmol_L_IAN, mmol_kg_IAN,
                            
                            mg_L_TA, mg_kg_TA, 
                            mmol_L_TA, mmol_kg_TA,
                            μg_L_TA, μg_kg_TA, 
                            μmol_L_TA, μmol_kg_TA,
                            
                            mg_L_UIA, mg_kg_UIA, 
                            mmol_L_UIA, mmol_kg_UIA,
                            μg_L_UIA, μg_kg_UIA, 
                            μmol_L_UIA, μmol_kg_UIA
                            # mg_L_IA, mg_kg_IA, 
                            # μg_L_IA, μg_kg_IA, 
                            # mmol_L_IA, mmol_kg_IA
                            ),
                   # units = alkUnitsList_short,
                   
                   units = c('mg/L', 'mg/kg (ppm)',
                             'mmol/L (mM)', 'mmol/kg',
                             'μg/L', 'μg/kg (ppb)',
                             'μmol/L (μM)', 'μmol/kg',
                             
                             'mg/L', 'mg/kg (ppm)',
                             'mmol/L (mM)', 'mmol/kg',
                             'μg/L', 'μg/kg (ppb)',
                             'μmol/L (μM)', 'μmol/kg',
                             
                             'mg/L', 'mg/kg (ppm)',
                             'mmol/L (mM)', 'mmol/kg',
                             'μg/L', 'μg/kg (ppb)',
                             'μmol/L (μM)', 'μmol/kg',
                             
                             'mg/L', 'mg/kg (ppm)',
                             'mmol/L (mM)', 'mmol/kg',
                             'μg/L', 'μg/kg (ppb)',
                             'μmol/L (μM)', 'μmol/kg'
                   ),
                   
                   # units = c('mg/L TA-N', 'mg/kg TA-N',
                   #           'μg/L TA-N', 'μg/kg TA-N',
                   #           'mmol/L TA-N', 'mmol/kg TA-N',
                   #           'mg/L UIA-N', 'mg/kg UIA-N',
                   #           'μg/L UIA-N', 'μg/kg UIA-N',
                   #           'mmol/L UIA-N', 'mmol/kg UIA-N',
                   #           # 'mg/L IA-N', 'mg/kg IA-N',
                   #           # 'μg/L IA-N', 'μg/kg IA-N',
                   #           # 'mmol/L IA-N', 'mmol/kg IA-N',
                   #           
                   #           'mg/L TA', 'mg/kg TA',
                   #           'μg/L TA', 'μg/kg TA',
                   #           'mmol/L TA', 'mmol/kg TA',
                   #           'mg/L UIA', 'mg/kg UIA',
                   #           'μg/L UIA', 'μg/kg UIA',
                   #           'mmol/L UIA', 'mmol/kg UIA'
                   #           # 'mg/L IA', 'mg/kg IA',
                   #           # 'μg/L IA', 'μg/kg IA',
                   #           # 'mmol/L IA', 'mmol/kg IA'
                   #           ),
                   
                   # NB: ** NEED ** "stringsAsFactors = FALSE" to quell...
                   # Warning in `[<-.factor`(`*tmp*`, idx_s, value = "<strong><i>33.00</i></strong>") :
                   # invalid factor level, NA generated
                   # when print
                   stringsAsFactors = FALSE
  )
  
  return(df)
}



# ---- Convert TEMP ----

# ic temperature units are KELVIN
tempToIcUnits <- function(temp, tempUnitsIndex) {
  if(1 == tempUnitsIndex) {
    return(temp + 273.15)
  } else if(2 == tempUnitsIndex) {
    return( (5 * (temp - 32) / 9) + 273.15 )
  } else {
    return(temp)
  }
}

# return df of all 3 temp conversions

# when called WITH IC temp (KELVIN)
tempToAllUnits <- function(temp_K) {
  
  # 1. convert K to C
  temp_C <- temp_K - 273.15
  
  # 3. convert K to F
  temp_F <- (9 / 5) * temp_C + 32
  
  # temp_C <- round(temp_C, 2)
  # temp_F <- round(temp_F, 2)
  # temp_K <- round(temp_K, 2)
  
  temp_C <- formatC(temp_C, format='f', digits=2)
  temp_F <- formatC(temp_F, format='f', digits=2)
  temp_K <- formatC(temp_K, format='f', digits=2)
  
  df <- data.frame(vals = c(temp_C, temp_F, temp_K), 
                   units = c('C', 'F', 'K'),
                   stringsAsFactors = FALSE)
  
  return(df)
}



# ---- Convert SAL to ALL Units ----

# convert to all units
# @param h_input_val, entered hydrometry value
# @param h_input_idx, entered hydrometry units index
# @param sal in IC units (ppt)
# @param temp in IC units (K)
# @param tc, hydrometry temp correction, boolean

# Answers the Q: What is the hydrometer reading for sal in K?
# salToAllUnits <- function(h_input_val, h_input_idx, sal, temp, tc) {

salToAllUnits <- function(sal, temp) {
  
  t <- temp - 273.15
  
  h_15_0 <- round(givenSalAndTempCalcUncorrectedHydrometerReading(sal, t, 15.556, 0), 5)
  h_15_1 <- round(givenSalAndTempCalcUncorrectedHydrometerReading(sal, t, 15.556, 1), 5)
  h_20_0 <- round(givenSalAndTempCalcUncorrectedHydrometerReading(sal, t, 20.0, 0), 5)
  h_20_1 <- round(givenSalAndTempCalcUncorrectedHydrometerReading(sal, t, 20.0, 1), 5)
  h_25_0 <- round(givenSalAndTempCalcUncorrectedHydrometerReading(sal, t, 25.0, 0), 5)
  h_25_1 <- round(givenSalAndTempCalcUncorrectedHydrometerReading(sal, t, 25.0, 1), 5)
  
  
  h_15_0 <- formatC(h_15_0, format='f', digits=5)
  h_15_1 <- formatC(h_15_1, format='f', digits=5)
  h_20_0 <- formatC(h_20_0, format='f', digits=5)
  h_20_1 <- formatC(h_20_1, format='f', digits=5)
  h_25_0 <- formatC(h_25_0, format='f', digits=5)
  h_25_1 <- formatC(h_25_1, format='f', digits=5)
  
  # cat('2: salTo--All--Units ... passed in: ', sal, ' -- h_25_1: ', h_25_1, '\n')

  # c in S/m
  # NB: Salinity Trap !!
  # see: Fofonoff & ...
  if(sal >= 0.02)
    c_Sm <- calcConductivityFromSalinity(sal, t, 0)
  else
    c_Sm <- 0.0
  
  c_microScm <- c_Sm * 10000
  c_Scm <- c_Sm / 100
  c_mScm <- c_dSmm <- c_Sm * 10
  
  # cat('3: salTo--All--Units ... passed in: ', sal, ' -- c_Sm: ', c_Sm, '\n')
  
  c_Sm <- round(c_Sm, 3)
  c_microScm <- round(c_microScm, 0)
  c_Scm <- round(c_Scm, 4)
  c_mScm <- c_dSmm <- round(c_dSmm, 2)
  
  c_Sm <- formatC(c_Sm, format='f', digits=3)
  c_microScm <- formatC(c_microScm, format='f', digits=0)
  c_Scm <- formatC(c_Scm, format='f', digits=4)
  c_mScm <- c_dSmm <- formatC(c_mScm, format='f', digits=2)
  
  sal <- round(sal, 2)
  sal <- formatC(sal, format='f', digits=2)
  
  # see: http://stackoverflow.com/questions/38348629/r-dt-datatable-format-text-field-and-vertically-align-another-field
  
  
  # df <- data.frame(vals = c(   sal,   c_microScm, c_Sm,   c_Scm,     c_mScm,       h_15,      h_20,      h_25),
                   # units = c('ppt', 'μS/cm (EC)', 'S/m', 'S/cm', 'mS/cm (dS/m)', '60F/60F', '20C/20C', '77F/77F')
  df <- data.frame(vals = c(sal, c_microScm, c_Sm, c_Scm, c_mScm,
                            paste(paste0(h_15_0, "<br/>", h_15_1, collapse=" ")),
                            paste0(h_20_0, '<br/>', h_20_1),  paste0(h_25_0, '<br/>', h_25_1)),
  # df <- data.frame(vals = mapply(FUN = v, 1:length(units), c(0, 0, 0, 0, 0, 5, 4, 3)),
                   units = mapply(FUN = f, 1:8, c(0, 0, 0, 0, 0, 5, 4, 3)),
  
  # NB: ** NEED ** "stringsAsFactors = FALSE" to quell...
  # Warning in `[<-.factor`(`*tmp*`, idx_s, value = "<strong><i>33.00</i></strong>") :
  # invalid factor level, NA generated
  # when print
  stringsAsFactors = FALSE
                   )
  
  return(df)
}


# f & v -- support functions to build df in salToAllUnits(sal, temp)

f <- function(x, y) {
  
  units <- c('ppt', 
             'μS/cm (μmho/cm)', 'S/m', 'S/cm', 'mS/cm (dS/m)', 
             '60F/60F', '20C/20C', '77F/77F')
  units_h <- c('60F/60F (raw)', '60F/60F (real)', 
               '20C/20C (raw)', '20C/20C (real)', 
               '77F/77F (raw)', '77F/77F (real)')
  
  if(x %in% c(1:5)) {
    
    paste(units[x])
    
  } else {
    
    paste(paste0(units_h[x - y],
                 "<br/>",
                 units_h[x - (y - 1)],
                 collapse=" "))
  }
}

v <- function(x, y) {
  
  vals = c(7.9, 45000, 4.5, 0.45, 45)
  vals_h = c(1.0110, 1.0211, 1.0119, 1.0128, 1.0230, 1.0235)
  
  if(x %in% c(1:5)) {
    
    paste(vals[x])
    
  } else {
    
    paste(paste0(vals_h[x - y],
                 "<br/>",
                 vals_h[x - (y - 1)],
                 collapse=" "))
  }
}



# ---- Convert SAL to I.C. Units ----

# @sal      - input "salinity" value -- could be 'true' salinity, condcutivity, hydrography
# @salUnits - index of salinity units from drop-down widget
# @temp     - temp passed in as I.C. units (K), but -- N.B. -- each auxiliary method requires Celcius
# @tc       - temp correction flag for hydrometer conversions

# output -- ic salinity units -- are PARTS PER THOUSAND
salToIcUnits <- function(sal, salUnits, temp, tc) {
  
  # cat('START: salToIcUnits ... passed in: ', sal, ' ', salUnits, '\n')
  # cat('                     tc: ', tc, '\n')
  
  # *** convert temp in K to temp in C for methonds below
  temp <- temp - 273.15
    
#  '‰'=1,'μS/cm'=2,'S/m'=3, 'S/cm'=4, 'mS/cm'=5,'dS/m'=6,
#   NB: (reference temp) / (standard temp)
#  20C/20C'=7,'60F/60F'=8,'77F/77F'=9
  
    if (1 == salUnits) {
      # cat('PASSED IN...‰...sal: ', sal, ' temp: ', temp, '\n')
      return(sal)
       
    } else if (2 == salUnits) {     # ** NB: first convert to S/m
        # cat('\n\nPASSED IN...μS/cm...sal: ', sal, ' --> ', sal * 0.0001, ' & temp: ', temp, '\n\n')
        return(calcSwSalinityFromConductivity(sal * 0.0001, temp, 0))
    } else if (3 == salUnits) {
        # cat('\n\nPASSED IN...S/m...sal: ', sal, ' --> ', sal * 1, ' & temp: ', temp, '\n\n')
        return(calcSwSalinityFromConductivity(sal, temp, 0))
    } else if (4 == salUnits) {
        return(calcSwSalinityFromConductivity(sal * 100.0, temp, 0))
    } else if (5 == salUnits) {
      return(calcSwSalinityFromConductivity(sal * 0.1, temp, 0))
    # } else if (6 == salUnits) {
    #     return(calcSwSalinityFromConductivity(sal * 0.1, temp, 0))
      
    # } else if (6 == salUnits) {
    #     # return(calcSalOfSpGr(sal, temp, 15.0, 4.0, tc))
    #     return(calcSalFromCorrectedSpGr(sal, temp, 15.0, 4.0, tc))
      
      
    # [KLUDGE] -- CHANGE TO '7'
    } else if (7 == salUnits) {
      
      # need to pass tc (boolean) -- 0: un-corrected datum entered
        return(test_spgr_calcs(sal, temp, 20.0, tc))
    } else if (6 == salUnits) {
      
      return(test_spgr_calcs(sal, temp, 15.5556, tc))
    } else {                        # ** 77F/77F
      
      return(test_spgr_calcs(sal, temp, 25.0, tc))
    }
}


# spgr_reading (uncorrected); temps in C; tc (formerly boolean temp correction, now glass/plastic hydrometer)
# see: http://www.waldonell.com/me/reef-aquarium-water-parameters-salinity-density-and-specific-gravity
calcSalFromCorrectedSpGr <- function(spgr_reading, t_sample, t_standard, t_reference, tc) {
  
  # get uncorrected (ball-park) salinityy
  rSal <- calcSalFromSpGravity(spgr_reading, t_sample, t_standard)
  # cat('\n\nUNcorrected rSal = ', rSal, '\n')
  
  # for Gupta correction
  gamma <- 0.000030    # hydrometer coefficient of expansion (C^-1)
  
  gupta_correction <- spgr_reading * (calcRho(t_standard + 273.15, rSal) / 
                                        (calcRho(t_sample + 273.15, rSal) * (1 + gamma * (t_sample - t_standard)))
                                      - 1)
  
  rhf_corrected <- spgr_reading * calcRho(t_standard + 273.15, rSal) / calcRho(t_sample + 273.15, rSal)
  rhf_corrected_35 <- spgr_reading * calcRho(t_standard + 273.15, 35) / calcRho(t_sample + 273.15, 35)
  # cat('\n\n--------', calcRho(t_sample + 273.15, rSal) / calcRho(t_standard + 273.15, rSal),'\n\n')
  # cat('\n\nspgr_reading: ', spgr_reading, '  t_standard (C): ', t_standard, '  t_sample (C): ', t_sample, '\n\n')
  # cat('\ngupta_correction = ', gupta_correction, '\n\n')
  # cat('  gupta_corrected = ', spgr_reading + gupta_correction, '\n')
  # cat('    rhf_corrected = ', rhf_corrected, '\n')
  # cat(' rhf_corrected_35 = ', rhf_corrected_35, '\n\n')
  # cat('rSal: ', rSal, '\n\n')
  corrected_sal <- round(calcSalFromSpGravity(spgr_reading + gupta_correction, t_sample, t_standard), 2)
  # cat('         Corrected mySal = ', corrected_sal, '\n\n')
  # cat('     vs. Corrected mySal = ', round(calcSalFromSpGravity(rhf_corrected, t_sample, t_standard), 2), '\n\n')
  # cat('vs. Corrected (35) mySal = ', round(calcSalFromSpGravity(rhf_corrected_35, t_sample, t_standard), 2), '\n\n')
  
  
  # RE-calc gupta correction from updated salinity ?????
  gupta_correction_2 <- spgr_reading * (calcRho(t_standard + 273.15, corrected_sal) / 
                                          (calcRho(t_sample + 273.15, corrected_sal) * (1 + gamma * (t_sample - t_standard))) 
                                        - 1)
  
  # cat('\ngupta_correction_2 = ', gupta_correction_2, ' --> ', spgr_reading + gupta_correction_2, '\n\n')
  corrected_sal_2 <- round(calcSalFromSpGravity(spgr_reading + gupta_correction_2, t_sample, t_standard), 2)
  # cat('         Corrected mySal_2 = ', corrected_sal_2, '\n\n')
  
  # RE-RE-calc gupta correction from updated salinity ?????
  gupta_correction_3 <- spgr_reading * (calcRho(t_standard + 273.15, corrected_sal_2) / 
                                          (calcRho(t_sample + 273.15, corrected_sal_2) * (1 + gamma * (t_sample - t_standard))) 
                                        - 1)
  
  # cat('\ngupta_correction_3 = ', gupta_correction_3, ' --> ', spgr_reading + gupta_correction_3, '\n\n')
  corrected_sal_3 <- round(calcSalFromSpGravity(spgr_reading + gupta_correction_3, t_sample, t_standard), 2)
  # cat('         Corrected mySal_3 = ', corrected_sal_3, '\n\n')
  
  # RE-RE-RE-calc gupta correction from updated salinity ?????
  gupta_correction_4 <- spgr_reading * (calcRho(t_standard + 273.15, corrected_sal_3) / 
                                          (calcRho(t_sample + 273.15, corrected_sal_3) * (1 + gamma * (t_sample - t_standard))) 
                                        - 1)
  
  # cat('\ngupta_correction_4 = ', gupta_correction_4, ' --> ', spgr_reading + gupta_correction_4, '\n\n')
  corrected_sal_4 <- round(calcSalFromSpGravity(spgr_reading + gupta_correction_4, t_sample, t_standard), 2)
  # cat('         Corrected mySal_4 = ', corrected_sal_4, '\n\n')
  
  
  return(corrected_sal_4)
}

# http://www.code10.info/index.php?option=com_content&view=article&id=65:conversion-between-conductivity-and-pss-78-salinity&catid=54:cat_coding_algorithms_seawater&Itemid=79
#...valid within the temperature range between –2°C and +35°C, 
# pressure range between 0 and 10000 decibars and a practical salinity range between 2 and 42 
# or the respective electrical conductivity and conductivity ratio.
# Although practical salinity values < 2 are not defined, the equations deliver valid non-zero results 
# down to thresholds of conductivity ratios > 0.0005 and salinities > 0.02. 
# Values in these outer limits are estimates congruent with the Fortran algorithms (UNESCO 1983). 
# Below these thresholds functions return 0 (UNESCO 1983).

# t in C and p in dbar
calcSwSalinityFromConductivity <- function(c, t, p) {
  # cat('0: calc--Sw--SalinityFromConductivity ... passed in -- c: ', c, '\n')
  
  #            double sal = 0.0;
  # ** NB: Must add myPressure and convert myConductivity to S/m for this calc
  #            double t = myTemp;     // ** C
  #            double p = myPressure; // ** dbar
  
# NB: With this algorithm, MUST CHANGE INPUT temp from KELVIN --> CELCIUS
#     Unlike WQ Tech 0.95 in Xcode, this conversion performed in salToIcUnits()
  
  # ** @param c = C(S,t,p) is the measured electrical conductivity
  # ** myConductivity = C(35,15,0) = electrical conductivity of standard SW,
  # ** which is 4.2914 S/m
  # ** e.g.: http://www.code10.info/index.php?option=com_content&view=article&id=65:conversion-between-conductivity-and-pss-78-salinity&catid=54:cat_coding_algorithms_seawater&Itemid=79
  
  # cat('*** calcSwSalinityFromConductivity ... c: ', c, '\n')
  
  # ** S/m (or 42914 μS/cm) 
  myConductivity <- 4.2914
  
  # ** 1. calculate conductivity ratio R
  myR <- c / myConductivity
  
  # ** 2. calculate rsubt -- function of temperature
  c0 <- 0.6766097;      c1 <-  0.0200564
  c2 <- 0.0001104259;   c3 <- -0.00000069698
  c4 <- 0.0000000010031
  
  rSubT <- c0 + c1*t + c2*t*t + c3*t*t*t + c4*t*t*t*t
  
  # ** 3. calculate Rsubp -- function of pressure
  e0 <- 0.00002070;      e1 <- -0.0000000006370
  e2 <- 0.000000000000003989
  d1 <- 0.03426;     d2 <-  0.0004464
  d3 <- 0.4215;      d4 <- -0.003107
  
  # cat('*** AAAAA calcSwSalinityFromConductivity \n')
  RsubP <- 1 + p*(e0 + e1*p + e2*p*p) / (1 + d1*t + d2*t*t + (d3 + d4)* myR)
  # cat('*** BBBBB calcSwSalinityFromConductivity \n')
  
  # ** 4. calclate RsubT
  RsubT <- myR / (rSubT * RsubP)
  
  # ** 5. calculate S'
  k  <-  0.0162
  b0 <-  0.0005; b1 <- -0.0056; b2 <- -0.0066
  b3 <- -0.0375; b4 <-  0.0636; b5 <- -0.0144
  
  Sprime <- (t - 15) / (1 + k*(t - 15))
  
  # cat('*** calcSwSalinityFromConductivity ... RsubT: ', RsubT, '\n')
  
  Sprime <- Sprime * (b0 + b1*sqrt(RsubT) + b2*RsubT + 
                        b3*RsubT^(1.5) + 
                        b4*RsubT*RsubT + b5*RsubT^(2.5))
  # cat('bbbbbbbbbbb??')
  # cat('*** calcSwSalinityFromConductivity ... Sprime: ', Sprime, '\n')
  
  # ** 6. finish it off to calc salinity
  a0 <-  0.0080; a1 <- -0.1692; a2 <- 25.385
  a3 <- 14.0941; a4 <- -7.0261; a5 <-  2.7081
  
  mySal <- a0 + a1 * sqrt(RsubT) + 
  a2 * RsubT + a3 * RsubT^(1.5) + 
  a4 * RsubT*RsubT + a5 * RsubT^(2.5) + 
  Sprime
  
  return(mySal)
}


# use divide-and-conquer
# return S/m
calcConductivityFromSalinity <- function(sal, temp, p) {
  
  # cat('0: calcConductivityFromSalinity ... passed in: ', sal, ' -- temp: ', temp, '\n')
  
  tol <- 0.00001
  
  low <- 0
  high <- 12   # NB: NB: NB: 'high' must be >(=) highest expected/computed value
  
  c <- (low + high) / 2
  
  # cat('FIRST c: ', c,'\n')
  
  guess_sal <- calcSwSalinityFromConductivity(c, temp, p)
  
  while(abs(sal - guess_sal) >= tol) {
    # cat(' IN guess_sal: ', guess_sal,'\n')
    if(sal > guess_sal)
      low <- c
    else
      high <- c
    
    c <- (low + high) / 2
    
    # cat('B: (', low, ' + ', high, ') / 2 = ', (low + high) / 2, '\n')
    guess_sal <- calcSwSalinityFromConductivity(c, temp, p)
    # cat('OUT guess_sal: ', guess_sal,'\n\n')
    # cat('D: ', (sal - guess_sal),'\n\n')
  }
  
  # cat('\n******************************************\n')
  # cat('Conductivity ', round(c, 3),' S/m\n')
  # cat('******************************************\n')
  
  return(c)
}


# currently return UNcorrected hydrometer reading
calcHydrometerReadingFromSalinity <- function(sal, tSample, tStandard) {
  
  # NB: [KLUDGE] x 2 next two lines...
  tc <- 0
  tReference <- tStandard
  
  
  tol <- 0.000001
  
  
  low <- 0.800
  high <- 1.100
  
  c <- (low + high) / 2
  
  # Gupta correction
  gamma <- 0.000030    # hydrometer coefficient of expansion (C^-1)
  
  gupta_correction_factor <- (calcRho(tStandard + 273.15, sal) / 
                             (calcRho(tSample + 273.15, sal) * (1 + gamma * (tSample - tStandard)))
                           - 1)
  
  # cat('     gupta_correction_factor: ', gupta_correction_factor, '\n')
  
  # get uncorrected (ball-park) salinity
  guess_sal <- calcSalFromSpGravity(c * (1 + gupta_correction_factor), tSample, tStandard)
  # cat('\n\nUNcorrected guess_sal = ', guess_sal, '\n')
  
  while(abs(sal - guess_sal) >= tol) {
    # cat('A: ', guess_sal,'\n')
    if(sal > guess_sal)
      low <- c
    else
      high <- c
    
    c <- (low + high) / 2
    
    # cat('c = (', low, ' + ', high, ') / 2 = ', (low + high) / 2, '\n')
    guess_sal <- calcSalFromSpGravity(c * (1 + gupta_correction_factor), tSample, tStandard)
    # cat('C: ', guess_sal,'\n')
    # cat('D: ', (sal - guess_sal),'\n\n')
  }
  
  minus_corrected_sal <- round(calcSalFromSpGravity(c * (1 + gupta_correction_factor), tSample, tStandard), 2)
  
  # cat('\n******************************************\n')
  # cat('UNcorrected HydrometerReading: ', round(c, 6), ' (', tReference, '/', tStandard, ')\n')
  # # cat(' -corrected HydrometerReading: ', round(c - gupta_correction, 6), ' (', tReference, '/', tStandard, ')\n')
  # cat('               guess Salinity: ', round(guess_sal, 4), '\n')
  # # cat('     minus_corrected Salinity: ', round(minus_corrected_sal, 4), '\n')
  # cat('******************************************\n')
  
  return(c)
}


# *****-------------------------------------------*****
givenSalAndTempCalcUncorrectedHydrometerReading <- function(sal, tSample, tStandard, tc) {
  
  spgr <- calcRho(tSample + 273.15, sal)/calcRhoFW(tStandard + 273.15)
  
  gupta <- ifelse(tc, 0, gupta_correction(sal, tSample, tStandard))
  # return(spgr - gupta_correction(sal, tSample, tStandard))
  
  return(spgr - gupta)
}

# Gupta correction for hydrometer
gupta_correction <- function(sal, tSample, tStandard) {
  # Gupta correction
  gamma <- 0.000030    # hydrometer coefficient of expansion (C^-1)
  
  gupta_correction_factor <- (calcRho(tStandard + 273.15, sal) / 
                                (calcRho(tSample + 273.15, sal) * (1 + gamma * (tSample - tStandard)))
                              - 1)
  
  return(gupta_correction_factor)
}
# *****-------------------------------------------*****
  

# // ** TEST THIS METHOD TO CALC SALINITY from Sp. Gr.
# // ** NB0: Must CHANGE TEMP for K (IC units) to Celcius
# // ** NB1: changed params of original method in WqCalc
# // **      from NSString to double
# // ** NB2: 77 F / 77 F => (ref temp) / (standard temp)
# // ** NB3: MUST trap case in which, for a given sample temp, a sp. gr. reading
# // **      yields a salinity < 0.0

# @tc       - temp correction flag for hydrometer conversions

# *******
# ******* NB: 09-IX-2016, [CHANGED] order TO "tReference, tStandard"
# *******

calcSalOfSpGr <- function(spgr, tSample, tReference, tStandard, tc) {
  
  # cat('tc: ', tc, ' ... class(tc): ', class(tc),'\n')
  
# NB: With this algorithm, MUST CHANGE INPUT temp from KELVIN --> CELCIUS
#     Unlike WQ Tech 0.95 in Xcode, this conversion performed in salToIcUnits()
#     (same with calcRhoFW and within calcSalFromSpGravity)
         
# calc densities at sample, reference, & standard tempertures
# ** NB: in **THIS** implementation (vs. WqCalc), must add back 273.15 to pass Kelvin temp...
    rhoSample     <- calcRhoFW(tSample + 273.15)		  # should be 0.996542 at 27C??
    rhoReference  <- calcRhoFW(tReference + 273.15)		# should be 0.998232 at 20C??
#	rhoStandard   = [carbCalc calcRhoFW:(tempStandard + 273.15)];		// should be  at C??

# cat('in CALC_SAL_OF_SPGR, SPGR:',spgr,'\n')
# cat('in CALC_SAL_OF_SPGR, temp:',tSample,'...',tStandard,'...',tReference,'\n')
# cat('in CALC_SAL_OF_SPGR, rho:',rhoSample,'...',rhoReference,'\n')
         
    # 1: correct hydrometer reading with REFERENCE temp, if required
         # if (tc) {
           
          gamma <- 0.000030    # hydrometer coefficient of expansion (C^-1)
         # ** check data correction: +0.0015 for sp. gr. hydrometer; -0.0002 for density hydrometer
          
          c <- spgr * (rhoReference / (rhoSample * (1 + gamma * (tSample - tReference))) - 1)
          c_2 <- spgr * ((rhoReference / rhoSample) * (1 + gamma * (tSample - tReference)) - 1)
          
          cat('-------------\n')
          # cat('                             c = ', c, '\n')
          # cat('                           c_2 = ', c_2, '\n')
          cat('                       tSample = ', tSample, '\n')
          # cat('                    tReference = ', tReference, '\n')
          cat('                     rhoSample = ', rhoSample, '\n')
          # cat('                  rhoReference = ', rhoReference, '\n')
          # cat('    (rhoSample / rhoReference) = ', (rhoSample / rhoReference), '\n')
          samp_sal <- 30
          cat('                   rhoSample_s = ', calcRho(tSample + 273.15, samp_sal), '\n')
          cat('                rhoReference_s = ', calcRho(tReference + 273.15, samp_sal), '\n')
          cat('  (rhoSample / rhoReference)_s = ', (calcRho(tSample + 273.15, samp_sal) / 
                                                      calcRho(tReference + 273.15, samp_sal)), '\n')
          # cat('gamma * (tSample - tReference) = ', gamma * (tSample - tReference), '\n')
          cat('-------------\n')
         #   Rho  = -rhoReference * gamma * (tempSample - tempReference) / 1000; // mult by 1000 to express rho in g/ml
          # spgr <- spgr + c
          
         # }
         
    # 2: calc sample salinity with CarbCalc method(s)
# ** [KLUDGE] **
# ** to 'help' VALIDATION & CONVERSION play nice...?

      # if(spgr >= 0.99 || spgr <= 1.0399)
      #    mySal <- calcSalFromSpGravity(spgr, tSample, tStandard)
      # else
      #    mySal <- 0.0
         
# NB: [KLUDGE] validate input to avoid returning mySal < 0...
    #      if (mySal < 0 || mySal > 45) 
    #        mySal <- 0
    # 
    # cat('mySal: ', mySal, '\n')
    # 
    # return(mySal)
    return(30)
}
         
# ** NB: NB: NB: Temp sent in CELCIUS, *BUT* when send to calcRhoFW:,
# **             now must send in KELVIN (unlike implementation in WqCalc project)

# *******
# ******* NB: 09-IX-2016, [CHANGED] tempCal TO tempRef
# *******

calcSalFromSpGravity <- function(spGrav, tempRead, tempRef) {
  
  A  <-   0.824493 - 0.0040899 * tempRead
  A  <- A +   0.000076438 * tempRead * tempRead
  A  <- A +  -0.00000082467 * tempRead^3
  A  <- A +   0.0000000053875 * tempRead^4
  
  B  <-  -0.00572466 + 0.00010227 * tempRead
  B  <- B +  -0.0000016546 * tempRead * tempRead
  
  C  <-   0.00048314
 
  mySal <- calcSecantMethod(C, 
                            B, 
                            A, 
                            0.0, 
                            calcRhoFW(tempRead + 273.15) - calcRhoFW(tempRef + 273.15) * spGrav)
  
  mySal <- mySal * mySal
  
  # cat('in CALC_SAL_**FROM**_SPGR:',spGrav,'...sample temp',tempRead,'...calibration temp',tempCal,'\n')
  # cat('...salinity = ',mySal,'\n')
    
  return(mySal)
}

# -------------

# 1. spGrav / calcRowFW(tempRef) = UNcorrected SW density = rho_uncorrected_MEASURED
# 2. rho_uncorrected_NEW <- calcRho(tempRead + 273.15, guess_sal)
# -- refine guess_sal in while()-loop --
# -- LOOP TEST(s): (1) abs(rho_uncorrected_MEASURED - rho_uncorrected_NEW) >= tol
#                  (2) guess_sal > 10^(-1)
# -- guess_sal adjustment...
#                           if rho_uncorrected_NEW < rho_uncorrected_MEASURED, then 'new' low = guess_sal
#                           else 

calcSalFromSpGravity_bisection <- function(spgr_uncorrected_MEASURED, tempRead, tempCal) {
  
  # 0. define tolerance and compute first (rough) guess_sal
  tol <- 0.00001
  
  low <- 0
  high <- 80   # NB: NB: NB: 'high' must be >(=) highest expected/computed value
  guess_sal <- (low + high) / 2
  
  standardRho <- calcRhoFW(tempCal + 273.15)
  
  spgr_crit_low <- calcRho(42 + 273.15, 0) / standardRho
  spgr_crit_high <- calcRho(4 + 273.15, 0) / standardRho
  cat('\n-----------------\n')
  # cat('spgr_crit(0 ppt, ', tempRead, 'C, at ', tempRef, 'C) = ', round(spgr_crit, 6), '\n')
  # cat('gupta_correction_crit: ', round(gupta_correction(0, tempRead, tempRef), 5), '\n')
  cat('-----------------\n')
  
  if(spgr_uncorrected_MEASURED <= spgr_crit_low && 
     spgr_uncorrected_MEASURED >= spgr_crit_high)
    return('NEMA!')
  
  spgr_uncorrected_NEW <- calcRho(tempRead + 273.15, guess_sal) / standardRho
  # cat('FIRST spgr_uncorrected_NEW: ', spgr_uncorrected_NEW,'\n')
  
  gupta <- gupta_correction(guess_sal, tempRead, tempCal)
  
  while(abs(spgr_uncorrected_NEW - spgr_uncorrected_MEASURED) >= tol  &&  guess_sal > 10^(-1)) {
    # cat(' IN guess_sal: ', guess_sal,'\n')
    
    if(spgr_uncorrected_NEW < spgr_uncorrected_MEASURED)
      low <- guess_sal
    else
      high <- guess_sal
    
    guess_sal <- (low + high) / 2
    
    # cat('B: (', low, ' + ', high, ') / 2 = ', (low + high) / 2, '\n')
    spgr_uncorrected_NEW <- calcRho(tempRead + 273.15, guess_sal) / standardRho
  }
  
  # cat('\n******************************************\n')
  # cat('Sal from Spgr ', round(guess_sal, 3), ' (', round(spgr_uncorrected_NEW, 5), ')\n')
  # cat('gupta_correction: ', round(gupta, 5), ' --> ', round(spgr_uncorrected_NEW + gupta, 5), '\n')
  # cat('******************************************\n')
  
  return(guess_sal)
}


# [TEST]
# given salinity and temperature for a hydrometer calibration temperature,
# calculate the TRUE spgr, the "gupta" correction, and the UNcorrected hydrometer reading

# tc (boolean) flags if entered spgr_in is un-corrected (tc = 0) or corrected (tc = 1)
test_spgr_calcs <- function(spgr_in, tRead, tCal, tc) {
  
  # spgr_TRUE <- calcRho(tRead + 273.15, sal) / calcRhoFW(tCal + 273.15)
  # 
  # gupta <- gupta_correction(sal, tRead, tCal)
  # 
  # cat('\n===================\n')
  # cat('spgr_TRUE: ', spgr_TRUE, '\n')
  # cat('    gupta: ', gupta, '\n')
  # cat(' spgr_RAW: ', spgr_TRUE - gupta, '\n')
  # cat('===================\n\n')
  
  low <- 0
  high <- 50
  my_sal <- (low + high) / 2
  
  spgr_TRUE <- calcRho(tRead + 273.15, my_sal) / calcRhoFW(tCal + 273.15)
  # gupta <- gupta_correction(my_sal, tRead, tCal)
  gupta <- ifelse(tc, 0, gupta_correction(my_sal, tRead, tCal))
  
  # spgr_target <- 1.0220
  
  while(abs(spgr_in - (spgr_TRUE - gupta)) >= 0.0000001) {
    
    if(spgr_in > (spgr_TRUE - gupta))
      low <- my_sal
    else
      high <- my_sal
    
    my_sal <- (low + high) / 2
    
    spgr_TRUE <- calcRho(tRead + 273.15, my_sal) / calcRhoFW(tCal + 273.15)
    # gupta <- gupta_correction(my_sal, tRead, tCal)
    gupta <- ifelse(tc, 0, gupta_correction(my_sal, tRead, tCal))
  }
  
  # cat('\n===================\n')
  # cat('spgr_TRUE: ', spgr_TRUE, ' (', round(spgr_TRUE, 4), ')\n')
  # cat('    gupta: ', gupta, '\n')
  # cat(' spgr_RAW: ', spgr_TRUE - gupta, ' (', round(spgr_TRUE - gupta, 4), ')\n')
  # cat('   my_sal: ', my_sal, ' (', round(my_sal, 2), ')\n')
  # cat('    CHECK: ', calcRho(tRead + 273.15, my_sal) / calcRhoFW(tCal + 273.15), '\n')
  # cat('===================\n')

  return(my_sal)
}


# ---- GASSES ----
# see: converter_gasses.R



# ---- LENGTH ----

# ---- vectors used in length conversions
fractionsAsDecimals <- c(0.0000, 0.0625, 0.1250, 0.1875, 0.2500, 0.3125, 0.3750, 0.4375, 0.5000,
                         0.5625, 0.6250, 0.6875, 0.7500, 0.8125, 0.8750, 0.9375, 1.0000)
fractionsAsLiteral <- c('0', '1/16', '1/8', '3/16', '1/4', '5/16', '3/8', '7/16', '1/2',
                        '9/16', '5/8', '11/16', '3/4', '13/16', '7/8', '15/16', '1')



# ---- Convert meters to ft & in
# return: c(ansFtIn, ansYdFtIn)
convertMetersToFtAndInches <- function(meters) {
  
  ## numericInput field cleared or '0'
  # if(is.na(meters) | 0 == length(meters) | meters == 0) { 
  if(is.na(meters) | 0 == length(meters) | meters == '') { 
    return(c(NA, NA)) 
  }
  
  ansDecimalFt <- meters * 3.280839895                 ## convert meters to ft, ans is decimal feet
  
  ansWholeFt <- floor(ansDecimalFt)                    ## get whole number of ft
  
  ansDecimalInch <- 12 * (ansDecimalFt - ansWholeFt)   ## calc decimal inches
  
  ansWholeInch <- floor(ansDecimalInch)                ## get whole number of inches
  
  ansFractionalInch <- ansDecimalInch - ansWholeInch   ## get remaining 'fractional' part of inches
  
    # cat('           meters:',meters,'\n')
    # cat('     ansDecimalFt:',ansDecimalFt,'\n')
    # cat('       ansWholeFt:',ansWholeFt,'\n')
    # cat('   ansDecimalInch:',ansDecimalInch,'\n')
    # cat('     ansWholeInch:',ansWholeInch,'\n')
    # cat('ansFractionalInch:',ansFractionalInch,'\n')
    # cat('ansFractionalInch:',sprintf('%.54f',ansFractionalInch),'\n')
    # cat('======================================\n')
  
  # which decimal expression of 16ths of an inch is >= the calculated ansFracionalInch
  #   cat('     which(fractionsAsDecimals >= ansFractionalInch):',
  #       which(fractionsAsDecimals >= ansFractionalInch),'\n')
  #   cat('min(which(fractionsAsDecimals >= ansFractionalInch)):',
  #       min(which(fractionsAsDecimals >= ansFractionalInch)),'\n')
  
  nextHighestSixteenthIdx <- min(which(fractionsAsDecimals >= ansFractionalInch))
  nextHighestSixteenthVal <- fractionsAsDecimals[nextHighestSixteenthIdx]
  
  # adjust output to account for cases in which inch rounds to 1" and/or 12:
  
  if(nextHighestSixteenthVal == '1') {
    ansWholeInch = ansWholeInch + 1
    nextHighestSixteenthIdx = 0
  }
  
  if(ansWholeInch == 12) {
    ansWholeFt = ansWholeFt + 1
    ansWholeInch = 0
  }
  
    # cat('                                                   input meters:',meters,'\n')
    # cat('                                              ansFractionalInch:',ansFractionalInch,'\n')
    # cat('                                        nextHighestSixteenthIdx:',nextHighestSixteenthIdx,'\n')
    # cat('                                    nextHighestSixteenthIdx - 1:',nextHighestSixteenthIdx - 1,'\n')
    # cat('                         fractions[nextHighestSixteenthIdx - 1]:',fractionsAsDecimals[nextHighestSixteenthIdx - 1],'\n')
  #   cat('                             fractions[nextHighestSixteenthIdx]:',fractionsAsDecimals[nextHighestSixteenthIdx],'\n')
  #   cat('abs(ansFractionalInch - fractions[nextHighestSixteenthIdx - 1]):',abs(ansFractionalInch - fractionsAsDecimals[nextHighestSixteenthIdx - 1]),'\n')
  #   cat('    abs(ansFractionalInch - fractions[nextHighestSixteenthIdx]):',abs(ansFractionalInch - fractionsAsDecimals[nextHighestSixteenthIdx]),'\n')
  
  
  # cat('nextHighestSixteenthIdx:', nextHighestSixteenthIdx, '\n')
  
  # NB: [R] is not zero-indexed
  # if (nextHighestSixteenthIdx != 0) {
  if (nextHighestSixteenthIdx != 1 && nextHighestSixteenthIdx != 0) {
    distanceToLL <- abs(ansFractionalInch - fractionsAsDecimals[nextHighestSixteenthIdx - 1])
    distanceToUL <- abs(ansFractionalInch - fractionsAsDecimals[nextHighestSixteenthIdx])
    
        # cat('diff with LL:', distanceToLL, '\n')
        # cat('diff with UL:', distanceToUL, '\n')
    
    # as default, round up to UL
    roundToIdx <- nextHighestSixteenthIdx
    if((distanceToLL < distanceToUL) & (nextHighestSixteenthIdx != 0 | nextHighestSixteenthIdx != 1))
      roundToIdx <- roundToIdx - 1
  } else {
    roundToIdx <- 0
  }
  
  #   cat('.....\n')
  #   cat(nextHighestSixteenthVal,'>=',ansFractionalInch,'\n')
  #   cat(ansFractionalInch,'is between',fractionsAsLiteral[nextHighestSixteenthIdx - 1],
  #       'and',fractionsAsLiteral[nextHighestSixteenthIdx],'of an inch\n\n')
  #   
  #   cat(meters,'m =',ansWholeFt,"'",ansWholeInch,fractionsAsLiteral[roundToIdx],'"\n')
  
  # ansFtIn <- paste(ansWholeFt,"' ",ansWholeInch,"  ",fractionsAsLiteral[roundToIdx],"\"")
  ansFtIn <- paste0(ansWholeFt,' ft  ',ansWholeInch,' ',fractionsAsLiteral[roundToIdx],' in')
  
  ansIn <- paste0(ansWholeFt * 12 + ansWholeInch,' ',fractionsAsLiteral[roundToIdx],' in')
  
  
  #   cat('ansWholeFt / 3 =',ansWholeFt / 3,'\n')
  #   cat('floor(ansWholeFt / 3() =',floor(ansWholeFt / 3),'\n')
  
  ansWholeYd <- floor(ansWholeFt / 3)
  ansWholeFt <- ansWholeFt - (3 * ansWholeYd)
  ansYdFtIn <- paste0(ansWholeYd,' yd  ',ansWholeFt,' ft  ',ansWholeInch,' ',fractionsAsLiteral[roundToIdx],' in')
  
  # cat('ansFtIn:',ansFtIn,'  ansYdFtIn:',ansYdFtIn,'\n')
  
  return(c(ansFtIn, ansYdFtIn, ansIn))
}



# ---- Convert (YD,) ft & in to meters
# NB: decimalInch is a string
convertYdFtAndInchesToMeters <- function(wholeYd, 
                                         wholeFt, 
                                         wholeInch, decimalInch) {
  
  # cat('\n\n\nConverting YARDS, feet, and inches to meters (icUnits)\n')
  # print(wholeYd) ; print(wholeFt) ; print(wholeInch) ; print(decimalInch)
  idxDecimal <- which(fractionsAsLiteral == decimalInch)
  # cat('index of', decimalInch, 'in',fractionsAsLiteral,'is',idxDecimal, '\n')
  decimalInch <- fractionsAsDecimals[idxDecimal]
  
  wholeFtTotal <- 3 * wholeYd + wholeFt
  
  metersFromWholeFt <- wholeFtTotal / 3.280839895
  
  metersFromWholeInch <- (wholeInch / 12) / 3.280839895
  
  metersFromDecimalInch <- (decimalInch / 12) / 3.280839895
  
  #     print('--------------------------------------\n')
  #     print('--------------------------------------\n')
  #     print(fractionsAsDecimals)
  #     print(decimalInch)
  
  #     print(fractionsAsDecimals[2])
  #     print(fractionsAsDecimals[16])
  #     cat('fractionsAsDecimals[', decimalInch, '] =', fractionsAsDecimals[idxDecimal], '\n')
  #     cat('wholeFtTotal:', wholeFtTotal, '\n\n')
  #     cat(metersFromWholeFt,'+',metersFromWholeInch,'+',metersFromDecimalInch,'=\n')
  #     cat(metersFromWholeFt + metersFromWholeInch + metersFromDecimalInch,'\n')
  #     print('--------------------------------------******\n')
  #     print('--------------------------------------******\n')
  
  return(metersFromWholeFt + metersFromWholeInch + metersFromDecimalInch)
}
