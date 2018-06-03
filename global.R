# global header for iQuacalc (Lite)

suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))

suppressMessages(library(shinyjs))
suppressMessages(library(V8))      # support extendShinyjs() of shinyjs see: http://deanattali.com/2015/05/31/shinyjs-extend/

suppressMessages(library(shinyStore))

suppressMessages(library(DT))

# suppressMessages(library(highcharter))  # interactive chart -- probably use ggvis instead
# library(plotly)

# library(FSA)           # for logbtcf() to correct allometric back-transformation

# suppressMessages(library(car))           # for bootCase() in lw_upload_and_analyze's non-linear prediction limits
# suppressMessages(library(propagate))     # predictNLS() in lw_upload_and_analyze's non-linear prediction limits


# NB: dplyr's 'select()' masked by MASS loaded with propagate, 
#     so qualify as dplyr::select in converter_gasses.R
#     UNLESS...load *after* MASS is loaded...?
suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
# library(stringi)
# library(dplyr)         # NB: Is this installed on {shiny} server?
# library(tidyr)         # NB: Is this installed on {shiny} server?
# library(purrr)         # use map_df() to facilitate ggplot-ing the WQ Map family of pH isopleths



source('CarbCalc.R')

source('converters/converter.R')
source('converters/converter_gasses.R')
source('converters/validator_for_iquacalc.R')
source('converters/scratchpad_converter.R')     # type (recognize) text for one conversion

source('modules/conversion_display_module.R')  # display conversion result of clicked table cell

source('modules/wq_map_module.R')

source('adjustReagents.R')
source('modal_dlgs/modal_dlg_feed.R')
# source('www/cbToggleAntics.js')

source('modules/volume_numeric_input_module.R')

source('modules/temperature_module.R')
source('modules/temperature_numeric_input_module.R')

source('modules/salinity_module.R')
source('modules/salinity_numeric_input_module.R')

source('modules/ph_module.R')
source('modules/ph_numeric_input_module.R')

source('modules/alk_module.R')
source('modules/alk_initial_waypoint_module.R')
source('modules/alk_target_waypoint_module.R')
source('modules/alk_green_zone_module.R')

source('modules/ca_numeric_input_module.R')

source('modules/length_module_js.R')
source('modules/length_module_js_uioutput.R')
source('modules/length_module_js_uioutput_TWO.R')

source('modules/depth_module.R')

source('modules/area_module.R')
source('modules/volume_module.R')

source('modules/biomass_module.R')
source('modules/abundance_module.R')

source('modules/flow_rate_module.R')

source('modules/hydraulic_load_module.R')

source('modules/gas_sat_module_WITH_MODULES.R')
source('modules/barometric_module.R')
source('modules/barometric_numeric_module.R')

source('modules/gas_module.R')       # for O2, N2, Ar
source('modules/co2_gas_module.R')
source('modules/co2_gas_atm_module.R')

source('modules/gas_tgp_module.R')   # stand-alone TGP calc [deprecate tgp_module_WITH_MODULES.R]
source('modules/gas_sat_module.R')   # display O2, N2, Ar, CO2 saturation
source('modules/o2_duration_module.R')

source('modules/gas_conversion_module.R')

source('modules/uia_module_HC_2.R')
source('modules/tan_module.R')
source('modules/tan_conversion_module.R')

source('modules/tan_numeric_input_module.R')
source('modules/uia_numeric_input_module.R')

source('modules/co2_dissolved_numeric_input_module.R')  # for WQ Map
source('modules/co2_measured_numeric_input_module.R')   # for TGP

source('modules/disinfection_module.R')

source('modules/upload_and_analyze_lw.R')
source('modules/upload_and_analyze_cor.R')

source('modules/scratchpad_module.R')

source('modules/data_table_module.R')
source('modules/data_table_HORIZ_module.R')
source('modules/data_table_SPLIT_module.R')

source('modules/multiplier_module.R')


# trying to pack conversion info for each unit conversion into a map with
# key = string & value = list of two lists: c(xUnits, xFactors)
# (no luck yet)

# from: http://r.789695.n4.nabble.com/how-to-convert-string-to-object-td2999281.html
# text <- 'xFactors' 
# parse(text=text) 
# f <- eval(parse(text=text))
# f[idx]

# ---- list of lists ----
# from: http://stackoverflow.com/questions/10776742/r-how-make-list-of-lists-in-r
# > l.1 <- list(1, 2, 3)
# > l.2 <- list('a', 'b', 'c')
# > L <- list(a = l.1, b = l.2)
# > L$a
# > L$a[2]

# So...
# vector of ordered conversion types:
#         conversionTypes <- c('length', 'area', 'volume', 'temperatue', ... )
# list of units and factors for each conversion type:
#         vol = list(volUnits, volFactors)
# conversionData <- list(length = list(lenUnits, lenFactors),
#                        area = list(areaUnits, areaFactors), ... )
#
# then call conversion function with (fromVal, fromUnits, toUnits, type)
#     where type = 'length', 'volume', etc.
#
# ...or...
#
# each conversion type (e.g., 'vol', 'temp', ...) is a list of two vectors,
# e.g., vol <- c(volUnits, volFactors) and temp <- c(tempUnits, tempFactors)
# then, this type (e.g., 'vol') is sent to the converter as a param, within which it is unpacked...
#   * fromIdx <- which(fromUnits == type$units)
#   * toIdx <- which(toUnits == type$units)
#   * icFromVal <-  fromVal / type#factors[fromIdx]
#   * return(icFromVal * type#factors[toIdx])


# ---- Adjustment REAGENTS ----

# define the df that relates slopes (meq/mmol) to reagents ----
# reagents  <- c('nahco3',  'na2co3',   'naoh', 
#                'caco3',   'caoh2',    'cao', 
#                'plusCo2', 'minusCo2', 'hcl')
# 
# cmpds     <- c('NaHCO\u2083', 'Na\u2082CO\u2083', 'NaOH', 
#                'CaCO\u2083',  'Ca(OH)\u2082',     'CaO', 
#                '+CO\u2082',   '-CO\u2082',        'HCl')
# 
# slopes    <- c(1, 2, 1000000, 2, 1000000, 1000000, 0, 0, 1000000)
# radSlopes <- c(pi / 4, atan(2), pi / 2, atan(2), pi / 2, pi / 2, 0, pi, 3 * pi / 2)    # used to capture direction
# sinTheta  <- round(cos(radSlopes), 6)
# cosTheta  <- round(sin(radSlopes), 6)
# 
# reagent.df<-data.frame(name = as.character(reagents),
#                        cmpd = cmpds,
#                        m    = slopes,
#                        mRad = radSlopes,
#                        sin  = sinTheta,
#                        cos  = cosTheta,
#                        
#                        stringsAsFactors = F)

# *****************

names <- c('nahco3',      'na2co3',           'naoh', 'caco3',      'caoh2',        'cao', 'plusCo2',   'minusCo2',  'hcl')
cmpds <- c('NaHCO\u2083', 'Na\u2082CO\u2083', 'NaOH', 'CaCO\u2083', 'Ca(OH)\u2082', 'CaO', '+CO\u2082', '-CO\u2082', 'HCl')
mws   <- c(84.00661, 105.98844, 39.99711, 100.0869, 74.09268, 56.0774, 44.0096, 44.0096, 36.46094)
meq_mmol  <- c(1, 2, 1, 2, 2, 2, 0, 0, 1)
slopes    <- c(1, 2, 1000000, 2, 1000000, 1000000, 0, 0, 1000000)  # NB: m for HCl *not* negative
radSlopes <- c(pi / 4, atan(2), pi / 2, atan(2), pi / 2, pi / 2, 0, pi, 3 * pi / 2)
sinTheta  <- round(cos(radSlopes), 6)
cosTheta  <- round(sin(radSlopes), 6)


reagent_data <- tibble(name      = names,
                       cmpd      = cmpds,
                       mw        = mws,
                       meq_mmol  = meq_mmol,
                       m         = slopes,
                       mRad      = radSlopes,
                       sin       = sinTheta,
                       cos       = cosTheta
)



# ---- LENGTH ----

# NB: for selectInput choices in 'new' (i.e., horizontal input) Length conversion
#     user has 4 entry-units options, but 8 result units ('lengthUnits')
# lengthUnitsChoices <- c('m', 'cm', 'mm', 'yd, ft, in')
# lengthUnitsChoices <- c('m', 'cm', 'mm', 'yards')

# lengthUnitsChoices <- c('meters', 'centimeters', 'millimeters', 'yd, ft, in' = 'yards')
# lengthUnitsChoices <- c('m', 'cm', 'mm', 'yd, ft, in' = 'yards')
lengthUnitsChoices <- c('m', 'cm', 'mm', 'yards')

# NB: to LABEL length numericInput labels 
lengthUnitsChoices_Short <- c('meters', 'centimeters', 'millimeters', 'yards')

lengthUnits <- c("m", "cm", "mm",
                 "yd, ft, & in",
                 "ft & in",
                 "in",
                 "yd (decimal)", "ft (decimal)", "in (decimal)")

# NB: 777.777 & 888.888 are dummy flags for a loop in converter.R/convertAll()
lengthFactors <- c(1.0, 100.0, 1000.0,
                   888.888,             # [KLUDGE] to tag 'yd, ft, in' units
                   777.777,             # [KLUDGE] to tag 'ft, in' units
                   787.878,             # [KLUDGE] to tag 'in' units
                   1.0936133, 3.280839895, 39.37007874)

# HOW ... modify for "yd, ft, & in" ?
lengthMin  <- c(rep(0, 7))
lengthMax  <- c(rep(1000, 5), 5000, 10000)
lengthStep <- c(rep(0.1, 7))
lengthSet  <- c(rep(1, 7))

fractionalInches <- c('0', '1/16', '1/8', '3/16', '1/4',
                      '5/16', '3/8', '7/16', '1/2',
                      '9/16', '5/8', '11/16', '3/4',
                      '13/16', '7/8', '15/16')

# NB: Using 'lengthUnits instead of 'lengthUnitsChoices' here
#     the former with 8 entry units for calculation & datatable display, 
#     the latter used for 4 unit choices in selectInput
length.data <- list(units = lengthUnits, factors = lengthFactors, icUnits = 'm')


# ---- AREA ----

# areaUnitsChoices <- c("ha", "m²", "cm²", "mm²", "acre", "ft & in", "yd²", "ft²", "in²")
areaUnitsChoices <- c("ha", "m²", "cm²", "mm²", "acre", 'yd²', "ft²")

areaUnits <- c("ha","m²","cm²","mm²",
               "acre","yd²","ft²","in²")
               # "acre","ft & in","yd²","ft²","in²")
areaFactors <- c(1.0,10000.0,100000000.0,10000000000.0,
                 2.4710538147,11959.900463,107639.10417,15500031.0)
                 # 2.4710538147,1.0,11959.900463,107639.10417,15500031.0)

areaMin  <- c(rep(0, 7))
areaMax  <- c(rep(1000, 5), 5000, 10000)
areaStep <- c(rep(0.1, 7))
areaSet  <- c(rep(1, 7))

area.data <- list(units = areaUnits, factors = areaFactors, icUnits = 'ha')


# ---- VOLUME ----

volumeUnitsChoices <- c('milliliters (ml)', 'liters (L)', 'cubic meters (m\U00B3)',
                        'gallons (US)', 'gallons (UK)', 'cubic feet (ft\U00B3)', 'acre-ft')

volumeUnitsChoices_short <- c('ml', 'L', 'm\U00B3',
                              'gal (US)', 'gal (UK)', 
                              'ft\U00B3', 
                              'ac-ft')

# when ic volume units are LITERS, then toLiterFactors vector is...
# toLitersFactors <- c(0.001, 1.0, 1000.0, 3.785412, 4.54609)
toLitersFactors <- c(1000.0, 1.0, 0.001, 0.2641720, 0.2199692, 0.035314667, 0.0000008107)

volumeUnits <- c('milliliters (ml)', 'liters (L)', 'cubic meters (m\U00B3)',
              'gallons (US)', 'gallons (UK)', 'cubic feet (ft\U00B3)', 'acre-ft')
volumeFactors <- c(1000.0, 1.0, 0.001, 0.2641720, 0.2199692, 0.035314667, 0.0000008107)

volumeMin  <- c(rep(0, 7))
volumeMax  <- c(rep(10000, 7))
volumeStep <- c(rep(0.1, 7))
volumeSet  <- c(1000, 1000, 100, 3500, 3500, 500, 1)

volume.data <- list(units = volumeUnits, factors = volumeFactors, icUnits = 'liters (L)')

# *** *** *** for AREA x DEPTH = VOLUME *** *** ***
# used to switch selectInput choices conditioned on 'Area' or 'Depth' radio button
areaOrDepthUnitsChoices <- list(Area = area.data, Depth = length.data)


# ---- Biomass ----

# added: kg/ha, kg/acre, lb/ha, lb/acre ... kg/acre-ft, lb/acre-ft
biomassUnits <- c('kg/m²', 'g/m²', 'lb/m²', 'lb/ft²',
                  'kg/ha', 'kg/acre', 'lb/ha', 'lb/acre',
                  'kg/m³', 'g/L', 'lb/gal (US)', 'lb/gal (UK)', 'lb/ft³',
                  'kg/acre-ft', 'lb/acre-ft')

biomassFactors <- c(1.0, 1000.0, 2.2046226218, 0.20481614,
                    10000.0, 4046.8564, 22046.226218, 8921.791,
                    1.0, 1.0, 0.0083454044, 0.010022413, 0.062427961,
                    1233.48, 2719.358)

biomass.data <- list(units = biomassUnits, factors = biomassFactors, icUnits = 'kg/m²')


# ---- Abundance ----

abundanceUnits <- c('ind/m²', 'ind/ft²', 'ind/ha', 'ind/acre',
                    'ind/m³', 'ind/L', 'ind/gal (US)', 'ind/gal (UK)', 
                    'ind/ft³', 'ind/acre-ft')

abundanceFactors <- c(1.0, 0.09290313, 10000, 4046.781,
                      1.0, 0.001, 0.003785441, 0.004546074, 
                      0.02831682, 1233.502)

abundance.data <- list(units = abundanceUnits, factors = abundanceFactors, icUnits = 'ind/m²')


# ---- FLOW RATE ----
flowRateUnitsList <- c("m³/d", "liter/d", "gal/d (US)", "gal/d (UK)", "ft³/d", "acre-ft/d",
                       "m³/hr", "liter/hr", "gal/hr (US)", "gal/hr (UK)", "ft³/hr", "acre-ft/hr", 
                       "m³/min", "liter/min", "gal/min (US)", "gal/min (UK)", "ft³/min", "acre-ft/min", 
                       "m³/sec", "liter/sec", "gal/sec (US)", "gal/sec (UK)", "ft³/sec", "acre-ft/sec")

# (m³/day) / (m³/day), (liter/d) / (m³/day),  ...
# toCubicMetersPerDayFactors <- c(1.0, 1000.0, 264.17205236, 219.9692483, 35.314666213, 0.00081071319218,
#                                 0.041666666666, 41.666666666, 11.007168849, 9.1653853456, 1.4714444255, 0.000033779716341, 
#                                 0.00069444444443, 0.69444444443, 0.18345281414, 0.15275642243, 0.024524073759, 0.00000056299527233)

toCubicMetersPerDayFactors <- c(1.0,           1000.0,         264.17205236,  219.9692483,    35.314666213,    0.00081071319218, 
                                0.041666666666,  41.666666666,  11.007168849,   9.1653853456,  1.4714444255,   0.000033779716341, 
                                0.00069444444443, 0.69444444443, 0.18345281414, 0.15275642243, 0.024524073759, 0.00000056299527233,
                                0.00001157407,    0.01157407,    0.003057547,   0.002545940,   0.0004087346,   0.000000009383255)

flowUnits <- c("m³/d",   "liter/d",   "gal/d (US)",   "gal/d (UK)",   "ft³/d",   "acre-ft/d",
               "m³/hr",  "liter/hr",  "gal/hr (US)",  "gal/hr (UK)",  "ft³/hr",  "acre-ft/hr", 
               "m³/min", "liter/min", "gal/min (US)", "gal/min (UK)", "ft³/min", "acre-ft/min", 
               "m³/sec", "liter/sec", "gal/sec (US)", "gal/sec (UK)", "ft³/sec", "acre-ft/sec")

flowFactors <- c(1.0,           1000.0,         264.17205236,  219.9692483,    35.314666213,    0.00081071319218, 
                 0.041666666666,  41.666666666,  11.007168849,   9.1653853456,  1.4714444255,   0.000033779716341, 
                 0.00069444444443, 0.69444444443, 0.18345281414, 0.15275642243, 0.024524073759, 0.00000056299527233,
                 0.00001157407,    0.01157407,    0.003057547,   0.002545940,   0.0004087346,   0.000000009383255)

flow_rate.data <- list(units = flowUnits, factors = flowFactors, icUnits = 'm³/d')



# ---- HYDRAULIC LOADING ----

# *** I.C. Units, #11,   1.000000 (m³/d) / m²

hydraulicUnits <- c('(m³/s) / m²',   '(L/s) / m²',   '(gal (US)/s) / ft²',   '(gal (UK)/s) / ft²',   '(ft³/s) / ft²',
                    '(m³/min) / m²', '(L/min) / m²', '(gal (US)/min) / ft²', '(gal (UK)/min) / ft²', '(ft³/min) / ft²',
                    '(m³/hr) / m²',  '(L/hr) / m²',  '(gal (US)/hr / ft²',   '(gal (UK)/hr) / ft²',  '(ft³/hr) / ft²',
                    '(m³/d) / m²',   '(L/d) / m²',   '(gal (US)/d) / ft²',   '(gal (UK)/d) / ft²',   '(ft³/d) / ft²')

hydraulicFactors <- c(0.00001157407, 0.01157407, 0.0002840557, 0.0002365258, 0.00003797272,
                      0.0006944444,  0.6944444,  0.01704334,   0.01419155,   0.002278363, 
                      0.04166667,   41.66667,    1.022600,     0.8514930,    0.1367018, 
                      1.0,          10.00000,   24.54241,     20.43583,      3.280843)


hydraulic.data <- list(units = hydraulicUnits, factors = hydraulicFactors, icUnits = '(m³/d) / m²')



# ---- RHO ----

# rhoUnitsChoices <- c('rho_kg/L', 'g/L', 'MT/m\U00B3', 
#                      'lb/gal (US)', 'lb/gal (UK)')

rhoUnits <- c('kg/L', 'g/L', 'kg/m\U00B3', 'MT/m\U00B3', 
              'lb/gal (US)', 'lb/gal (UK)')
# "acre","ft & in","yd²","ft²","in²")
rhoFactors <- c(0.001, 1.0, 1.0, 0.001, 
                0.00834541126, 0.010022421)

rho.data <- list(units = rhoUnits, factors = rhoFactors, icUnits = 'g/L')




# ---- TEMPERATURE ----

tempUnitsList <- c('Celcius (C)', 'Fahrenheit (F)', 'Kelvin (K)')
tempUnitsList_short <- c('C', 'F', 'K')

tempMin  <- c(    4,    39.2,  277.15)
tempMax  <- c(   40,   105,    315)
tempStep <- c(    0.1,   0.1,    0.01)
tempSet  <- c(   28,    82,    301)



# ---- SALINITY ----

salUnitsList <- c('parts per thousand (‰)',
                  'conductivity: μS/cm (μmho/cm)','conductivity: S/m','conductivity: S/cm',
                  'conductivity: mS/cm (dS/m)',
                  # 'conductivity: mS/cm','conductivity: dS/m',
                  # 'hydrometer: 15C/4C','hydrometer: 20C/20C',
                  'hydrometer: 60F/60F',
                  'hydrometer: 20C/20C','hydrometer: 77F/77F')
salUnitsList_short <- c('‰ (ppt)',
                        # 'μS/cm','S/m', 'S/cm',
                        'μS/cm (μmho/cm)','S/m', 'S/cm',
                        # 'mS/cm',
                        'mS/cm (dS/m)',
                        # 'mS/cm','dS/m',
                        # '15C/4C','20C/20C',
                        '60F/60F',
                        '20C/20C',
                        '77F/77F')
# for numeric input label in WQ Map....
salUnitsList_short_est <- c('‰ (ppt)',
                            'μS/cm','S/m', 'S/cm',
                            # 'μS/cm (μmho/cm)','S/m', 'S/cm',
                            'mS/cm',
                            # 'mS/cm (dS/m)',
                            # 'mS/cm','dS/m',
                            # '15C/4C','20C/20C',
                            '60F/60F',
                            '20C/20C',
                            '77F/77F')

# below: removed 15C/4C and added S/cm
# salMin<-c(    0,      0,    0,      0,     0,  0.9900,   0.9900,   0.9900,  0.9900)
# salMax<-c(   45,  60000,    6,     60,    60,  1.0399,   1.0399,   1.0399,  1.0399)
# elements 6 - 9: hydrometer max w/OUT temp correction; 10 - 13, WITH temp correction;
# salMax<-c(   45,  60000,    6,     60,    60,  1.0235,   1.0235,   1.0235,  1.0260, 1.0170, 1.0170, 1.0170, 1.0200)
# salStep<-c( 0.1,    100, 0.01,    0.1,   0.1,  0.0001,   0.0001,   0.0001,  0.0001)
# salSet<-c(   30,  45000,  4.5,     45,    45,  1.0100,   1.0100,   1.0100,  1.0100)

salMin<-c(     0,   5000, 0.50,   0.0050,   5.0,     0.9990,  0.9990,  0.9990)
salMax<-c(    40,  60000,  6.5,    0.065,    65,     1.0360,  1.0340,  1.0380)
salStep<-c(    0.01, 100,  0.01,   0.001,     0.1,   0.0001,  0.0001,  0.0001)
# salStep<-c(    0.01, 100,  0.01,   0.001,     0.1,   0.0001,  0.0001,  0.0001)
salSet<-c(    30,  45000,  4.5,    0.045,    45,     1.0100,  1.0100,  1.0100)


# ---- [Alk] ----

# alkUnitsList <- c('meq/kg (mmol/kg)', 'meq/L (mmol/L)', 
#                   'ppm-m CaCO3 (mg/kg)', 'ppm-v CaCO3 (mg/L)', 
#                   'dKH')

alkUnitsList <- c('meq/kg (mmol/kg)', 'meq/L (mmol/L)', 
                  'mg/kg CaCO3', 'mg/L CaCO3', 
                  'dKH')

alkUnitsList_short <- c('meq/kg', 'meq/L',
                        'mg/kg CaCO3', 'mg/L CaCO3',
                        'dKH')

alkMin  <- c(    0,    0,        0,       0,    0)
alkMax  <- c(    8,    8,      450,     450,   25)
alkStep <- c(    0.01, 0.01,     0.1,     0.1,  0.1)
alkSet  <- c(    2.4,  2.4,    120,     120,    8.0)



# ---- TAN - UIA ----

# Nitrogen Constants
NH3  <- 17.031     # g NH3/mole
NH4  <- 18.038     # g NH4/mole
HNO3 <- 47.01344   # g HNO3/mole
NO3  <- 62.0049    # g NO3/mole
HNO2 <- 47.01344   # g HNO2/mole
NO2  <- 46.0055    # g NO2/mole
N    <- 14.00674   # g N/mole

H    <-  1.00794   # g H/mole
O    <- 15.9994    # g O/mole


# ammoniaUnitsList <- c('Celcius (C)', 'Fahrenheit (F)', 'Kelvin (K)')
# ammoniaUnitsList_short <- c('C', 'F', 'K')

critUiaUnitsList <- c('μg/L', 'μg/kg', 'mg/L', 'mg/kg', 
                      'μmol/L', 'μmol/kg', 'mmol/L', 'mmol/kg')
critUiaMin  <- c(    0,    0,    0,      0,      0,    0,    0,      0)
critUiaMax  <- c(   10,   10,    0.001,  0.001,  5,    5,    5,      5)
critUiaStep <- c(    0.01, 0.01, 0.001,  0.001,  0.01, 0.01, 0.01,   0.01)
critUiaSet  <- c(    0.02, 0.02, 0.025,  0.025,  0.02, 0.02, 0.025,  0.025)

tanUiaMin  <- c(rep(0, 36))
tanUiaMax  <- c(   10,   10,    100,  100,  150,    150, 5,      5)
tanUiaStep <- c(    0.01, 0.01, 0.001,  0.001,  0.01, 0.01, 0.01,   0.01)
tanUiaSet  <- c(    0.02, 0.02, 0.025,  0.025,  0.02, 0.02, 0.025,  0.025)


# IC Units: 'mg/L TA-N'
tanUnitsList <- c('mg/L TA-N',   'mg/kg TA-N',
                  'mmol/L TA-N', 'mmol/kg TA-N',
                  'μg/L TA-N',   'μg/kg TA-N',
                  'μmol/L TA-N', 'μmol/kg TA-N',
                  
                  'mg/L UIA-N',   'mg/kg UIA-N',
                  'mmol/L UIA-N', 'mmol/kg UIA-N',
                  'μg/L UIA-N',   'μg/kg UIA-N',
                  'μmol/L UIA-N', 'μmol/kg UIA-N',
                  # 'mg/L IA-N', 'mg/kg IA-N',
                  # 'μg/L IA-N', 'μg/kg IA-N',
                  # 'mmol/L IA-N', 'mmol/kg IA-N'
                  
                  'mg/L TA',   'mg/kg TA',
                  'mmol/L TA', 'mmol/kg TA',
                  'μg/L TA',   'μg/kg TA',
                  'μmol/L TA', 'μmol/kg TA',
                  
                  'mg/L UIA',   'mg/kg UIA',
                  'mmol/L UIA', 'mmol/kg UIA',
                  'μg/L UIA',   'μg/kg UIA',
                  'μmol/L UIA', 'μmol/kg UIA'
                  # 'mg/L IA', 'mg/kg IA',
                  # 'μg/L IA', 'μg/kg IA',
                  # 'mmol/L IA', 'mmol/kg IA'
                  )

tanUnitsMin  <- c(rep(0, 32))
tanUnitsMax  <- c(5, 5, 0.35, 0.35,  150,  150,  5.0,  5.0,    
                  3, 3, 0.20, 0.20,   40,   40,  2.0,  2.0,    
                  # 3, 3,  80,  80, 0.20, 0.20,
                  
                  6, 6, 0.40, 0.40,  160, 160, 4.0,  4.0,    
                  3, 3, 0.20, 0.20,   40,  40, 3.0,  3.0    
                  # 3, 3,  80,  80, 0.20, 0.20
                  )
tanUnitsStep <- c(0.01, 0.01, 0.001, 0.001, 0.1, 0.1, 0.01, 0.01, 
                  0.01, 0.01, 0.001, 0.001, 0.1, 0.1, 0.01, 0.01,
                  # 0.001, 0.001, 0.1, 0.1, 0.01, 0.01,
                  
                  0.01,  0.01, 0.001, 0.001,  0.1, 0.1, 0.01, 0.01,
                  0.01,  0.01, 0.001, 0.001,  0.1, 0.1, 0.01, 0.01
                  # 0.001, 0.001, 0.1, 0.1, 0.01, 0.01
                  )
tanUnitsSet  <- c(0.25, 0.25, 0.2, 0.2, 120, 120, 1.5, 1.5,
                  0.25, 0.25, 0.1, 0.1, 12, 12, 2.0, 2.0,
                  # 1, 1, 10, 10, 0.1, 0.1,
                  
                  0.25, 0.25, 0.2, 0.2, 100, 100, 1.5, 1.5,
                  0.25, 0.25, 0.1, 0.1, 12, 12, 2.0, 2.0
                  # 1, 1, 10, 10, 0.1, 0.1
                  )

# following used in uia_numeric_input_module.R to support wq_map_module.R

uiaUnitsList <- c('mg/L UIA-N',   'mg/kg UIA-N',
                  'mmol/L UIA-N', 'mmol/kg UIA-N',
                  'μg/L UIA-N',   'μg/kg UIA-N',
                  'μmol/L UIA-N', 'μmol/kg UIA-N',
                  
                  'mg/L UIA',   'mg/kg UIA',
                  'mmol/L UIA', 'mmol/kg UIA',
                  'μg/L UIA',   'μg/kg UIA',
                  'μmol/L UIA', 'μmol/kg UIA'
)

uiaUnitsMin  <- c(rep(0, 16))
uiaUnitsMax  <- c(3, 3, 0.20, 0.20,   40,   40,  2.0,  2.0,    
                  3, 3, 0.20, 0.20,   40,   40,  3.0,  3.0 
)
uiaUnitsStep <- c(0.01, 0.01, 0.001, 0.001, 0.1, 0.1, 0.01, 0.01,
                  0.01, 0.01, 0.001, 0.001,  0.1, 0.1, 0.01, 0.01
)
uiaUnitsSet  <- c(0.25, 0.25, 0.1, 0.1, 15, 15, 2.0, 2.0,
                  0.25, 0.25, 0.1, 0.1, 12, 12, 2.0, 2.0
)


# Ca++ ----
# see: http://www.advancedaquarist.com/2002/3/chemistry

# typical SW...
# 10.3 mM, 0.412 g/kg

caUnitsList <- c('mg/kg', 'mg/L', 'mmol/kg', 'mmol/L')

caMin  <- c(rep(0, 4))
caMax  <- c(600, 600, 15, 15)
caStep <- c(rep(0.1, 4))
caSet  <- c(412, 412, 10.3, 10.3)



# ---- CHOICES to populate wq_state_module.R ----

# c('rho-kg/L', 'g/L', 'MT/m\U00B3', 'lb/gal (US)', 'lb/gal (UK)')
wqStateChoices_rho <- rhoUnits
wqStateChoices_ionic_strength <- c('mol/kg-H2O (molal)')
wqStateChoices_uia_posto <- c('%')
wqStateChoices_borate <- c('borate-kg/L', 'g/L', 'g/kg', 'lb/gal (US)')



# ---- BAROMETRIC Pressure ----

barometricUnitsList <- c('atm', 'mm Hg (torr)', 'mbar', 'kPa',
                         'km', 'm', 'ft')

barometricUnitsList_short <- c('atm', 'mm Hg', 'mbar', 'kPa',
                               'km', 'm', 'ft')

barometricChoices <- list(Pressure=barometricUnitsList[1:4],
                          'by Altitude'=barometricUnitsList[5:7])


# Barometric pressure range (see: https://water.usgs.gov/software/DOTABLES/)
# 380-836 mm Hg, 14.97-32.91 in Hg, 507-1114 mbar, 51-112 kPa, or 0.5-1.1 atm


# barometricMin<-c(1, 760, 1015, 102,
#                  0, 0, 0)
# barometricMax<-c(0.8, 600, 800, 80,
#                  1.8, 1800, 6000)
barometricMin<-c(0.5, 380, 510, 50,
                 0, 0, 0)
barometricMax<-c(1.1, 830, 1115, 115,
                 4.0, 4000, 13000)
barometricStep<-c(0.01, 1, 1, 0.1,
                  0.1, 1, 1)
barometricSet<-c(1, 760, 1013.25, 101.325,
                 0, 0, 0)


# ---- GAS Calc Data ----

# MAX Values...
# > calcGasAllUnits('O2', 2080, 'μmol/kg', 273.15 + 40, 0)
# O2 :  31.9988  &  22.3924 
# vals        units
# 1     65.002        mg/kg
# 2     66.040         mg/L
# 3    2080.00      μmol/kg
# 4    2063.82       μmol/L
# 5       2.08      mmol/kg
# 6       2.06       mmol/L
# 7    46.2139         mL/L
# 8     2.0009          atm
# 9  1520.6740 mm Hg (torr)
# 10 2027.3986         mbar
# 11   29.4050          psi
# 12   59.8691        in Hg
# 13  813.9748       in H2O

gasUnitsListPref <- c('atm', 'mm Hg (torr)', 'mg/L', 'mL/L', "μmol/kg", 
                      'mg/kg', 'mL/kg',
                      'mbar', 'psi', 'in Hg', 'in H2O',
                      "μmol/L", "mmol/kg", "mmol/L")

gasUnitsListPref_short <- c('atm', 'mm Hg', 'mg/L', 'mL/L', "μmol/kg", 
                            'mg/kg', 'mL/kg',
                            'mbar', 'psi', 'in Hg', 'in H2O',
                            "μmol/L", "mmol/kg", "mmol/L")


gasChoices <- list(Preferred=gasUnitsListPref[1:5],
                   'Mass & Volume'=gasUnitsListPref[6:7],
                   Pressure=gasUnitsListPref[8:11],
                   Moles=gasUnitsListPref[12:14])


gasMin <- c(     0, 0, 0, 0, 0, 0,  
                 0, 0, 
                 0, 0,
                 0, 0, 0, 0)
gasMax <- c(    0.3, 240.0, 12.0, 8.0, 360.0,
                12.0, 8.0,
                300.0, 4.0,
                10.0, 120.0, 360.0, 0.4, 0.4)
# gasMax <- c(    1.5, 1200, 60, 40, 1800,  # for PURE O2
#               60, 40, 
#               1500, 20, 50, 600,
#               1800, 2, 2)
gasStep <- c(    0.01, 0.1, 0.01, 0.01, 0.1, 
                 0.01, 0.01, 
                 0.1, 0.01, 0.01, 0.1, 
                 0.1, 0.01, 0.01)
gasSet <- c(    0.20, 150, 7.5, 5.25, 230,  
                7.5, 5.25, 
                200, 3, 6, 80,
                230, 0.23, 0.23)


# ** all gas constants at STP
MW_CO2  <- 44.0095             # g/mol
MV_CO2  <- 22.26289            # L/mol
# RHO_CO2 <- 1.976810            # g/L
RHO_CO2 <- 1.97678             # g/L
MFV_CO2 <- 0.000400            # atmospheric mole fraction by volume (unitless)

MW_O2  <- 31.998               # g/mol, mg/mol
MV_O2  <- 22.392               # or 22.3916 L/mol, mL/mmol ("real", not "ideal")
# RHO_O2 <- 1.42900269734374     # g/L
RHO_O2 <- 1.42899              # g/L
MFV_O2 <- 0.20946              # atmospheric mole fraction by volume (unitless)

MW_N2  <- 28.014               # g/mol
MV_N2  <- 22.404               # L/mol ("real", not "ideal") from Hamme & Emerson (2004), p. 1526
# RHO_N2 <- 1.25037493304767     # g/L
RHO_N2 <- 1.25040              # g/L
MFV_N2 <- 0.78084              # atmospheric mole fraction by volume (unitless)

MW_AR  <- 39.948               # g/mol
MV_AR  <- 22.393               # L/mol ("real", not "ideal")
# RHO_AR <- 1.78395034162461     # g/L
RHO_AR <- 1.78395              # g/L
MFV_AR <- 0.00934              # atmospheric mole fraction by volume (unitless)


real_gas_vals <- data.frame(code = c('mw', 'mv', 'rho', 'χ atm'), 
                            CO2  = c(MW_CO2, MV_CO2, RHO_CO2, MFV_CO2), 
                            O2   = c(MW_O2, MV_O2, RHO_O2, MFV_O2),
                            N2   = c(MW_N2, MV_N2, RHO_N2, MFV_N2), 
                            Ar   = c(MW_AR, MV_AR, RHO_AR, MFV_AR), 
                            # CO2 = c(44.0095, 22.26289, 1.976810, 0.000400), 
                            # O2 = c(31.9988, 22.3924, 1.42900269734374, 0.209476),
                            # N2 = c(28.0134, 22.404, 1.25037493304767, 0.78084), 
                            # Ar = c(39.948, 22.393, 1.78395034162461, 0.00934),  
                            units = c('g/mol', 'L/mol', 'g/L', '-'), 
                            
                            stringsAsFactors = F)

# Garcia & Gordon (2004), L&O 37: 1307
sat_coefs <- data.frame(code = c(rep('a', 6), rep('b', 4), 'c'), 
                        O2 = c(5.80871, 3.20291, 4.17887, 5.10006, -0.0986643, 3.80369, -0.00701577, 0.00770028, -0.0113864, -0.00951519, -2.75915), 
                        N2 = c(6.42931, 2.92704, 4.32531, 4.69149,  0.0,       0.0,     -0.00744129, 0.00802566, -0.0146775,  0.0,         0.0), 
                        Ar = c(2.79150, 3.17609, 4.13116, 4.90379,  0.0,       0.0,     -0.00696233, 0.00766670, -0.0116888,  0.0,         0.0), 
                        
                        stringsAsFactors = F)


# ---- TGP Calc Data ----

tgpUnitsList <- c(    '%',
                  'Δ atm', 'Δ mm Hg (torr)', 'Δ mbar',
                    'atm',   'mm Hg (torr)',   'mbar')

tgpUnitsList_short <- c(    '%',
                        'Δ atm', 'Δ mm Hg', 'Δ mbar',
                          'atm',   'mm Hg',   'mbar')


tgpChoices <- list(                  tgpUnitsList[1],
                   'Δ Pressure'    = tgpUnitsList[2:4],
                   'Total Pressure'= tgpUnitsList[5:7])


tgpMin<-c(0, 
          -1, -600, 0,
          0, 0, 0)
tgpMax<-c(150,  
          1, 700, 2,
          1.5, 1200, 60)
tgpStep<-c(0.1, 
           0.1, 0.1, 0.1,
           0.1, 0.1, 0.1)
tgpSet<-c(100, 
          7.5, 5.25, 0.23,
          0.20, 150, 7.5)



# # --- O2 Tank Duration ----

# • D Tank = 0.16   (4" x 16")
# • E Tank = 0.28   (4" x 26")
# • G Tank = 2.41   (8" x 36")
# • H/K Tank = 3.14 (9" x 52")
# • M tank = 1.56

pureO2TankSizes   <- c('D (4" x 16")', 'E (4" x 26")', 'G (8" x 36")', 'H/K (9" x 52")')
pureO2TankFactors <- c(0.16, 0.28, 2.41, 3.14, 1.56)

# for pressure select input in o2_duration_module.R
# 'atm', 'mm Hg (torr)', 'mbar', 'psi', 'in Hg'

pureO2PressureUnitsList <- c('atm', 'mm Hg (torr)', 'mbar', 'psi', 'in Hg')

# see: http://www.onlineconversion.com/pressure.htm
pureO2PressureMin     <- c(rep(0, 5))
# pureO2PressureMax     <- c(142.89650984, 108601.34062, 144789.90288, 2100, 4275.6433809)
pureO2PressureMax     <- c(142, 108600, 144790, 2100, 4275)
pureO2PressureStep    <- c(1, 100, 100,   10, 10)
pureO2PressureSet     <- c(100, 77500, 103000, 1500, 3050)

pureO2FlowRateUnitsList <- c('liter/min', 'liter/hr')
pureO2FlowRateMin     <- c(rep(0, 2))
pureO2FlowRateMax     <- c(20, 120)
pureO2FlowRateStep    <- c(0.1, 1)
pureO2FlowRateSet     <- c(2, 50)

# now, only "liters (L)"
loxVolumeUnitsList <- c('liters (L)', 'cubic feet (ft\U00B3)')

loxVolumeMin     <- c(0, 0)
loxVolumeMax     <- c(200, 10)
loxVolumeStep    <- c(1, 0.1)
loxVolumeSet     <- c(120, 6)

loxFlowRateUnitsList <- c('liter/min', 'liter/hr')
loxFlowRateMin     <- c(rep(0, 2))
loxFlowRateMax     <- c(20, 120)
loxFlowRateStep    <- c(0.1, 1)
loxFlowRateSet     <- c(2, 50)




# # --- CO2 - dissolved ----

# for max CO2 = 100 mg/L ...
# > calcGasAllUnits('CO2', 100, 'mg/L', 40 + 273.15, 0)
# vals        units
# 1   0.0959          atm
# 2  72.8611 mm Hg (torr)
# 3  100.000         mg/L
# 4  50.5866         mL/L
# 5  2290.05      μmol/kg
# 6  100.784        mg/kg
# 7  50.9832        mL/kg
# 8  97.1402         mbar
# 9   1.4089          psi
# 10  2.8685        in Hg
# 11 39.0005       in H2O
# 12 2272.24       μmol/L
# 13  2.2901      mmol/kg
# 14  2.2722       mmol/L

co2DissolvedChoices <- c('mg/L', 'mg/kg', 
                         'mmol/L', 'mmol/kg', 
                         'μmol/L', 'μmol/kg',
                         'mm Hg (torr)', 'atm')

co2DissolvedChoices_short <- c('mg/L', 'mg/kg', 
                               'mmol/L', 'mmol/kg', 
                               'μmol/L', 'μmol/kg',
                               'mm Hg', 'atm')

co2_dissolvedMin  <- c(rep(0, 8))
co2_dissolvedMax  <- c(100, 100, 2.3, 2.3, 2300, 2300, 75, 0.10)
# co2_dissolvedMax  <- c(rep(100, 8))
co2_dissolvedStep <- c(0.1, 0.1, 0.01, 0.01, 1, 1, 0.1, 0.001)
co2_dissolvedSet  <- c(10, 10, 0.23, 0.23, 230, 230, 7.5, 0.01)



# # --- CO2 - atmospheric ----

co2UnitsList <- c('μatm', "μmol/mol", 'ppmv', '%')

co2UnitsList_with_micro <- c('micro-atm', "micro-mole/mole", 'ppmv', '%')

co2_gasChoices <- list(co2UnitsList[1],
                       co2UnitsList[2],
                       co2UnitsList[3],
                       co2UnitsList[4])

# co2_gasChoices <- list('Partial Pressure'=co2UnitsList[1],
#                        'Mole Fraction'=co2UnitsList[2],
#                        'Mixing Ratio'=co2UnitsList[3])


co2_gasMin  <- c(rep(300, 3), 0)
co2_gasMax  <- c(rep(1000, 3), 0.1)
co2_gasStep <- c(rep(1, 3), 0.0001)
co2_gasSet  <- c(rep(404, 3), 0.0404)



# # --- Bunsen Coefficients ----
# using the solubility coefficients of 
# Weiss, R. F. from "The Solubility of N2, O2, and Ar in Water and Seawater", 
# Deep-Sea Research, Vol. 17, pp. 721-735, 1970.

bunsen_coefs <- data.frame(code = c(rep('a', 3), rep('b', 3)), 
                           O2 = c(-58.3877, 85.8079, 23.8439, -0.034892, 0.015568, -0.0019387),
                           N2 = c(-59.6274, 85.7661, 24.3696, -0.051580, 0.026329, -0.0037252),
                           Ar = c(-55.6578, 82.0262, 22.5929, -0.036267, 0.016241, -0.0020114),
                           stringsAsFactors = F)

