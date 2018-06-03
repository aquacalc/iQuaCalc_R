# [Alk] module functions for
# "iQuaCalc (Lite) compute duration of O2 cylinder module.R"

# see: http://respcalc.com/oxygen-tank-duration-calculation/
# and: https://www.tgh.org/sites/default/files/oxygen-duration-nugget-.pdf
# Duration of Flow = Oxygen Tank Conversion Factor * Remaining Tank Pressure (psi) / Continuous Flow Rate (L/min)
# O2 Cylinder Conversion Factors: max volume (L) by max pressure (psi)
# for LOX, 860 L gas / 1 L liquid

# WITH RESIDUAL PRESSURE: see http://www-users.med.cornell.edu/~spon/picu/calc/o2tankd.htm

# The calculations are based on Boyle's law that states that 
# pressure and the volume of a given amount of a confined gas are inversely proportional (P * V = k).

# Duration  =  	k * (P - R) / F

# Pressure  =  	(T * F / k) + R

# k is the tank constant in PSI-1 liters-1.
# R is the Safe Residual Pressure in PSI, typically 200 PSI.
# F is the Flow in liters per minute.
# T is the duration in minutes.
# P is the tank gauge pressure in PSI.



# • D Tank = 0.16   (4" x 16")
# • E Tank = 0.28   (4" x 26")
# • G Tank = 2.41   (8" x 36")
# • H/K Tank = 3.14 (9" x 52")
# • M tank = 1.56


# Calculate duration of LOX with 5.4 L at 15L/min
# 1.  Convert to volume of gas first:
#    5.4L of liquid x 860 L of gas/L of liquid = 4644 L of gas
# 2. Then take into account the liter flow:
#      4644L of gas/ 15L/min = 309.6 minutes or 5.16 hours

# Calculate duration of an E cylinder with 1500 psi at 2 L/min
# 625 L (gas) / 2216 psi = 0.2820397 L / psi
#      1500 x .28 = 420 / 2 = 210 minutes or 3.5 hours

# DIFFERENT CALC: https://www.manuelsweb.com/O2remaining.htm
# capacity (in L) / service pressure (in psi) = remaining contents (in L) / gauge pressure (in psi)
# The service capacity for an e-cylinder carrying oxygen is 1900 psi.
# The volume of oxygen in an e-cylinder is 660 liters.
  
  # 660 L / 1900 psi = remaining contents (in L) / gauge pressure (in psi)

  # {0.35 x psi on gauge}  / {L/min to be delivered}
  
  # {0.35 x 500 psi} / {6 L/min} = 28 min


# LOX calc ----
# see: http://sciencing.com/calculate-liquid-oxygen-gaseous-oxygen-5822250.html

# As an example, calculate the volume of the gaseous oxygen at 20 Celsius and 
# the pressure of one atmosphere (atm) that is obtained from evaporation of 
# 70 liters (L) of liquid oxygen.
# 
# Multiply the volume (in Liters) of the liquid oxygen by 1,000 to convert it 
# to milliliters (ml). In our example 70 L will be converted to 70,000 ml.
# 
# Multiply the volume of the liquid oxygen by its density, 1.14 g/ml, to calculate 
# the mass of the compound. 
# In our example, the mass of oxygen is 70,000 ml x 1.14 g/ml or 79,800 g.
# 
# Divide the mass of oxygen by its molecular mass to calculate the number of moles. 
# In our example, the oxygen amount is 79,800 g / 32 g/mole = 2,493.75 moles.
# 
# Convert temperature in Celsius to Kelvin (K) by adding the value "273.15." 
# In this example, temperature is 20 + 273.15 = 293.15 K.
# 
# Multiply the pressure in atm by the factor "101,325" to convert the pressure to 
# the SI unit Pascal (Pa). In our example, Pressure = 101,325 x 1 atm = 101,325 Pa.
# 
# Round the molar gas constant R to the fourth digit to obtain 8.3145 J/mole x K. 
# Note that the constant is given in the International System of Units (SI). 
# "J" means Joule, a unit of energy.
# 
# Calculate the volume (in cubic meters) of gaseous oxygen using the ideal gas law: 
# multiply the amount of oxygen (in moles) by temperature and the molar gas constant 
# followed by dividing the product by pressure. 
# In our example, Volume = 2493.75 (mole) x 8.3145 (J/mole x K) x 293.15(K) / 101,325 (Pa) = 
# 59.99 cubic meters or 59,990 L.

# Oxygen Tank Sizes
#  "welding" grade (~85% pure), not "medical" grade (~99% pure)
# Another thing to keep in mind is the size of the unit. There are two different ways this is measured. 
# The old way was to give them letters ranging from A to E, where A was the smallest unit and E the largest. 
# The new way is to label them with M (for medical) with a number. The number signifies the cubic feet of 
# oxygen that the unit will hold. Most suppliers will show you the conversion of the old technique to the 
# new to ensure you get the size that is right for you.

# see: https://thefishsite.com/articles/transportation-of-warmwater-fish
# A standard oxygen cylinder holds about 280 cubic feet of oxygen 
# and weighs about 150 pounds. 
# A LOX dewar (tank) holds about 4,500 ft^3 (127,425.8 L) O2 (gas? => ~148 L liquid) 
# and weighs about 780 pounds.
# see: https://www.was.org/documents/MeetingPresentations/AQ2010/AQ2010_0050.pdf
# Dewar may weigh 354 kg and hold 127 m3 or 127,000 L


o2DurationModuleInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
   # fluidRow(
     
     tabsetPanel(
       
       # Pure O2 UI ----
       
       tabPanel('Pure Oxygen Tank',
         
         fluidRow(
           
           # tags$h3(tags$strong('Pure Oxygen Tank', align = 'left', 
           #                     style = 'padding: 0px 0px 0px 8px; margin: 0px 0px 0px 8px; color: green;')),
           
           column(width = 4,
                  
                  wellPanel(
                    
                    radioButtons(ns('rb_o2_tank_options'),
                                 'O2 Tank Sizes',
                                 
                                 choices = pureO2TankSizes,
                                 
                                 selected = 'H/K (9" x 52")',
                                 inline = F)
                  )
           ),
           
           column(width = 8,
                  
                  fluidRow(
                    
                    column(width = 8,
                           
                           sliderInput(ns('o2_tank_pressure_convert'), 'Gauge Pressure',
                                       min = 0, max = 20, value = 2, step = 1)
                    ),
                    
                    column(width = 4,
                           
                           br(),
                           
                           selectInput(ns('o2_tank_pressure_units'), 'Pressure Units', 
                                       choices = pureO2PressureUnitsList,
                                       selected = 'psi')
                    )
                  ),
                  
                  fluidRow(
                    
                    column(width = 8,
                           
                           sliderInput(ns('o2_tank_flow_convert'), 'Gas Flow Rate', 
                                       min = 0, max = 45, value = 33)
                    ),
                    
                    column(width = 4,
                           
                           br(),
                           
                           selectInput(ns('o2_tank_flow_units'), 'Flow Rate Units', 
                                       
                                       choices=pureO2FlowRateUnitsList,
                                       
                                       selected = 'liter/min')
                    )
                  )  # END fluidRow "O2 flow input"
           )  # END column #2/2 for "Pure O2 tank"
           
         ),  # END fluidRow "Pure Oxygen Tank"
         
         hr(),
         
         fluidRow(
           
           column(width = 12,
                  
                  box(
                    width = NULL,
                    title = 'Pure O2 Tank Duration',
                    solidHeader = T,
                    status = 'primary',
                    background = 'light-blue',
                    
                    htmlOutput(ns('test_o2_tank'))
                    
                    # tags$style(HTML('.box-body {padding: 30px; margin: 30px;}'))
                    
                  )    # ---- end box
           )   # END Pure O2 result column
           
         )   # END fluidRow Pure O2 result 
         
       ), # END tabPanel 1/2 (Pure O2)
       
       # LOX UI ----
       tabPanel('LOX Tank',
         
         fluidRow(
           
           # tags$h3(tags$strong('Liquid Oxygen (LOX) Tank', align = 'left',
           #                     # style = 'padding: 0px 5px 0px; margin: 0px 5px 0px; color: green;')),
           #                     style = 'padding: 0px 0px 0px 8px; margin: 0px 0px 0px 8px; color: green;')),
           
           column(width = 4,
                  
                  wellPanel(
                    
                    tags$h4('Ignore this section for now', align = 'center'),
                    
                    radioButtons(ns('rb_lox_tank_options'),
                                 'LOX Tank Sizes',
                                 
                                 choices = pureO2TankSizes,
                                 
                                 selected = 'H/K (9" x 52")',
                                 inline = F)
                  )
           ),
           
           column(width = 8,
                  
                  fluidRow(
                    
                    column(width = 8,
                           
                           sliderInput(ns('lox_tank_volume_convert'), 'LOX Volume',
                                       min = 0, max = 20, value = 2, step = 0.1)
                    ),
                    
                    column(width = 4,
                           
                           br(),
                           
                           selectInput(ns('lox_tank_volume_units'), 'LOX Volume Units',
                                       choices = loxVolumeUnitsList,
                                       selected = 'liters (L)')
                    )
                  ),
                  
                  fluidRow(
                    
                    column(width = 8,
                           
                           sliderInput(ns('lox_tank_flow_convert'), 'LOX Flow Rate',
                                       min = 0, max = 45, value = 33)
                    ),
                    
                    column(width = 4,
                           
                           br(),
                           
                           selectInput(ns('lox_tank_flow_units'), 'LOX Flow Rate Units',
                                       
                                       choices=loxFlowRateUnitsList,
                                       
                                       selected = 'liter/min')
                    )
                  )  # END fluidRow "LOX flow input"
           )  # END column #2/2 for LOX
           
         ),  # END fluidRow LOX
         
         hr(),
         
         fluidRow(
           
           column(width = 12,
                  
                  box(
                    title = 'LOX Tank Duration',
                    width = NULL,
                    solidHeader = T,
                    status = 'primary',
                    background = 'light-blue',
                    
                    htmlOutput(ns('test_lox_tank'))
                    
                    # see: https://stackoverflow.com/questions/43689281/rshiny-reduce-bottom-padding-shiny-dashboard-box
                    # tags$style(HTML('.form-group, .selectize-control {margin: 0px;}
                    #                 .box-body {padding: 0px; margin: 0px;}'))
                    
                    # tags$style(HTML('.box-body {padding: 5px 5px 25px 5px; margin: 5px 5px 25px 5px;}'))
                    
                  )    # ---- end box
           )   # END LOX result column
           
         )  # END fluidRow "LOX"
         
       ) # END tabPanel 2/2 (LOX)
     ) # END tabsetPanel
    
  )  # END tagList
}



o2DurationModule <- function(input, output, session, st) {
  
  # "*_init" flags when app is (re-)launched
  rv <- reactiveValues(o2_tank_pressure_units_select_init = -1,
                       o2_tank_pressure_units_default = 'psi',
                       o2_tank_pressure_sl_init = -1,
                       o2_tank_pressure_default = pureO2PressureSet,
                       
                       o2_tank_flow_units_select_init = -1,
                       o2_tank_flow_units_default = 'liter/min',
                       o2_tank_flow_sl_init = -1,
                       o2_tank_flow_default = c(2, 18),
                       
                       lox_tank_volume_units_select_init = -1,
                       lox_tank_volume_units_default = 'liters (L)',
                       lox_tank_volume_sl_init = -1,
                       lox_tank_volume_default = loxVolumeSet,
                       
                       lox_tank_flow_units_select_init = -1,
                       lox_tank_flow_units_default = 'liter/min',
                       lox_tank_flow_sl_init = -1,
                       lox_tank_flow_default = c(15, 18)
                       )
  
  
  # ---- PURE O2 PRESSURE updates ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$o2_tank_pressure_units, priority = 50, {
    
    if(rv$o2_tank_pressure_units_select_init < 0)  {
      
      x <- session$ns('o2_tank_pressure_units')
      
      rv$o2_tank_pressure_units_select_init <- 1
      
      o2_tank_pressure_units_init <- st()[[x]]
      
      if(length(o2_tank_pressure_units_init) == 0)
        o2_tank_pressure_units_init <- rv$o2_tank_pressure_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'o2_tank_pressure_units', 'Pressure Units',
                        # choices=alkUnitsList,
                        choices  = pureO2PressureUnitsList,
                        selected = o2_tank_pressure_units_init)
    }
    
    updateStore(session, session$ns("o2_tank_pressure_units"), input$o2_tank_pressure_units)
    
    idx <- which(input$o2_tank_pressure_units == pureO2PressureUnitsList)
    
    y <- paste0(session$ns('sl_'), input$o2_tank_pressure_units)
    
    my_o2_tank_pressure_value <- st()[[y]]
    
    if(length(my_o2_tank_pressure_value) == 0)
      my_o2_tank_pressure_value <- rv$o2_tank_pressure_default[idx]
    
    updateSliderInput(session, "o2_tank_pressure_convert", 
                      label = paste0('Gauge Pressure (', pureO2PressureUnitsList[idx], ')'),
                      value = my_o2_tank_pressure_value,
                      min   = pureO2PressureMin[idx], 
                      max   = pureO2PressureMax[idx], 
                      step  = pureO2PressureStep[idx])
    
    freezeReactiveValue(input, "o2_tank_pressure_convert")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$o2_tank_pressure_units), my_o2_tank_pressure_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$o2_tank_pressure_convert, {
    
    if(rv$o2_tank_pressure_sl_init < 0) {
      
      rv$o2_tank_pressure_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$o2_tank_pressure_units == pureO2PressureUnitsList)
    
    y <- paste0(session$ns('sl_'), input$o2_tank_pressure_units)
    
    my_o2_tank_pressure_value <- st()[[y]]
    
    
    if(length(my_o2_tank_pressure_value) == 0)
      my_o2_tank_pressure_value <- rv$o2_tank_pressure_default[idx]
    else
      my_o2_tank_pressure_value <- input$o2_tank_pressure_convert
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$o2_tank_pressure_units), my_o2_tank_pressure_value)
    
  })
  
  
  # ---- PURE O2 FLOW RATE updates ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$o2_tank_flow_units, priority = 50, {
    
    if(rv$o2_tank_flow_units_select_init < 0)  {
      
      x <- session$ns('o2_tank_flow_units')
      
      rv$o2_tank_flow_units_select_init <- 1
      
      o2_tank_flow_units_init <- st()[[x]]
      
      if(length(o2_tank_flow_units_init) == 0)
        o2_tank_flow_units_init <- rv$o2_tank_flow_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'o2_tank_flow_units', 'Flow Rate Units',
                        # choices=alkUnitsList,
                        
                        # choices=list('per MINUTE' = pureO2FlowRateUnitsList[13:18], 
                        #              'per HOUR' = pureO2FlowRateUnitsList[7:12], 
                        #              'per DAY' = pureO2FlowRateUnitsList[1:6]),
                        
                        choices=pureO2FlowRateUnitsList,
                        
                        selected = o2_tank_flow_units_init)
    }
    
    updateStore(session, session$ns("o2_tank_flow_units"), input$o2_tank_flow_units)
    
    idx <- which(input$o2_tank_flow_units == pureO2FlowRateUnitsList)
    
    y <- paste0(session$ns('sl_'), input$o2_tank_flow_units)
    
    my_o2_tank_flow_value <- st()[[y]]
    
    if(length(my_o2_tank_flow_value) == 0)
      my_o2_tank_flow_value <- rv$o2_tank_flow_default[idx]
    
    updateSliderInput(session, "o2_tank_flow_convert", 
                      label = paste0('Gas Flow Rate (', pureO2FlowRateUnitsList[idx], ')'),
                      value = my_o2_tank_flow_value,
                      min   = pureO2FlowRateMin[idx], 
                      max   = pureO2FlowRateMax[idx], 
                      step  = pureO2FlowRateStep[idx]
                      )
    
    freezeReactiveValue(input, "o2_tank_flow_convert")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$o2_tank_flow_units), my_o2_tank_flow_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$o2_tank_flow_convert, {
    
    if(rv$o2_tank_flow_sl_init < 0) {
      
      rv$o2_tank_flow_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$o2_tank_flow_units == pureO2FlowRateUnitsList)
    
    y <- paste0(session$ns('sl_'), input$o2_tank_flow_units)
    
    my_o2_tank_flow_value <- st()[[y]]
    
    
    if(length(my_o2_tank_flow_value) == 0)
      my_o2_tank_flow_value <- rv$o2_tank_flow_default[idx]
    else
      my_o2_tank_flow_value <- input$o2_tank_flow_convert
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$o2_tank_flow_units), my_o2_tank_flow_value)
    
  })
  
  
  
  
  # ---- LOX VOLUME updates ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$lox_tank_volume_units, priority = 50, {
    
    if(rv$lox_tank_volume_units_select_init < 0)  {
      
      x <- session$ns('lox_tank_volume_units')
      
      rv$lox_tank_volume_units_select_init <- 1
      
      lox_tank_volume_units_init <- st()[[x]]
      
      if(length(lox_tank_volume_units_init) == 0)
        lox_tank_volume_units_init <- rv$lox_tank_volume_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'lox_tank_volume_units', 'LOX Volume Units',
                        # choices=alkUnitsList,
                        choices = loxVolumeUnitsList,
                        selected = lox_tank_volume_units_init)
    }
    
    updateStore(session, session$ns("lox_tank_volume_units"), input$lox_tank_volume_units)
    
    idx <- which(input$lox_tank_volume_units == loxVolumeUnitsList)
    
    y <- paste0(session$ns('sl_'), input$lox_tank_volume_units)
    
    my_lox_tank_volume_value <- st()[[y]]
    
    if(length(my_lox_tank_volume_value) == 0)
      my_lox_tank_volume_value <- rv$lox_tank_volume_default[idx]
    
    updateSliderInput(session, "lox_tank_volume_convert", 
                      label = paste0('LOX Volume (', loxVolumeUnitsList[idx], ')'),
                      value = my_lox_tank_volume_value,
                      min   = loxVolumeMin[idx], 
                      max   = loxVolumeMax[idx], 
                      step  = loxVolumeStep[idx])
    
    freezeReactiveValue(input, "lox_tank_volume_convert")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$lox_tank_volume_units), my_lox_tank_volume_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$lox_tank_volume_convert, {
    
    if(rv$lox_tank_volume_sl_init < 0) {
      
      rv$lox_tank_volume_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$lox_tank_volume_units == loxVolumeUnitsList)
    
    y <- paste0(session$ns('sl_'), input$lox_tank_volume_units)
    
    my_lox_tank_volume_value <- st()[[y]]
    
    
    if(length(my_lox_tank_volume_value) == 0)
      my_lox_tank_volume_value <- rv$lox_tank_volume_default[idx]
    else
      my_lox_tank_volume_value <- input$lox_tank_volume_convert
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$lox_tank_volume_units), my_lox_tank_volume_value)
    
  })
  
  
  # ---- LOX FLOW RATE updates ----
  
  # Observe SELECT_INPUT input, store when changed
  observeEvent(input$lox_tank_flow_units, priority = 50, {
    
    if(rv$lox_tank_flow_units_select_init < 0)  {
      
      x <- session$ns('lox_tank_flow_units')
      
      rv$lox_tank_flow_units_select_init <- 1
      
      lox_tank_flow_units_init <- st()[[x]]
      
      if(length(lox_tank_flow_units_init) == 0)
        lox_tank_flow_units_init <- rv$lox_tank_flow_units_default
      
      # user (re-)opened app. Is store$select empty?
      updateSelectInput(session, 'lox_tank_flow_units', 'LOX Flow Rate Units',
                        
                        choices=loxFlowRateUnitsList,
                        
                        selected = lox_tank_flow_units_init)
    }
    
    updateStore(session, session$ns("lox_tank_flow_units"), input$lox_tank_flow_units)
    
    idx <- which(input$lox_tank_flow_units == pureO2FlowRateUnitsList)
    
    y <- paste0(session$ns('sl_'), input$lox_tank_flow_units)
    
    my_lox_tank_flow_value <- st()[[y]]
    
    if(length(my_lox_tank_flow_value) == 0)
      my_lox_tank_flow_value <- rv$lox_tank_flow_default[idx]
    
    updateSliderInput(session, "lox_tank_flow_convert", 
                      label = paste0('LOX Flow Rate (', loxFlowRateUnitsList[idx], ')'),
                      value = my_lox_tank_flow_value,
                      min   = loxFlowRateMin[idx], 
                      max   = loxFlowRateMax[idx], 
                      step  = loxFlowRateStep[idx]
    )
    
    freezeReactiveValue(input, "lox_tank_flow_convert")
    
    # update slider value for current units ???
    updateStore(session, paste0(session$ns('sl_'), input$lox_tank_flow_units), my_lox_tank_flow_value)
    
  })
  
  
  # Observe SLIDER_INPUT input, store when changed
  observeEvent(input$lox_tank_flow_convert, {
    
    if(rv$lox_tank_flow_sl_init < 0) {
      
      rv$lox_tank_flow_sl_init <- 1
      
      return()
    }
    
    idx <- which(input$lox_tank_flow_units == loxFlowRateUnitsList)
    
    y <- paste0(session$ns('sl_'), input$lox_tank_flow_units)
    
    my_lox_tank_flow_value <- st()[[y]]
    
    
    if(length(my_lox_tank_flow_value) == 0)
      my_lox_tank_flow_value <- rv$lox_tank_flow_default[idx]
    else
      my_lox_tank_flow_value <- input$lox_tank_flow_convert
    
    
    # update slider value for current units
    updateStore(session, paste0(session$ns('sl_'), input$lox_tank_flow_units), my_lox_tank_flow_value)
    
  })
  
  
  # Duration Results Outputs ----
  
  output$test_o2_tank <- renderUI({
    
    HTML(paste0(tags$h3(df_o2_tank_duration(), style = "text-align: center;"))) 
  })
  
  
  output$test_lox_tank <- renderUI({
    
    HTML(paste0(tags$h3(df_lox_tank_duration(), style = "text-align: center;"))) 
    
  })
  
  
  # ---- Pure O2 Tank Duration ----
  
  df_o2_tank_duration <- reactive({
    
    # req(
    #   icTemp(),
    #   icSal(),
    #   cancelOutput = T
    # )
    
    # Duration of Flow = Oxygen Tank Conversion Factor * Remaining Tank Pressure (psi) / Continuous Flow Rate (L/min)
    
    
    pure_o2_tank_factor_idx <- which(input$rb_o2_tank_options == pureO2TankSizes)
    
    # ---- For Pure O2 Tank ----
    
    # 1. get O2 tank conversion factor (L / psi)
    pure_o2_tank_factor_idx <- which(input$rb_o2_tank_options == pureO2TankSizes)
    my_pure_o2_factor <- pureO2TankFactors[pure_o2_tank_factor_idx]
    
    
    # 2. convert pressure units to psi -- from ... bar, mbar, torr, psi ...
    ## ** FOR ONE ATMOSPHERE HERE, thus final argument in convertGasFromTo()
    my_o2_pressure_psi_vector <- convertGasFromTo('O2', 
                                                  input$o2_tank_pressure_convert, 
                                                  input$o2_tank_pressure_units, 
                                                  'psi', 
                                                  273.15 + 25, 0,
                                                  1.0) ## ** FOR 1.0 atm HERE, thus final argument
    
    my_o2_pressure_psi <- my_o2_pressure_psi_vector$vals
    
    # 3. convert flow rate to L/minute
    my_o2_flow_lpm <- convert(input$o2_tank_flow_convert, 
                              input$o2_tank_flow_units, 
                              'liter/min', 
                              flow_rate.data)
    
    # 4. multiply conversion factor by pressure in psi -- [L / psi] * [psi] = L
    # 5. divide by flow rate in L/min -- [L / psi] * [psi] / [L / min] = minutes remaining
    my_pure_o2_result_min <- my_pure_o2_factor * my_o2_pressure_psi / my_o2_flow_lpm
    
    # 6. report as minutes and hours
    my_pure_o2_result_hr <- my_pure_o2_result_min / 60.0
    
    
    if(ceiling(my_pure_o2_result_min / 60.0) > 0)
      str_hr_min <- paste0(floor(my_pure_o2_result_min / 60.0), ' hr ', 
                           round(my_pure_o2_result_min %% 60.0, 0), ' min')
    else
      str_hr_min <- my_pure_o2_result_hr
    
    my_result <- paste0('The ', input$rb_o2_tank_options, ' tank at ', 
                        input$o2_tank_pressure_convert, ' ', input$o2_tank_pressure_units, 
                        ' & a flow rate of ', input$o2_tank_flow_convert, ' ', input$o2_tank_flow_units,
                        ' lasts about ', str_hr_min)
  })
  
  
  
  # ---- LOX Tank Duration ----
  
  df_lox_tank_duration <- reactive({
    
    # req(
    #   icTemp(),
    #   icSal(),
    #   cancelOutput = T
    # )
    
    # ---- For LOX Tank ----
    
    # 1. convert tank volume to liters
    lox_volume_liquid_liters <- convert(input$lox_tank_volume_convert, 
                                        input$lox_tank_volume_units, 
                                        'liters (L)',
                                        volume.data)
    
    # 2. multiply tank liquid volume (liters) by 860 (liter gas / liter liquid) to get gas volume in L
    # 2493.75 (mole) x 8.3145 (J/mole x K) x 293.15(K) / 101,325 (Pa)
    # 35.62634 * 8.3145 * 293.15 / 101325
    #  0.8569996
    
    # Multiply the volume (in Liters) of the liquid oxygen by 1,000 to convert it 
    # to milliliters (ml). In our example 70 L will be converted to 70,000 ml.
    # 
    # Multiply the volume of the liquid oxygen by its density, 1.14 g/ml, to calculate 
    # the mass of the compound. 
    # In our example, the mass of oxygen is 70,000 ml x 1.14 g/ml or 79,800 g.
    # 
    # Divide the mass of oxygen by its molecular mass to calculate the number of moles. 
    # In our example, the oxygen amount is 79,800 g / 32 g/mole = 2,493.75 moles.
    # 
    # Convert temperature in Celsius to Kelvin (K) by adding the value "273.15." 
    # In this example, temperature is 20 + 273.15 = 293.15 K.
    # 
    # Multiply the pressure in atm by the factor "101,325" to convert the pressure to 
    # the SI unit Pascal (Pa). In our example, Pressure = 101,325 x 1 atm = 101,325 Pa.
    # 
    # Round the molar gas constant R to the fourth digit to obtain 8.3145 J/mole x K. 
    # Note that the constant is given in the International System of Units (SI). 
    # "J" means Joule, a unit of energy.
    # 
    # Calculate the volume (in cubic meters) of gaseous oxygen using the ideal gas law: 
    # multiply the amount of oxygen (in moles) by temperature and the molar gas constant 
    # followed by dividing the product by pressure. 
    # In our example, Volume = 2493.75 (mole) x 8.3145 (J/mole x K) x 293.15(K) / 101,325 (Pa) = 
    # 59.99 cubic meters or 59,990 L.
    
    lox_volume_gas_liters <- 860 * lox_volume_liquid_liters
    
    # 3. convert flow rate to liters/minute
    my_lox_flow_lpm <- convert(input$lox_tank_flow_convert, 
                               input$lox_tank_flow_units, 
                               'liter/min', 
                               flow_rate.data)
    
    # 4. divide result of #2 by flow rate in liters/minute to get duration in minutes
    lox_result_min <- lox_volume_gas_liters / my_lox_flow_lpm
    
    # 5. report as minutes and hours
    lox_result_hr <- lox_result_min / 60.0
    
    if(ceiling(lox_result_min / 60.0) > 0)
      str_hr_min <- paste0(floor(lox_result_min / 60.0), ' hr ', 
                           round(lox_result_min %% 60.0, 0), ' min')
    else
      str_hr_min <- lox_result_hr
    
    my_result <- paste0('A LOX tank with ', 
                        input$lox_tank_volume_convert, ' ', input$lox_tank_volume_units, 
                        ' & a flow rate of ', 
                        input$lox_tank_flow_convert, ' ', 
                        input$lox_tank_flow_units,
                        ' lasts about ', str_hr_min)
  })
  
}