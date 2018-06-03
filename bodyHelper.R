# for iQuaCalc (Lite)

# bodyHelper.R

source('bodyHTML.R')

# body <- dashboardBody(
body <- (
  
  tabItems(
    
    tabItem(tabName = 'home',
            homeStuff
    ),
    
    tabItem(tabName = 'un_ionized_ammonia',
            ammoniaStuff
    ),
    
    tabItem(tabName = "gas_sat",
            # h3('Dissolved Gas Calculations'),
            gasSatStuff
    ),
    
    tabItem(tabName = "gas_tgp",
            # h3('Dissolved Gas Calculations'),
            totalGasPressureStuff
    ),
    
    tabItem(tabName = "o2_tank_duration",
            # h3('Dissolved Gas Calculations'),
            o2TankDurationStuff
    ),
    
    # tabItem(tabName = "gasses",
    #         # h3('Dissolved Gas Calculations'),
    #         dissolvedGasStuff
    # ),
    
    tabItem(tabName = 'poStoToPpm',
            poStoToPpmStuff
    ),
    
    tabItem(tabName = "tanks",
            h2('Calculate Tank Area & Volume'),
            # tankStuff,
            h2('404: Not yet implemented in this demo')
    ),
    
    tabItem(tabName = 'scratch_pad',
            h2('Experimental: Conversion Scratchpad'),
            scratchpadStuff
    ),
    
    tabItem(tabName = 't_s_alk',
            t_s_alk_ConversionStuff
    ),
    
    tabItem(tabName = 'ammonia_conversion',
            ammoniaConversionStuff
    ),
    
    tabItem(tabName = 'gas_conversion',
            gasConversionStuff
    ),
    
    tabItem(tabName = 'length',
            h3('Length Converters'),
            lengthConversionStuff
    ),
    
    tabItem(tabName = 'area',
            h3('Area Converters'),
            areaConversionStuff
    ),

    tabItem(tabName = 'vol',
            h3('Volume Converters'),
            volConversionStuff
    ),
    
    tabItem(tabName = 'flowVolume',
            # h3('Flow Volume Conversion'),
            flowRateConversionStuff
    ),
    
    tabItem(tabName = 'hydraulic_load',
            hydraulicLoadConversionStuff
    ),
    
    tabItem(tabName = 'biomass',
            biomassConversionStuff
    ),
    
    tabItem(tabName = 'length_weight',
            # h3('Analyze Length-Weight Data'),
            lengthWeightStuff
    ),
    
    tabItem(tabName = 'wq_map',
            # h3('Analyze Length-Weight Data'),
            wqMapStuff
    )
    
  )    # ---end tabItems
)    # ---- end body