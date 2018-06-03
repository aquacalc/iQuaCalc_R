# for iQuaCalc (Lite)

# sidebar helper


sbm <- sidebarMenu(

  menuItem('Home', tabName = 'home', icon = shiny::icon('dashboard')),

  menuItem('General Conversions', tabName = 'conversions', icon = shiny::icon('bar-chart-o'),
           menuSubItem('Scratchpad',              tabName = 'scratch_pad'),
           menuSubItem('T, S, [Alk]',             tabName = 't_s_alk'),
           menuSubItem('Ammonia',                 tabName = 'ammonia_conversion'),
           menuSubItem('Biomass & Abundance',     tabName = 'biomass'),
           menuSubItem('Flow Rate & Turnover',    tabName = 'flowVolume'),
           menuSubItem('Hydraulic Load Rate',     tabName = 'hydraulic_load'),
           menuSubItem('Length',                  tabName = 'length'),
           menuSubItem('Area',                    tabName = 'area'),
           menuSubItem('Volume',                  tabName = 'vol')
           ),

  menuItem('Tanks', tabName = 'tanks', icon = shiny::icon('dashboard')),

  menuItem('Un-ionized Ammonia', tabName = 'un_ionized_ammonia', icon = shiny::icon('dashboard')),
  
  # menuItem("TGP", tabName = "tgp_tab", icon = shiny::icon("th")),

  menuItem("Gas Calculations", tabName = "gasses", icon = shiny::icon("th"),
           menuSubItem('Gas Solubility',     tabName = 'gas_sat'),
           menuSubItem('Total Gas Pressure', tabName = 'gas_tgp'),
           menuSubItem('Gas Conversions',    tabName = 'gas_conversion'),
           menuSubItem('O2 Tank Duration',   tabName = 'o2_tank_duration')
           ),

  menuItem("Disinfection", tabName = "poStoToPpm", icon = shiny::icon("th")),
  
  menuItem('Analysis', tabName = 'analysis', icon = shiny::icon('bar-chart-o'),
           menuSubItem('Length-Weight', tabName = 'length_weight'),
           menuSubItem('Yadda',         tabName = 'yadda')
  ),
  
  menuItem('WQ Map', tabName = 'wq_map', icon = shiny::icon('dashboard'))

)
  