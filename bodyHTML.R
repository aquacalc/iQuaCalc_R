# dashboardBody HTML


# ---- home tab ----

homeStuff <- {
  
    tags$img(src = 'iQuaCalc_Home.png')

  # div(
  #   # class = 'home',
  # tags$img(src = 'iQuaCalc_Home.png'),
  # style = "height: 600px; width: 650px; background-color: #3366CC; background-size: cover;"
  # # background-image: url('CIMG0092.jpg');",
  # # tags$img(src = 'CIMG0092.jpg'),
  # # style = "background-color: #3366CC; background-image: url('CIMG0092.jpg');
  # # height: 600px; width: 650px",
  # # h1('iQuaCalc -- The Part-time WQ Assistant')
  # # HTML(paste0(tags$strong(h1('iQuaCalc')), '\n', tags$strong(h2('The Part-time WQ Tech')))),
  # # tags$i(tags$strong(tags$h1('iQuaCalc', align = 'center'))),
  # # tags$h2('The Part-time WQ Assistant', align = 'center')
  # )
}


# ---- WQ Map ----

wqMapStuff <- {
  
  fluidRow(
    
    column(width = 12,
           
           wqMapModuleInput('wq_map')
    )
  )
  
}


# ---- Scratchpad ----

scratchpadStuff <- {
  
  scratchpadModuleInput('my_scratchpad')
}


# ---- TANK Dimensions ----

tankStuff <- {
  
  div(
    
    id = 'tankDimensions',
    
    fluidRow(
      
      column(
        width = 3,
        selectInput('tankUnits', 'From units', c('millimeters (mm)'='mm', 'centimeters (cm)'='cm',
                                                 'meters (m)'='m', 'yd, ft, & in' = 'yards'),
                    selected = 'm'),
        # use uiOutput + renderUI to change 'Length' to 'Meters', 'Feet', etc.
        # uiOutput('metersTankUI'),
        
        numericInput('metersOrYards', 'm', 1.0, 0, 500, 0.01),
        
        conditionalPanel(
          condition = "input.tankUnits == 'yards'",
          numericInput('feetConvertTank', 'feet', 1, 0, 11, 1),
          numericInput('inchConvertTank', 'inches', 0, 0, 11, 1),
          selectInput('fractionalInchesTank', 'fractional inches', c('0', '1/16', '1/8', '3/16', '1/4',
                                                                     '5/16', '3/8', '7/16', '1/2',
                                                                     '9/16', '5/8', '11/16', '3/4',
                                                                     '13/16', '7/8', '15/16'))
        )       # end conditionalPanel
      ),        # end column(), width = 3
      
      column(
        width = 9,
        tabsetPanel(
          id = 'tankPanels',
          # tabPanel( title = 'Samocha RW Tank',
          #   h3('Rectangular, half-hexagonal ends'),
          #   radioButtons('samocha', 'Choose a measurement to enter...', 
          #                c('length', 'width', 'shallow-end depth', 'deep-end depth'), inline = T),
          #   h4(textOutput('whatTimesWhat_samocha')),
          #   # numericInput('slopeSamocha', 'slope to drain (%)', 1, 0, 45, 0.1),
          #   wellPanel(
          #     div(DT::dataTableOutput('tankSamochaConversionTable'), style = 'font-size:135%')
          #   )
          # ),
          tabPanel( title = 'Rectangular Tank',
            h3('Rectangular, square ends'),
            radioButtons('rectangular', 'Choose a measurement to enter...', 
                         c('length', 'width', 'shallow-end depth', 'deep-end depth'), inline = T),
            h4(textOutput('whatTimesWhat_rectangular')),
            # numericInput('slopeRectangular', 'slope to drain (%)', 1, 0, 45, 0.1),
            wellPanel(
              div(DT::dataTableOutput('tankRectangularConversionTable'), style = 'font-size:135%')
            )
          ),
          tabPanel( title = 'Circular Tank',
            # h3('Circular'),
            radioButtons('circular', 'Choose a measurement to enter...', 
                         c('radius', 'diameter', 'depth'), inline = T),
            h4(textOutput('whatTimesWhat_circular')),
            # numericInput('slopeCircular', 'slope to drain (%)', 1, 0, 45, 0.1),
            wellPanel(
              div(DT::dataTableOutput('tankCircularConversionTable'), style = 'font-size:135%')
            )
          )
        )       # end tabsetPanel()
        
      )         # end column(), width = 9 (tabsetPanel)
      # )           # end column(), width = 3
    )             # end fluidRow()
  )               # end div()
}                 # end tankStuff



# ---- FLOW RATE CONVERSION ----

flowRateConversionStuff <- {
  
  fluidRow(
    
    flowRateModuleInput('flow_rate_conversion')
    
  )  # END fluidRow
  
}    # END flowRateConversionStuff


# ---- HYDRAULIC LOAD CONVERSION ----

hydraulicLoadConversionStuff <- {
  
  fluidRow(
    
    hydraulicLoadModuleInput('hydraulic_load_conversion')
    
  )
  
}


# ---- BIOMASS CONVERSION ----

biomassConversionStuff <- {
  
  tabsetPanel(
    
    tabPanel(title = 'Biomass',
             
             h3('Biomass Conversion'),
             
             fluidRow(
               
               column(width = 12, 
                      
                      biomassModuleInput('biomass_convert')
               ),
               
               fluidRow(
                 
                 column(width = 6,
                        # style = 'padding: 0px',
                        
                        h3('per UNIT-AREA', align = 'center'),
                        datatableModuleInput('biomass_convert_1_dt',
                                             col_width = 12,
                                             font_size = '130')
                 ),
                 
                 column(width = 6,
                        # style = 'padding: 0px',
                        
                        h3('per UNIT-VOLUME', align = 'center'),
                        datatableModuleInput('biomass_convert_2_dt',
                                             col_width = 12,
                                             font_size = '130')
                 )
               )    # END fluidRow DT
               
             )
      
    ),
    
    tabPanel(title = 'Abundance',
             
             h3('Abundance Conversion'),
             
             fluidRow(
               
               column(width = 12, 
                      
                      abundanceModuleInput('abundance_convert')
               ),
               
               fluidRow(
                 
                 column(width = 6,
                        # style = 'padding: 0px',
                        
                        h3('per UNIT-AREA', align = 'center'),
                        datatableModuleInput('abundance_convert_1_dt',
                                             col_width = 12,
                                             font_size = '130')
                 ),
                 
                 column(width = 6,
                        # style = 'padding: 0px',
                        
                        h3('per UNIT-VOLUME', align = 'center'),
                        datatableModuleInput('abundance_convert_2_dt',
                                             col_width = 12,
                                             font_size = '130')
                 )
               )    # END fluidRow DT
             )
    )
  )
}



# ---- GAS CONVERSION ----

gasConversionStuff <- {
  
  fluidRow(
           
    gasConversionModuleInput('gas_convert')
  )   # END fluidRow
}  # END gasConversionStuff



# ---- TGP & DISSOLVED GASSES --------

gasSatStuff <- {
  
  fluidRow(
    
    tgpModuleInput('tgp_input_tgp')
    
  )   # END fluidRow
}  # END totalGasPressureStuff

totalGasPressureStuff <- {
  
  fluidRow(
    
    gasTgpModuleInput('tgp_calc')
  )
}


o2TankDurationStuff <- {
  
  o2DurationModuleInput('o2_tank_duration')
}



# ---- SAL CONVERSION ----

# see: http://stackoverflow.com/questions/20637248/shiny-4-small-textinput-boxes-side-by-side
textInput3 <- function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,...))
}

salConversionStuff <- {
  
  bootstrapPage(
    textInput3(inputId="xlimitsmin", label="x-min", value = 0.0, class="input-small"),
    
    textInput3(inputId="xlimitsmax", label="x-max", value = 0.5, class="input-small")
  )
  
}



# ---- T, S, Alk CONVERSION (modularized) ----

t_s_alk_ConversionStuff <- {
  
  fluidRow(
    
    column( width = 4,
            
            # temperatureNumericModuleInput('temp_convert')
            temperatureModuleInput('temp_convert', 1) # '1' => show conversion DT; '0', hide
      
            # fluidRow(
            #   datatableModuleInput('temp_convert_dt', 
            #                        col_width = 11, 
            #                        font_size = '130')
            # )
    ),
    
    column( width = 4,
            
            salinityModuleInput('sal_convert', 1)
            
            # ,
            # 
            # fluidRow(
            #   # column( width = 11,
            #           div(DT::dataTableOutput('sal_convert_dt'),
            #               style = 'font-size:130%')
            # 
            #           # datatable_sal_ModuleInput('sal_convert_dt',
            #           #                      col_width = 11,
            #           #                      font_size = '130')
            #   # )
            # )
    ),
    
    column( width = 4,
            
            alkModuleInput('alk_convert', 1),
            
            fluidRow(
              datatableModuleInput('alk_convert_dt', 
                                   col_width = 11, 
                                   font_size = '130')
            )
    )
  )
}



# ---- LENGTH CONVERSION ----

lengthConversionStuff <- {
  
  fluidRow(
    
    # column(width = 1),
    
    column(width = 6,
           
           lengthModuleInput_uioutput_TWO('length_convert_1', 1) # '1' => show conversion DT; '0', hide
           
           # fluidRow(
           #   datatableModuleInput('length_convert_dt_1', 
           #                        col_width = 12, 
           #                        font_size = '130')
           # )
    ),  # END column
    
    # column(width = 1),
    
    column(width = 6,
           
           # >***> [TEST] Length - uioutput ----
           lengthModuleInput_uioutput_TWO('length_convert_2', 1) # '1' => show conversion DT; '0', hide
    
           # fluidRow(
           #   datatableModuleInput('length_convert_dt_2',
           #                        col_width = 12,
           #                        font_size = '130')
           # )
    )   # END column
           
  )     # END fluidRow
  
}           # end lengthConversionStuff_2




# ---- AREA CONVERSION ----


areaConversionStuff <- {
  
  tabsetPanel(id = 'my_tabset_area', type = 'pills', selected = NULL,
              
              tabPanel('Area -> Area', value = 'a_to_a',
                       
                       fluidRow(
                         
                         column(width = 6,
                                
                                areaModuleInput('area_convert_1'),
                                
                                fluidRow(
                                  datatableModuleInput('area_convert_dt_1', 
                                                       col_width = 12, 
                                                       font_size = '130')
                                )
                         ),
                         
                         column(width = 6,
                                
                                areaModuleInput('area_convert_2'),
                                
                                fluidRow(
                                  datatableModuleInput('area_convert_dt_2', 
                                                       col_width = 12, 
                                                       font_size = '130')
                                )
                         )
                       )   # END fluidRow
              ),  # END tabPanel('Area -> Area')
              
              tabPanel('L x W -> Area', value = 'l_by_w_to_area',
                       
                       fluidRow(
                         
                         column(width = 6,
                                
                                wellPanel(style = 'padding: 3px;',
                                          
                                          tags$h5('LENGTH', align = 'center'),
                                          lengthModuleInput_uioutput_TWO('l_by_w_length', 0) # '1' => show conversion DT; '0', hide
                                          
                                  # lengthModuleInput('l_by_w_length', 'LENGTH')
                                )
                                
                         ),  # END column
                         
                         column(width = 6,
                                
                                wellPanel(style = 'padding: 3px;',
                                          
                                          tags$h5('WIDTH', align = 'center'),
                                          lengthModuleInput_uioutput_TWO('l_by_w_width', 0) # '1' => show conversion DT; '0', hide
                                          
                                  # lengthModuleInput('l_by_w_width', 'WIDTH')
                                )
                                
                         )   # END column
                       ),     # END fluidRow
                       
                       fluidRow(
                         
                         column(width = 12, offset = 3,
                                
                                datatableModuleInput('l_by_w_to_area_dt',
                                                     col_width = 6,
                                                     font_size = '130')
                         )
                       ) # END fluidRow DT
                       
              ),  # END tabPanel('L x W -> Area')
              
              tabPanel(HTML(paste0('πr',  tags$sup('2'),'-> Area')), value = 'pi_r2_to_area',
                  
                       fluidRow(
                         
                         column(width = 6,
                                
                                wellPanel(style = 'padding: 3px;',
                                  lengthModuleInput('radius_or_diameter', 'RADIUS')
                                )
                                
                         ),  # END column
                         
                         column(width = 6,
                                
                                HTML((paste0('πr',  tags$sup('2'),'-> Area'))),
                                
                                h2('RADIUS or DIAMETER?')

                         #        wellPanel(
                         #          lengthModuleInput('l_by_w_width')
                         #        )

                         )   # END column
                       ),     # END fluidRow
                       
                       fluidRow(
                         
                         column(width = 12, offset = 3,
                         
                                datatableModuleInput('pi_r2_to_area_dt',
                                                     col_width = 6,
                                                     font_size = '130')
                         )
                       )  # END fluidRow DT
                       
              ) # END tabPanel('πr^2 -> Area')
              
  )   # END tabsetPanel
  
}           # END areaConversionStuff




# ---- VOLUME CONVERSION ----

volConversionStuff <- {
  
  tabsetPanel(id = 'my_tabset_volume', type = 'pills', selected = NULL,
              
              tabPanel('Volume -> Volume', value = 'v_to_v',
                       
                       fluidRow(
                         
                         column(width = 6,
                                
                                wellPanel(style = 'padding: 3px;',
                                          
                                          volumeModuleInput('volume_convert_1')
                                ),
                                
                                fluidRow(
                                  datatableModuleInput('volume_convert_dt_1', 
                                                       col_width = 12, 
                                                       font_size = '130')
                                )
                         ),
                         
                         column(width = 6,
                                
                                wellPanel(style = 'padding: 3px;',
                                          
                                          volumeModuleInput('volume_convert_2')
                                ),
                                
                                fluidRow(
                                  datatableModuleInput('volume_convert_dt_2', 
                                                       col_width = 12, 
                                                       font_size = '130')
                                )
                         )
                       )   # END fluidRow
              ),    # END tabPanel('Volume -> Volume')
              
              tabPanel('L x W x D -> Volume', value = 'l_by_w_by_d_to_volume',
                       
                       fluidRow(
                         
                         column(width = 5, style = 'margin-left: 5px;',
                                
                                fluidRow(
                                  
                                  tags$h4('LENGTH'),
                                  
                                  # '1' => show conversion DT; '0', hide
                                  lengthModuleInput_uioutput_TWO('l_by_w_by_d_length', 0),
                                  
                                  tags$hr()
                                ),
                                
                                fluidRow(
                                  
                                  tags$h4('WIDTH'),
                                  
                                  lengthModuleInput_uioutput_TWO('l_by_w_by_d_width', 0),
                                  
                                  tags$hr()
                                ),
                                
                                fluidRow(
                                  
                                  tags$h4('DEPTH'),
                                  
                                  lengthModuleInput_uioutput_TWO('l_by_w_by_d_depth', 0)
                                )
                         ),
                         
                         column(width = 6,
                                
                                br(), br(), br(),
                                
                                datatableModuleInput('l_by_w_by_d_to_volume_dt',
                                                     col_width = 12,
                                                     font_size = '130')
                         )  # END column
                       )     # END fluidRow
                       
              ),    # END tabPanel('L x W x D -> Volume')
              
              tabPanel(HTML(paste0('D x πr', tags$sup(2), ' -> Volume')), value = 'd_by_pi_r2_to_volume',
                       
                       fluidRow(
                         
                         column(width = 6,
                                
                                tags$h4('DEPTH'),
                                
                                lengthModuleInput_uioutput_TWO('d_by_pi_r2_to_volume_depth', 0)
                                
                         ),  # END column
                         
                         column(width = 6,
                                
                                tags$h4('RADIUS'),
                                
                                lengthModuleInput_uioutput_TWO('d_by_pi_r2_to_volume_radius', 0)
                                
                         )   # END column
                         
                       ),     # END fluidRow
                       
                       fluidRow(
                         
                         column(width = 12, offset = 3,
                         
                                datatableModuleInput('d_by_pi_r2_to_volume_dt',
                                                     col_width = 6,
                                                     font_size = '130')
                         )
                       )    # END fluidRow DT
                       
              ),    # END tabPanel('D x πr^2 -> Volume')
              
              tabPanel('D x Area -> Volume', value = 'd_by_area_to_volume',
                       
                       fluidRow(
                         
                         column(width = 6,
                                
                                areaModuleInput('d_by_area_to_volume_area')
                                
                         ),  # END column
                         
                         column(width = 6,
                                
                                tags$h4('DEPTH'),
                                
                                lengthModuleInput_uioutput_TWO('d_by_area_to_volume_depth', 0)
                                
                                # lengthModuleInput('d_by_area_to_volume_depth', 'DEPTH')
                                
                         )   # END column
                         
                       ),     # END fluidRow
                       
                       fluidRow(
                         
                         column(width = 12, offset = 3,
                                
                                datatableModuleInput('d_by_area_to_volume_dt',
                                                     col_width = 6,
                                                     font_size = '130')
                         )
                       )    # END fluidRow DT
                       
              )    # END tabPanel('D x Area -> Volume')
    
  )    # END tabsetPanel(id = 'my_tabset_volume')
  
} # END volConversionStuff



# ---- UN-IONIZED AMMONIA UI ----

ammoniaStuff <- {
  
  fluidPage(

    uiaModuleInput('uia')
  )
  
} # END ammoniaStuff


ammoniaConversionStuff <- {
  
  fluidPage(
    
    tanConversionModuleInput('ammonia_convert')
  )
}


# ---- DISINFECTION poSto to ppm UI ----

poStoToPpmStuff <- {
  
  fluidPage( 
    
    disinfectionModuleInput('disinfect')
    # ,
    
    # fluidRow(
    #   
    #   column(width = 6,
    #          # style = 'padding: 0px',
    #          
    #          h3('Ammonia', align = 'center'),
    #          datatableModuleInput('ammonia_convert_1_dt',
    #                               col_width = 12,
    #                               font_size = '130')
    #   ),
    #   
    #   column(width = 6,
    #          # style = 'padding: 0px',
    #          
    #          h3('Ammonia-Nitrogen', align = 'center'),
    #          datatableModuleInput('ammonia_convert_2_dt',
    #                               col_width = 12,
    #                               font_size = '130')
    #   )
    # )    # END fluidRow DT
  
  )    # ---- END fluidPage
}    # ---- END poStoToPpmStuff



# ---- DATA UPLOAD ----

lengthWeightStuff <- {
  
  lwUploadModuleInput('lw_analysis')
  
}
