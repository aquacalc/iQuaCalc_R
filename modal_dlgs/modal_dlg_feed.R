# modal dialog UI for wq_map_module.R
# feed ----

# Return the UI for a modal dialog with data selection input. If 'failed' is
# TRUE, then display a message that the previous value was invalid.
modal_feed_ui <- function(failed = FALSE) {
  
  modalDialog(title = 'Feed Dialogue',
              
              fluidRow(
                
                column(width = 4,
                       
                       tags$h4('Enter Biomass', align = 'center'),
                       
                       tabsetPanel(
                         
                         tabPanel(title = 'Direct', style = 'background-color: lightblue;',
                                  
                                  splitLayout(cellWidths = c('35%', '65%'),
                                              numericInput('biomass_direct', 'Enter Biomass', value = 0, min = 0, step = 0.1),
                                              selectInput('biomass_direct_units', 'Biomass Units', choices = c('kg', 'lbs'))
                                  )
                                  
                         ),
                         
                         tabPanel(title = 'by Numbers',
                                  
                                  splitLayout(cellWidths = c('35%', '65%'),
                                              numericInput('biomass_numbers_individuals', 'Enter Numbers', value = 0, min = 0, step = 0.1)
                                  ),
                                  br(),
                                  splitLayout(cellWidths = c('35%', '65%'),
                                              numericInput('biomass_numbers_wbar', 'Enter Numbers', value = 0, min = 0, step = 0.1),
                                              selectInput('biomass_numbers_wbar_units', 'Mean Weight', choices = c('g/ind', 'lbs/ind'))
                                  )
                         )
                       )
                  
                ),
                
                column(width = 8,
                       
                       textInput("dataset", "Choose data set",
                                 placeholder = 'Try "mtcars" or "abc"'
                       ),
                       
                       selectInput("datasetSelect", "Choose data set", 
                                   choices = c('Moe!', 'Larry!!', 'Curley!!!')
                       ),
                       
                       span('(Try the name of a valid data object like "mtcars", ',
                            'then a name of a non-existent object like "abc")')
                )
                
              ),
              
              if (failed)
                
                div(tags$b("Invalid name of data object", style = "color: red;")),
              
              footer = tagList(
                
                modalButton("Cancel"),
                
                actionButton("ok", 'Plot on WQ Map')
              )
  )
}


# From NetBeans …Z_NBS code
# 
# /*
#   * MixingDialog.java
# *
#   * Created on Feb 16, 2010, 7:20:54 PM
# */
#   
#   CarbCalc carbCalc;
# TANCalc  tanCalc;
# 
# double number, wBar, biomass;
# double dailyFeedRate, feedPerDay;
# double oldNumber, oldBiomass, oldFeedPerDay; // cache last entry, restore when required
# 
# double o2FactorFish, o2UptakeDailyFish, o2FactorBacters, o2UptakeDailyBacters, totO2UptakeDaily;
# double co2Factor, totCO2ProdDaily, totCO2ProdDailyPerLiter;
# double co2MolesPerLiterPerHr;
# double newPhTot;
# 
# // ** NBB: NEED TO TRANSMIT -- if not display -- PRESSURE
# double pressure;
# 
# String sOriginalInputVol; // ** immutable volumes (s* & d*) read from WQchartDlg2,
# Double dOriginalInputVol; // ** reduced if user chooses "Exchange" radio btn
# 
# 
# // ** the following to distinguish those DISPLAY-ed vs. those for CALCulations
# // ** OLD vs. NEW vs. FINAL refers to original, make-up, & final water, respectively
# // ** DISPLAY vs. CALC refers to units to display & those for calculations, respectively
# double           oldTempDisplay,   oldAlkDisplay,   oldTANDisplay;
# double           oldTempCalc,      oldAlkCalc,      oldTANCalc;
# 
# double           newTempDisplay,   newAlkDisplay,   newTANDisplay;
# double           newTempCalc,      newAlkCalc,      newTANCalc;
# 
# double           finalTempDisplay, finalAlkDisplay, finalTANDisplay;
# double           finalTempCalc,    finalAlkCalc,    finalTANCalc;
# 
# // ** volume (mass!!?) proportionality constants to calc linear combinations
# double frac1, frac2;
# 
# double oldDIC, newDIC, finalDIC; // need to calc final pH
# 
# // ** need to track IC units and those delivered from WQchartDlg2 tMarker data
# String volICunits  = "m3",    volDisplayUnits;
# String tempICunits = "C",     tempDisplayUnits;
# 
# private String omega = "\u03A9";
# private String point = "\u270F";
# private String arrow = "\u21E8";
# 
# private ArrayList<String> startingWqState;
# private ArrayList<String> startingWqUnits;
# 
# 
# // ** without persistence (yet), initialize the calc
# rbBiomass.setSelected(true);
# biomass = 120; // ** kg
# oldBiomass = biomass;
# txtBiomass.setText(String.format("%.2f", biomass));
# 
# this.setLocation(500, 30);
# this.pack();
# this.setVisible(true);
# }
# 
# public void initTheCalc() {
#   
#   setBiomassControls();
#   
#   calcFromBiomass();
# }
# 
# // ** when change a text field's entry, may need to convert, so call the next method
# // ** to get that field's index, needed to call conversionAntics() from within the
# // ** method that validates a field's input
# // ** later, maybe pack this into an Enum??
# private int getTxtFieldIndex(String name) {
# 
# int index = 0; // NB: **CAREFUL**, '0' => "Volume"...
# 
# if(name.equals("txtBiomass"))
# index = 0;
# else if(name.equals("txtWBar"))
# index = 1;
# else
# ;
# 
# return(index);
# }
# 
# private void checkChartModifierFields(String textField) {
# 
# if(textField.equals("txtBiomass"))
# biomassChange();
# else if(textField.equals("txtFeedAmount"))
# rationChange();
# else
# ;
# 
# btnApply.setBackground(Color.GREEN);
# }
# 
# private void biomassChange() {
# oldBiomass = biomass;
# 
# calcFromBiomass();
# }
# 
# private void rationChange() {
# oldFeedPerDay = feedPerDay;
# 
# calcFromRation();
# }
# 
# // ** ******************* RESP DLG WORK *****************************
# 
# public void sendWqInputDisplayInfo(ArrayList<String> displayData,
# ArrayList<String> displayUnits) {
# 
# // ** assign WQchartDlg2's T-marker DISPLAY DATA to MixingDlg's input WQ data
# alDisplayData = displayData;
# sOriginalInputVol = alDisplayData.get(0);
# 
# // ** initialize MixingDialogs's own NEW DISPLAY DATA (here, same as INPUT, until
#                                                        // ** re-.set below to init values in controls
#                                                        // ** -- ??MUST passing new-water display DATA in *this* way...??
#                                                        // ** -- (HARD-WIRE for the Beta demo? combo box choice in production release)
#                                                        
#                                                        // ** initialize MixingDialog's own FINAL DISPLAY DATA (here, as INPUT display data
#                                                        for(int k=0; k<alDisplayData.size(); k++)
#                                                        alFinalDisplayData.set(k, alDisplayData.get(k));
#                                                        
#                                                        // ** assign WQchartDlg2's T-marker UNITS to MixingDlg's input & make-up WQ units
#                                                        alDisplayUnits = displayUnits;
# }
# 
# public void sendWqInputIcData(ArrayList<Double> icData,
# ArrayList<String> icUnits) {
# 
# // ** assign 'original' IC data from WQchartDlg2
# alIcData  = icData;
# dOriginalInputVol = alIcData.get(0);
# 
# alIcUnits = icUnits;
# 
# for(int k=0; k<alIcData.size(); k++) {
# 
# alFinalIcData.set(k, 0.00000);
# }
# 
# // ** set the INPUT DISPLAY INFO...
# setInputDisplayData();
# 
# // ** with all of the data assembled, run the calc...
# runTheCalc();
# }
# 
# private void setInputDisplayData() {
# 
# txtVol.setText(alDisplayData.get(0));
# lblVol.setText(alDisplayUnits.get(0));
# 
# txtTemp.setText(alDisplayData.get(1));
# lblTemp.setText(alDisplayUnits.get(1));
# 
# txtSal.setText(alDisplayData.get(2));
# 
# txtPh.setText(alDisplayData.get(12));
# 
# txtAlk.setText(alDisplayData.get(13));
# lblAlk.setText(alDisplayUnits.get(13));
# 
# txtTAN.setText(alDisplayData.get(4));
# lblTAN.setText(alDisplayUnits.get(4));
# 
# txtNH3.setText(alDisplayData.get(5));
# lblNH3.setText(alDisplayUnits.get(5));
# 
# // ** DIC for ORIGINAL water...
# alIcData.set(18, carbCalc.getDICofAlkTpH(alIcData.get(12), alIcData.get(13),
# alIcData.get(1),  alIcData.get(2)));
# 
# // ** NB: Need to CALC [CO2], NOT read in CO2-crit, eh?
# // ** fCO2, ORIGINAL (closed system)
# alIcData.set(6, alIcData.get(18) *
# carbCalc.getAlpha0(alIcData.get(12), alIcData.get(1), alIcData.get(2)) *
# //                           1000000.0 / (carbCalc.getKHWeiss(alIcData.get(1),  alIcData.get(2))));
# 44009.6 * carbCalc.getRho(alIcData.get(1), alIcData.get(2)) / 1000.0); // mg/l
# txtCO2.setText(String.format("%.1f", alIcData.get(6)));
# lblCO2.setText(alDisplayUnits.get(6));
# 
# txtOmegaCa.setText(String.format("%.2f", alIcData.get(21)));
# txtOmegaAr.setText(String.format("%.2f", alIcData.get(22)));
# }
# 
# 
# 
# 
# private void RUN__THE__CALC() {
# 
# // ** get or calculate the CO2 (hence, DIC) addition in moles/kg
# number = (Double)spinNumber.getValue();
# wBar   = (Double)spinWBar.getValue();
# 
# // ** for this first run, calc biomass au lieu de le prendre directement
# //        biomass = number * wBar / 1000.0; // ** NB: here, hard-wired conversion to KG
# 
# // ** for this run, [kg] = [ind/m3] * [g/ind] * [m3]/ 1000
# biomass = number * wBar * alIcData.get(0) / 1000.0; // ** NB: here, hard-wired conversion to KG
# 
# if(!rbBiomass.isSelected())
# txtBiomass.setText(String.format("%.2f", biomass));
# else
# biomass = Double.parseDouble(txtBiomass.getText());
# 
# //        dailyFeedRate = 0.017;
# dailyFeedRate = (Double)spinFeedRate.getValue() / 100.0;
# 
# if(!rbFeedRation.isSelected()) {
# 
# feedPerDay = dailyFeedRate * biomass;
# txtFeedAmount.setText(String.format("%.2f", feedPerDay));
# }
# else
# feedPerDay = Double.parseDouble(txtFeedAmount.getText());
# 
# o2FactorFish = 0.25;
# o2UptakeDailyFish = o2FactorFish * feedPerDay;
# o2FactorBacters = 0.125;
# o2UptakeDailyBacters = o2FactorBacters * feedPerDay;
# totO2UptakeDaily = o2UptakeDailyFish + o2UptakeDailyBacters;
# 
# co2Factor = 1.375;
# totCO2ProdDaily = co2Factor * totO2UptakeDaily;
# totCO2ProdDailyPerLiter = totCO2ProdDaily / alIcData.get(0);
# 
# txtO2Uptake.setText(String.format("%.2f", totO2UptakeDaily));
# txtCO2Release.setText(String.format("%.2f", totCO2ProdDaily));
# 
# double mwCO2 = 44.0098; // ** g/mole
# 
# double hrsPostFeeding = (Double)spinHours.getValue();
# 
# double co2MolesPerLiterPerDay = totCO2ProdDailyPerLiter / mwCO2;
# co2MolesPerLiterPerHr  = hrsPostFeeding * co2MolesPerLiterPerDay / 24.0;
# /*
# cat(" Number    [ind]: ", number);
# cat("   wBar  [g/ind]: ", wBar);
# cat("biomass     [kg]: ", biomass);
# 
# cat("*****   Feed   *****");
# cat("feed     [%/day]: ", dailyFeedRate);
# cat("feed    [kg/day]: ", feedPerDay);
# 
# cat("*****   Fish   *****");
# cat("O2        factor: ", o2FactorFish);
# cat("O2      [kg/day]: ", o2UptakeDailyFish);
# 
# cat("***** Bacteria *****");
# cat("O2        factor: ", o2FactorBacters);
# cat("O2      [kg/day]: ", o2UptakeDailyBacters);
# 
# cat("***** Total O2 *****");
# cat("O2-TOT  [kg/day]: ", totO2UptakeDaily);
# 
# cat("*** Closed System ***");
# cat("*****    CO2   *****");
# cat("      factor: ", co2Factor);
# cat("    [kg/day]: ", totCO2ProdDaily);
# cat("   [g/l/day]: ", totCO2ProdDailyPerLiter);
# cat("[mole/l/day]: ", co2MolesPerLiterPerDay);
# cat(" [mole/l/hr]: ", co2MolesPerLiterPerHr);
# // ** ...so, after 1 hr of respiration, DIC will increase by (co2MolesPerLiterPerDay/24.0)
# cat("After 1 hr, DIC = ", alIcData.get(18), " + ", co2MolesPerLiterPerHr, " =...");
# */
# 
# // ** use stoichiometry to calculate the change in alkalinity
# double newAlkPerHr = (15.0 / 106.0) * co2MolesPerLiterPerHr;
# //        cat("After 1 hr, Alk = ", alIcData.get(13), " + ", newAlkPerHr, " =...");
# 
# // ** use CarbCalc's getPhForAlkDIC() to calculate new pH
# newPhTot = carbCalc.getPhForAlkDIC((alIcData.get(13)+newAlkPerHr),
#                                    (alIcData.get(18)+co2MolesPerLiterPerHr),
#                                    alIcData.get(1),
#                                    alIcData.get(2));
# 
# alFinalIcData.set(12, newPhTot);
# alFinalIcData.set(13, (alIcData.get(13)+newAlkPerHr));
# alFinalIcData.set(17, (alIcData.get(18)+co2MolesPerLiterPerHr));
# 
# cat("     IC ---->", alIcData.get(1), " .. ", alIcData.get(2), " .. ", alIcData.get(18));
# cat("DISPLAY ---->", alFinalDisplayData.get(1), " .. ", alFinalDisplayData.get(2), " .. ", alFinalDisplayData.get(18));
# 
# double newPhNBS = carbCalc.getPhNBSFromTot(newPhTot,
#                                            alIcData.get(1),
#                                            alIcData.get(2),
#                                            alIcData.get(3));
# 
# // ** Omega-Ca, FINAL
# alFinalIcData.set(19, carbCalc.getOmegaSWCa((alIcData.get(18)+co2MolesPerLiterPerHr),
#                                             alIcData.get(1),
#                                             alIcData.get(2), newPhTot));
# alFinalDisplayData.set(19, String.format("%.2f", alFinalIcData.get(19)));
# 
# // ** Omega-Ar, FINAL
# alFinalIcData.set(20, carbCalc.getOmegaSWAr((alIcData.get(18)+co2MolesPerLiterPerHr),
#                                             alIcData.get(1),
#                                             alIcData.get(2), newPhTot));
# alFinalDisplayData.set(20, String.format("%.2f", alFinalIcData.get(20)));
# 
# alFinalDisplayData.set(12, String.format("%.2f", newPhNBS));
# alFinalDisplayData.set(13, String.format("%.2f", 1000.0 * (alIcData.get(13)+newAlkPerHr)));
# 
# /*
#   cat("After 1 hr, [Alk] from ", alIcData.get(13), "  to ", 
#                        (alIcData.get(13)+newAlkPerHr), " =...");
# 
# cat("After 1 hr,  DIC  from ", alIcData.get(18), "  to ", 
#                      (alIcData.get(18)+co2MolesPerLiterPerHr), " =...");
# 
# cat("After 1 hr, pHtot from ", alIcData.get(12), "  to ", 
#                      newPhTot, " =...");
# 
# cat("After 1 hr, pHNBS from ", alDisplayData.get(12), " to ", 
#                      newPhNBS, " =...");
# */
#   
#   // ** mg TAN/liter/hr added by remineralization...
# double addedTAN = co2MolesPerLiterPerHr * (16.0 / 106.0) * 17.0304 * 1000.0;
# 
# cat("moles CO2/l/hr: ", co2MolesPerLiterPerHr);
# cat("After 1 hr, TAN from ", alDisplayData.get(4), " + ", 
#                      addedTAN, " =...");
# 
# // ** post results to this dlg (and notify chart observer)
# setFinalDisplayData();
# 
# fillFinalDilutionData();
# }
# 
# private void calcFromNumber() {
#   
#   // ** get or calculate the CO2 (hence, DIC) addition in moles/kg
#   number = (Double)spinNumber.getValue();
#   wBar   = (Double)spinWBar.getValue();
#   // ** for this run, [kg] = [ind/m3] * [g/ind] * [m3]/ 1000
#   biomass = number * wBar * alIcData.get(0) / 1000.0; // ** NB: here, hard-wired conversion to KG
#   
#   txtBiomass.setText(String.format("%.2f", biomass));
#   
#   dailyFeedRate = (Double)spinFeedRate.getValue() / 100.0;
#   
#   feedPerDay = dailyFeedRate * biomass;
#   txtFeedAmount.setText(String.format("%.2f", feedPerDay));
#   
#   finishTheCalc();
# }
# 
# private void calcFromBiomass() {
#   
#   // ** hard-wired here in [kg]
#   biomass = Double.parseDouble(txtBiomass.getText());
#   
#   dailyFeedRate = (Double)spinFeedRate.getValue() / 100.0;
#   
#   feedPerDay = dailyFeedRate * biomass;
#   txtFeedAmount.setText(String.format("%.2f", feedPerDay));
#   
#   finishTheCalc();
# }
# 
# private void calcFromRation() {
#   
#   feedPerDay = Double.parseDouble(txtFeedAmount.getText());
#   
#   finishTheCalc();
# }
# 
# 
# 
# # // ** whether start from NUMBER, BIOMASS, or RATION, each runs this method...
# # finish_the_calc <- function() {
# #   
# #   o2_factor_fish <- 0.25
# #   o2_uptake_daily_fish <- o2_factor_fish * input$feed_per_day
# 
# #   o2_factor_bacters <- 0.125
# #   o2_uptake_daily_bacters <- o2_factor_bacters * input$feed_per_day
# 
# #   o2_uptake_daily_TOT <- o2_uptake_daily_fish + o2_uptake_daily_bacters
#    
# #   co2_factor <- 1.375
# #   co2_prod_daily_TOT <- co2_factor * o2_uptake_daily_TOT
# #   totCO2ProdDailyPerLiter = totCO2ProdDaily / alIcData.get(0);
# #   co2_prod_daily_per_liter_TOT <- co2_prod_daily_TOT / ???
# 
# #   mwCO2 <- 44.0098 // ** g/mole
# 
# #   input$hrs_post_feeding
# 
# #   co2_moles_per_liter_per_day <- co2_prod_daily_per_liter_TOT / mwCO2
# #   co2_moles_per_liter_per_hr  <- input$hrs_post_feeding * co2_moles_per_liter_per_day / 24.0
# #   
# #   //        double tanMolesPerLiterPerDay = totTANProdDailyPerLiter / mwTAN;
# #   //        tanMolesPerLiterPerHr  = hrsPostFeeding * tanMolesPerLiterPerDay / 24.0;
# #   
# #   cat(" Number    [ind]: ", number);
# #   cat("   wBar  [g/ind]: ", wBar);
# #   cat("biomass     [kg]: ", biomass);
# #   
# #   cat("*****   Feed   *****");
# #   cat("feed     [%/day]: ", dailyFeedRate);
# #   cat("feed    [kg/day]: ", feedPerDay);
# #   
# #   cat("*****   Fish   *****");
# #   cat("O2        factor: ", o2FactorFish);
# #   cat("O2      [kg/day]: ", o2UptakeDailyFish);
# #   
# #   cat("***** Bacteria *****");
# #   cat("O2        factor: ", o2FactorBacters);
# #   cat("O2      [kg/day]: ", o2UptakeDailyBacters);
# #   
# #   cat("***** Total O2 *****");
# #   cat("O2-TOT  [kg/day]: ", totO2UptakeDaily);
# #   
# #   cat("*** Closed System ***");
# #   cat("*****    CO2   *****");
# #   cat("      factor: ", co2Factor);
# #   cat("    [kg/day]: ", totCO2ProdDaily);
# #   cat("   [g/l/day]: ", totCO2ProdDailyPerLiter);
# #   cat("[mole/l/day]: ", co2MolesPerLiterPerDay);
# #   cat(" [mole/l/hr]: ", co2MolesPerLiterPerHr);
# #   // ** ...so, after 1 hr of respiration, DIC will increase by (co2MolesPerLiterPerDay/24.0)
# #   cat("After 1 hr, DIC = ", alIcData.get(18), " + ", co2MolesPerLiterPerHr, " =...");
#   
# #   // ** use stoichiometry to calculate the change in alkalinity
# #   new_alk_per_hr <- (15.0 / 106.0) * co2_moles_per_liter_per_hr
# #   cat("After 1 hr, Alk = ", alIcData.get(13), " + ", new_alk_per_hr, " =...");
# 
# #   // ** use stoichiometry to calculate the change in TAN
# #   kg_tan_per_hr <- 0.38 * 0.092 * input$feed_per_day / 24 // ** kg TAN / day
# 
# #   cat("After 1 hr, TAN = ", alIcData.get(4), " + ", kgTANPerHr, " =...");
# #   
# #   // ** use CarbCalc's getPhForAlkDIC() to calculate new pH
# #   new_ph_tot <- getPhForAlkDIC((alIcData.get(13)+newAlkPerHr),
# #   (alIcData.get(18)+co2MolesPerLiterPerHr),
# #   alIcData.get(1),
# #   alIcData.get(2))
# #   
# #   alFinalIcData.set(12, newPhTot);
# #   alFinalIcData.set(13, (alIcData.get(13)+newAlkPerHr));
# #   alFinalIcData.set(17, (alIcData.get(18)+co2MolesPerLiterPerHr));
# #   
# #   cat("     IC ---->", alIcData.get(1), " .. ", alIcData.get(2), " .. ", alIcData.get(18));
# #   cat("DISPLAY ---->", alFinalDisplayData.get(1), " .. ", alFinalDisplayData.get(2), " .. ", alFinalDisplayData.get(18));
# #   
# #   double newPhNBS = carbCalc.getPhNBSFromTot(newPhTot,
# #   alIcData.get(1),
# #   alIcData.get(2),
# #   alIcData.get(3));
# 
# #   // ** Omega-Ca, FINAL
# #   alFinalIcData.set(19, carbCalc.getOmegaSWCa((alIcData.get(18)+co2MolesPerLiterPerHr),
# #   alIcData.get(1),
# #   alIcData.get(2), newPhTot));
# #   alFinalDisplayData.set(19, String.format("%.2f", alFinalIcData.get(19)))
# 
# #   // ** Omega-Ar, FINAL
# #   alFinalIcData.set(20, carbCalc.getOmegaSWAr((alIcData.get(18)+co2MolesPerLiterPerHr),
# #   alIcData.get(1),
# #   alIcData.get(2), newPhTot));
# #   alFinalDisplayData.set(20, String.format("%.2f", alFinalIcData.get(20)));
#    
# #   alFinalDisplayData.set(12, String.format("%.2f", newPhNBS));
# #   alFinalDisplayData.set(13, String.format("%.2f", 1000.0 * (alIcData.get(13)+newAlkPerHr)));
#    
# #   // ** mg TAN/liter/hr added by remineralization...
# #   added_TAN <- co2_moles_per_liter_per_hr * (16.0 / 106.0) * 17.0304 * 1000.0
#    
# #   cat("moles CO2/l/hr: ", co2_moles_per_liter_per_hr);
# #   cat("After 1 hr, TAN from ", alDisplayData.get(4), " + ", 
# #   added_TAN, " =...");
#    
# #   // ** post results to this dlg (and notify chart observer)
# 
# # }
# 
# # private void setFinalDisplayData() {
# # 
# # // ** TO SOLVE SIG FIG PROBLEM...maybe send as <Double>, then String.format() below...?
# # ArrayList<Double> alFinalDisplayDbl = new ArrayList<Double>();
# # alFinalDisplayDbl = wqStateConverter.getDisplayDataAsDouble(alFinalIcData, alDisplayUnits);
# # /*
# # txtNH3Final.setText(String.format("%.2f", alNewIcData.get(15)));
# # txtCO2Final.setText(String.format("%.1f", alNewIcData.get(6)));
# # txtOmegaCaFinal.setText(String.format("%.2f", alNewIcData.get(19)));
# # txtOmegaArFinal.setText(String.format("%.2f", alNewIcData.get(20)));
# # */
# # txtAlkFinal.setText(String.format("%.2f", alFinalDisplayDbl.get(13)));
# # //        txtAlkFinal.setText(alFinalDisplayData.get(13));
# # //        txtAlkFinal.setText(alFinalDisplayData.get(11));
# # //        txtTANFinal.setText(String.format("%.2f", alFinalDisplayDbl.get(4)));
# # txtTANFinal.setText(alFinalDisplayData.get(4));
# # 
# # txtPhFinal.setText(alFinalDisplayData.get(12));
# # //        txtPhNBSFinal.setText(String.format("%.2f", alFinalDisplayDbl.get(12)));
# # txtNH3Final.setText(String.format("%.2f", alFinalIcData.get(15)));
# # 
# # // ** 17 to 18 ******************************
# # // ** 17 to 18 ******************************
# # // ** 17 to 18 ******************************
# # // ** fCO2, ORIGINAL (closed system)
# # alFinalIcData.set(6, (alIcData.get(18)+co2MolesPerLiterPerHr) *
# # carbCalc.getAlpha0(newPhTot, alIcData.get(1), alIcData.get(2)) *
# # //                           1000000.0 / (carbCalc.getKHWeiss(alIcData.get(1),  alIcData.get(2))));
# # 44009.6 * carbCalc.getRho(alIcData.get(1), alIcData.get(2)) / 1000.0); // mg/l
# # 
# # txtCO2Final.setText(String.format("%.1f", alFinalIcData.get(6)));
# # txtOmegaCaFinal.setText(alFinalDisplayData.get(19));
# # txtOmegaArFinal.setText(alFinalDisplayData.get(20));
# # }
# # 
# # // ** ******************* RESP DLG WORK *****************************
# # 
# # // ** fill the ArrayList with final dilution data for PhAlkChart & WQchartDlg2
# # private void fillFinalDilutionData() {
# # 
# # //        0:vol 1:temp 2:sal 3:pH-NBS 4: pH-Tot 5:Alk
# # //        6:DIC 7:TAN  8:NH3 9:CO2   10:oCa    11:oAr
# # 
# # finalWqState.set(0, alFinalDisplayData.get(0));
# # finalWqState.set(1, alFinalDisplayData.get(1));
# # finalWqState.set(2, alFinalDisplayData.get(2));
# # // ** NB: pH on NBS scale
# # finalWqState.set(3, alFinalDisplayData.get(12));
# # // ** NB: pH on Total scale
# # finalWqState.set(4, String.format("%.8f", alFinalIcData.get(12)));
# # // ** NB: [Alk] in eq/l
# # finalWqState.set(5, String.format("%.8f", alFinalIcData.get(13)));
# # // ** 17 to 18 ******************************
# # // ** 17 to 18 ******************************
# # // ** 17 to 18 ******************************
# # // ** NB: added finalDIC to plot WQ dilution marker
# # finalWqState.set(6, String.format("%.8f", alFinalIcData.get(18)));
# # 
# # finalWqState.set(7, txtTANFinal.getText());
# # finalWqState.set(8, txtNH3Final.getText());
# # finalWqState.set(9, txtCO2Final.getText());
# # finalWqState.set(10, txtOmegaCaFinal.getText());
# # finalWqState.set(11, txtOmegaArFinal.getText());
# # }
# # 
# # }
