# validation script for...
# ...iQuaCalc's WQ Map

# * getInitAndFinalDic() modified to take I.C. pH
# * [DEPRECATED] alkToIcUnits() in iQuaCalc 


# load CarbCalc 'brains'
source('CarbCalc.R')


# ---- get DIC ----

# temp, sal, & [Alk]s passed in as I.C. units
getInitAndFinalDic <- function(temp, sal,
                               initPh, initAlk,
                               finalPh, finalAlk) {
  
  # initPhFREE<-phNbsToPhFree(initPh,sal,temp,0)
  initDic <- 1000.0 * calcDicOfAlk(initAlk / 1000.0, initPh, temp, sal)
  
  # finalPhFREE<-phNbsToPhFree(finalPh,sal,temp,0) 
  finalDic <- 1000.0 * calcDicOfAlk(finalAlk / 1000.0, finalPh, temp, sal)
  
  # cat('\n\n')
  # cat('in getInitAndFinalDic with  INIT pH...', initPh, ' to ', finalPh, ' (FREE)\n')
  # cat('in getInitAndFinalDic with      Alk...', initAlk,' to ',finalAlk, '\n')
  # cat('in getInitAndFinalDic with      DIC...', initDic,' to ',finalDic, '\n')
  # cat('\n\n')
  
  return(c(initDic, finalDic))
  
}


# ---- get WQ state dataset for temp, sal, pH, & [Alk]
# return as vector: dic, ...

getWqState <- function(temp, sal, ph, alk) {
  
  initPh<-input$initPh   ; initPhFREE<-phNbsToPhFree(input$initPh,input$salInPpt,temp,0)
  initAlk<-input$initAlk ; initDic<-1000*calcDicOfAlk(initAlk/1000,initPhFREE,temp,input$salInPpt)
  
  finalPh<-input$finalPh   ; finalPhFREE<-phNbsToPhFree(finalPh,input$salInPpt,temp,0) 
  finalAlk<-input$finalAlk ; finalDic<-1000*calcDicOfAlk(finalAlk/1000,finalPhFREE,temp,input$salInPpt)
  
  initCo2<-calcCo2OfDic(initDic,temp,input$salInPpt,initPhFREE)
  finalCo2<-calcCo2OfDic(finalDic,temp,input$salInPpt,finalPhFREE)
  
  # some calcs for WQ display
  rho     <- calcRho(temp, input$salInPpt)
  kspca   <- calcKspCa(temp, input$salInPpt)
  kspar   <- calcKspAr(temp, input$salInPpt)
  k2      <- getK2(temp, input$salInPpt,0)
  caMolin <- convertCaToMolesPerKg(input$ca, temp, input$salInPpt)
  
  initOmegaCa<-calcOmegaCa(initDic/1000,caMolin,temp,input$salInPpt,initPhFREE)
  initOmegaAr<-calcOmegaAr(initDic/1000,caMolin,temp,input$salInPpt,initPhFREE)
  finalOmegaCa<-calcOmegaCa(finalDic/1000,caMolin,temp,input$salInPpt,finalPhFREE)
  finalOmegaAr<-calcOmegaAr(finalDic/1000,caMolin,temp,input$salInPpt,finalPhFREE)
}


# ---- user-defined validation ----

# If return NULL, validate ignored and app proceeds as normal
# If return a character string, validate fails and returns the string
# If return FALSE, validate fails silently
#    Shiny will not continue with the app (which would result in a red error message), 
#    but it will not display a grey validation error message either.


# validator<-function(input,comment,LL,UU,units) {
#   cat('in validate...',comment,'...',input,'...',LL,'...',UU,'...',units,'\n')
#   w<-paste('Please enter a ',comment,' between ',LL,' and ',UU,' ',units)
#   
#   if(input < LL || input > UU) {
#     w
#   } else if(input == '' || !is.numeric(input)) {
#     #    FALSE
#     'Please enter a valid temperature value, eh?'
#   } else {
#     NULL
#   }
# }
# 
# 
# # **** [DEPRECATED] in iQuaCalc ----
# # ---- convert init and final [Alk] to I.C. units
# # alkToIcUnits <- function(alk, alkIndex, icTemp, icSal) {
# #   cat('alkToIcUnits(',alk,',',alkIndex,',',icTemp,',',icSal,')\n')
# #   rho<-calcRho(icTemp,icSal)
# #   cat('rho = ',rho,'\n')
# #   
# #   
# #   if(1 == alkIndex) {
# #     return(alk/1000)
# #   } else {   # convert from ppm CaCO3 to meq/kg
# #     return( (alk  / 50.04345) / 1000 )
# #   }
# #   
# # # ---- NB: OLD code, when drop-down list with meq/kg, meq/L, ppm CaCO3, & dK
# # #   if(1 == alkIndex) {
# # #     return(alk/1000)
# # #   } else if(2 == alkIndex) {   # convert from meq/L to meq/kg
# # #     return((alk/1000)  * (rho/1000))
# # #   } else if(3 == alkIndex) {   # convert from ppm CaCO3 to meq/kg
# # #     return((alk  / 50.04345) / 1000)
# # #   }
# # }
# 
# # ---- Convert Temp ----
# 
# tempToIcUnits <- function(temp,tempUnitsIndex) {
#   if(1 == tempUnitsIndex) {
#     return(temp+273.15)
#   } else if(2 == tempUnitsIndex) {
#     return((5*(temp-32)/9)+273.15)
#   } else {
#     return(temp)
#   }
# }
# 
# 
# # ---- Convert Sal to I.C. Units, (ppt) g/kg----
# 
# # @sal      - input salinity value
# # @salUnits - index of salinity units from drop-down widget
# # @temp     - temp passed in as I.C. units (K), but -- N.B. -- each auxiliary method requires Celcius
# # @tc       - temp correction flag for hydrometer conversions
# salToIcUnits <- function(sal,salUnits,temp,tc) {
#   
#   # convert temp in K to temp in C for methonds below
#   temp <- temp - 273.15
#     
# #  '‰'=1,'μS/cm'=2,'S/m'=3,'mS/cm'=4,'dS/m'=5,
# #  '15C/4C'=6,'20C/20C'=7,'60F/60F'=8,'77F/77F'=9
#     if (1 == salUnits) {
#       return(sal)
#     } else if (2 == salUnits) {     # ** NB: first convert to S/m
#         return(calcSwSalinityFromConductivity(sal*0.0001,temp,0))
#     } else if (3 == salUnits) {
#         return(calcSwSalinityFromConductivity(sal,temp,0))
#     } else if (4 == salUnits) {
#         return(calcSwSalinityFromConductivity(sal * 0.1,temp,0))
#     } else if (5 == salUnits) {
#         return(calcSwSalinityFromConductivity(sal * 0.1,temp,0))
#     } else if (6 == salUnits) {
#         return(calcSalOfSpGr(sal,temp,4.0,15.0,tc))
#     } else if (7 == salUnits) {
#         return(calcSalOfSpGr(sal,temp,20.0,20.0,tc))
#     } else if (8 == salUnits) {
#         return(calcSalOfSpGr(sal,temp,15.5556,15.5556,tc))
#     } else {                        # ** 77F/77F
#         return(calcSalOfSpGr(sal,temp,25.0,25.0,tc))
#     }
# }
# 
# # http://www.code10.info/index.php?option=com_content&view=article&id=65:conversion-between-conductivity-and-pss-78-salinity&catid=54:cat_coding_algorithms_seawater&Itemid=79
# #...valid within the temperature range between –2°C and +35°C, 
# # pressure range between 0 and 10000 decibars and a practical salinity range between 2 and 42 
# # or the respective electrical conductivity and conductivity ratio.
# # Although practical salinity values < 2 are not defined, the equations deliver valid non-zero results 
# # down to thresholds of conductivity ratios > 0.0005 and salinities > 0.02. 
# # Values in these outer limits are estimates congruent with the Fortran algorithms (UNESCO 1983). 
# # Below these thresholds functions return 0 (UNESCO 1983).
# 
# # t in C and p in dbar
# calcSwSalinityFromConductivity <- function(c,t,p) {
#   #            double sal = 0.0;
#   # ** NB: Must add myPressure and convert myConductivity to S/m for this calc
#   #            double t = myTemp;     // ** C
#   #            double p = myPressure; // ** dbar
#   
# # NB: With this algorithm, MUST CHANGE INPUT temp from KELVIN --> CELCIUS
# #     Unlike WQ Tech 0.95 in Xcode, this conversion performed in salToIcUnits()
#   
#   # ** @param c = C(S,t,p) is the measured electrical conductivity
#   # ** myConductivity = C(35,15,0) = electrical conductivity of standard SW,
#   # ** which is 4.2914 S/m
#   # ** e.g.: http://www.code10.info/index.php?option=com_content&view=article&id=65:conversion-between-conductivity-and-pss-78-salinity&catid=54:cat_coding_algorithms_seawater&Itemid=79
#   
#   # ** S/m (or 42914 μS/cm) 
#   myConductivity <- 4.2914
#   
#   # ** 1. calculate conductivity ratio R
#   myR <- c / myConductivity
#   
#   # ** 2. calculate rsubt -- function of temperature
#   c0 <- 0.6766097;      c1 <-  0.0200564
#   c2 <- 0.0001104259;   c3 <- -0.00000069698
#   c4 <- 0.0000000010031
#   
#   rSubT <- c0 + c1*t + c2*t*t + c3*t*t*t + c4*t*t*t*t
#   
#   # ** 3. calculate Rsubp -- function of pressure
#   e0 <- 0.00002070;      e1 <- -0.0000000006370
#   e2 <- 0.000000000000003989
#   d1 <- 0.03426;     d2 <-  0.0004464
#   d3 <- 0.4215;      d4 <- -0.003107
#   
#   RsubP <- 1 + p*(e0 + e1*p + e2*p*p) / (1 + d1*t + d2*t*t + (d3 + d4)* myR)
#   
#   # ** 4. calclate RsubT
#   RsubT <- myR / (rSubT * RsubP)
#   
#   # ** 5. calculate S'
#   k  <-  0.0162
#   b0 <-  0.0005; b1 <- -0.0056; b2 <- -0.0066
#   b3 <- -0.0375; b4 <-  0.0636; b5 <- -0.0144
#   
#   Sprime <- (t - 15) / (1 + k*(t - 15))
#   
#   Sprime <- Sprime * (b0 + b1*sqrt(RsubT) + b2*RsubT + 
#                         b3*RsubT^(1.5) + 
#                         b4*RsubT*RsubT + b5*RsubT^(2.5))
#   
#   # ** 6. finish it off to calc salinity
#   a0 <-  0.0080; a1 <- -0.1692; a2 <- 25.385
#   a3 <- 14.0941; a4 <- -7.0261; a5 <-  2.7081
#   
#   mySal <- a0 + a1 * sqrt(RsubT) + 
#   a2 * RsubT + a3 * RsubT^(1.5) + 
#   a4 * RsubT*RsubT + a5 * RsubT^(2.5) + 
#   Sprime
#   
#   return(mySal)
# }
#   
# 
# # // ** TEST THIS METHOD TO CALC SALINITY from Sp. Gr.
# # // ** NB0: Must CHANGE TEMP for K (IC units) to Celcius
# # // ** NB1: changed params of original method in WqCalc
# # // **      from NSString to double
# # // ** NB2: 77 F / 77 F => (ref temp) / (standard temp)
# # // ** NB3: MUST trap case in which, for a given sample temp, a sp. gr. reading
# # // **      yields a salinity < 0.0
# 
# # @tc       - temp correction flag for hydrometer conversions
# calcSalOfSpGr <- function(spgr,tSample,tStandard,tReference,tc) {
#   
# # NB: With this algorithm, MUST CHANGE INPUT temp from KELVIN --> CELCIUS
# #     Unlike WQ Tech 0.95 in Xcode, this conversion performed in salToIcUnits()
# #     (same with calcRhoFW and within calcSalFromSpGravity)
#          
# # calc densities at sample, reference, & standard tempertures
# # ** NB: in **THIS** implementation (vs. WqCalc), must add back 273.15 to pass Kelvin temp...
#     rhoSample     <- calcRhoFW(tSample + 273.15)		  # should be 0.996542 at 27C??
#     rhoReference  <- calcRhoFW(tReference + 273.15)		# should be 0.998232 at 20C??
# #	rhoStandard   = [carbCalc calcRhoFW:(tempStandard + 273.15)];		// should be  at C??
# 
# cat('in ... validator_for_iquacalc.R ...')
# # cat('in CALC_SAL_OF_SPGR, SPGR:',spgr,'\n')
# # cat('in CALC_SAL_OF_SPGR, temp:',tSample,'...',tStandard,'...',tReference,'\n')
# # cat('in CALC_SAL_OF_SPGR, rho:',rhoSample,'...',rhoReference,'\n')
#          
#     # 1: correct hydrometer reading with REFERENCE temp, if required
#          if (tc) {
#           gamma <- 0.000030    # hydrometer coefficient of expansion (C^-1)
#          # ** check data correction: +0.0015 for sp. gr. hydrometer; -0.0002 for density hydrometer
#          
#           c <- spgr * (rhoReference / (rhoSample * (1 + gamma * (tSample - tReference))) - 1)
#          #   Rho  = -rhoReference * gamma * (tempSample - tempReference) / 1000; // mult by 1000 to express rho in g/ml
#           spgr <- spgr + c
#          }
#          
#     # 2: calc sample salinity with CarbCalc method(s)
# # ** [KLUDGE] **
# # ** to 'help' VALIDATION & CONVERSION play nice...?
# cat('')
#       if(spgr >= 0.99 || spgr <= 1.0399)
#          mySal <- calcSalFromSpGravity(spgr,tSample,tStandard)
#       else
#          mySal <- 0.0
#          
# # NB: [KLUDGE] validate input to avoid returning mySal < 0...
#          if (mySal < 0 || mySal > 45) mySal <- 0
#          
#          
#          return(mySal)
# }
#          
# # ** NB: NB: NB: Temp sent in CELCIUS, *BUT* when send to calcRhoFW:,
# # **             now must send in KELVIN (unlike implementation in WqCalc project)
# calcSalFromSpGravity <- function(spGrav,tempRead,tempCal) {
#   
#   A  <-   0.824493 - 0.0040899 * tempRead
#   A  <- A +   0.000076438 * tempRead * tempRead
#   A  <- A +  -0.00000082467 * tempRead^3
#   A  <- A +   0.0000000053875 * tempRead^4
#   
#   B  <-  -0.00572466 + 0.00010227 * tempRead
#   B  <- B +  -0.0000016546 * tempRead * tempRead
#   
#   C  <-   0.00048314
#  
#   mySal <- calcSecantMethod(C,B,A,0.0,calcRhoFW(tempRead+273.15) - calcRhoFW(tempCal+273.15) * spGrav)
#   
#   mySal <- mySal * mySal
#   
#   cat('in CALC_SAL_**FROM**_SPGR:',spGrav,'...sample temp',tempRead,'...calibration temp',tempCal,'\n')
#   cat('...salinity = ',mySal,'\n')
#     
#   return(mySal)
# }






