
// see: https://www.w3schools.com/js/js_whereto.asp
// Old JavaScript examples may use a type attribute: <script type="text/javascript">.
// The type attribute is not required. JavaScript is the default scripting language in HTML.
//<SCRIPT TYPE = "text/javascript">
//<script>

//<!--

// ENUM of adjustment reagents

var REAGENTS = {
  NaHCO3 : {name: "nahco3", cmpd: "NaHCO\u2083",      mw:  84.00661, m: 1,       mRad: Math.PI/4,    meq_mmol: 1},
  Na2CO3 : {name: "na2co3", cmpd: "Na\u2082CO\u2083", mw: 105.98844, m: 2,       mRad: Math.atan(2), meq_mmol: 2}, // + * 7H20 => 232.0954
  NaOH   : {name: "naoh",   cmpd: "NaOH",             mw:  39.99711, m: 1000000, mRad: Math.PI/2,    meq_mmol: 1}, // NB: pi/2 slope set to 10^6
  HCl    : {name: "hcl",    cmpd: "HCl",              mw:  36.46094, m: 1000000, mRad: 3*Math.PI/2,  meq_mmol: 1}, // NB: directed DOWNWARD...
  CaOH2  : {name: "caoh2",  cmpd: "Ca(OH)\u2082",     mw:  74.09268, m: 1000000, mRad: Math.PI/2,    meq_mmol: 2},
  CaCO3  : {name: "caco3",  cmpd: "CaCO\u2083",       mw: 100.0869,  m: 2,       mRad: Math.atan(2), meq_mmol: 2},
  CaO    : {name: "cao",    cmpd: "CaO",              mw:  56.0774,  m: 1000000, mRad: Math.PI/2,    meq_mmol: 2},
  MgCO3  : {name: "mgco3",  cmpd: "MgCO\u2083",       mw:  84.3139,  m: 2,       mRad: Math.atan(2), meq_mmol: 2},
  MgOH2  : {name: "mgoh2",  cmpd: "Mg(OH)\u2082",     mw:  58.31968, m: 1000000, mRad: Math.PI/2,    meq_mmol: 2},
  CO2_Remove: {name: "minusCo2",cmpd: "-CO\u2082",    mw:  44.0096,  m: 0,       mRad: Math.PI,      meq_mmol: 0},
  CO2_Add:    {name: "plusCo2", cmpd: "+CO\u2082",    mw:  44.0096,  m: 0,       mRad: 0,            meq_mmol: 0}
};


// ** Get enum data for an added reagent

function getEnumData(reagent) {

  if(reagent == REAGENTS.NaHCO3.name)
    return(REAGENTS.NaHCO3);
  else if(reagent == REAGENTS.Na2CO3.name)
    return(REAGENTS.Na2CO3);
  else if(reagent == REAGENTS.NaOH.name)
    return(REAGENTS.NaOH);
  else if(reagent == REAGENTS.HCl.name)
    return(REAGENTS.HCl);
  else if(reagent == REAGENTS.CaOH2.name)
    return(REAGENTS.CaOH2);
  else if(reagent == REAGENTS.CaCO3.name)
    return(REAGENTS.CaCO3);
  else if(reagent == REAGENTS.CaO.name)
    return(REAGENTS.CaO);
  else if(reagent == REAGENTS.CO2_Remove.name)
    return(REAGENTS.CO2_Remove);
  else if(reagent == REAGENTS.CO2_Add.name)
    return(REAGENTS.CO2_Add);
}


// ** Compute the WQ adjustment

function calcAdjustment(initDic, initAlk, finalDic, finalAlk) {
  
// document.write('Quadrant = ' + initDic);

  var t = document.getElementById('tempInC').value;
  var s = document.getElementById('salInPpt').value;

  var vol = document.getElementById('vol').value;

  var initPh  = document.getElementById('initPh').value;
  var initAlk = document.getElementById('initAlk').value/1000;

  var finalPh  = document.getElementById('finalPh').value;
  var finalAlk = document.getElementById('finalAlk').value/1000;

            var myP1 = getEnumData(cbChecked[0]); 
            var myP2 = getEnumData(cbChecked[1]);

            var lowerReagent;
            var higherReagent;

  var deltaAlk = finalAlk - initAlk;
  var deltaDic = finalDic - initDic;
        
  var wpSlope    = deltaAlk / deltaDic; // slope between waypoints
  var wpSlopeRad = Math.atan(wpSlope);


// ** 0. Clear chemAdjustment1 & 2...
        var chemAdjustment1 = 0;
        var chemAdjustment2 = 0;

// ** 1. Are there exactly TWO cbs checked?
        if(2 == cbChecked.length) {

// ** 1a Recover adjustment data for the two selected reagents
            
// ** 2. ID the reagent with the lower slope (AND WHEN THE SLOPES ARE EQUAL?)
// ** (Ans: When slopes are equal, disable all but first of those selected)
//            var lowerReagent = ((myP1.m < myP2.m) ? myP1 : myP2);

            if(myP1.mRad < myP2.mRad) {
                lowerReagent  = myP1;
                higherReagent = myP2;
            } else {
                lowerReagent  = myP2;
                higherReagent = myP1;
            }
         }

// ** 2a Re-assign elements of cbChecked so that lowerReagent is first
            cbChecked.length = 0;
            cbChecked.push(lowerReagent.name);
            cbChecked.push(higherReagent.name);

// ** 3. Is wpTerminal within the adjustment region? (test slopes & Q-is)
// ** 3a adjLowerSlope < wpSlope < adjUpperSlope ?

            var quadrant = 0;

            if(deltaDic >= 0 && deltaAlk >= 0)
                quadrant = 1;
            else if (deltaDic < 0 && deltaAlk >= 0)
                quadrant = 2;
            else if (deltaDic < 0 && deltaAlk < 0)
                quadrant = 3;
            else if (deltaDic >= 0 && deltaAlk < 0)
                quadrant = 4;
            else
                ;

            if(2 == quadrant)
                wpSlopeRad += Math.PI; // ** 2nd-Quadrant correction
            else if(3 == quadrant)
                wpSlopeRad += Math.PI;
            else if(4 == quadrant)
                wpSlopeRad += 2 * Math.PI;
            else
                ;

            var bContinueCalc = true;

            if(higherReagent.mRad - lowerReagent.mRad < Math.PI) {
// ** NB: >>>>>>>>>>>>>>>> MUST CHANGE THIS TEST, as higher/lower in terms of rads...
// **     >>>>>>>>>>>>>>>> but defined above in terms of non-rad slopes...
                if(wpSlopeRad <= higherReagent.mRad &&
                   wpSlopeRad >= lowerReagent.mRad)
                    ;
                else {
                        bContinueCalc = false;
                }
            } else {
                if(wpSlopeRad >= higherReagent.mRad)
                    ;
                else {
                    bContinueCalc = false;
                }
            }

//document.write('Quadrant = '+quadrant);
//document.write('wpSlopeRad: '+wpSlopeRad+' higherReagent.mRad: '+higherReagent.mRad+' lowerReagent.mRad: '+lowerReagent.mRad);
//document.write('bContinueCalc = '+bContinueCalc);


    // ** 3b First, just deal with "Q-I" reagents; extend it later

                if(bContinueCalc) {
                    if(myP1.m < myP2.m) {
                        lowerReagent  = myP1;
                        higherReagent = myP2;
                    } else {
                        lowerReagent  = myP2;
                        higherReagent = myP1;
                    }

            cbChecked.length = 0;
            cbChecked.push(lowerReagent.name);
            cbChecked.push(higherReagent.name);

    // ** 4b Calc DIC coordinate of "adjustment intersection"
                    var dicStar = 0;
                    var alkStar = 0;

    // ** if the higher reagent is NaOH, CaO, or Ca(OH)2, slope > 2 (undefined, in fact)
                    if(higherReagent.m > 2) {

                        dicStar = finalDic;
                        alkStar = lowerReagent.m * (finalDic - initDic) + initAlk;
                    } else {

                        dicStar  =  initAlk - lowerReagent.m * initDic;
                        dicStar += -finalAlk + higherReagent.m * finalDic;
                        dicStar /= (higherReagent.m - lowerReagent.m);

        // ** 4. Calc [Alk] coordinate of "adjustment intersection"
                        alkStar  =  lowerReagent.m * (dicStar - initDic);
                        alkStar +=  initAlk;
                    }

        // ** 5. Calc amt of REAGENT 1 (with lower slope) to reach this DIC
        //            if(0 == lowerReagent.charge) { // if reagent with lower slope is CO2
                        chemAdjustment1 =  Math.abs((dicStar - initDic) * lowerReagent.mw * vol);
        //            }

        // ** 6. Use value in 4. to calc "[Alk] deficit"
                    var alkDeficit = Math.abs(finalAlk - alkStar);

        // *****************************************************************************
        // *****************************************************************************
        // ** 7. Use REAGENT 2 to calc amount needed to raise [Alk] to wpTerminal
        // **    NB: higherReagent.slope NEVER CAN BE '0' or 'undefined'
        //            if(higherReagent.slope < 1000000) // if 'undefined'
                        chemAdjustment2 = alkDeficit * (higherReagent.mw / higherReagent.meq_mmol) * vol;
        //            else
        //                chemAdjustment2 = alkDeficit * higherReagent.mw * dummyVol;
        // *****************************************************************************
        // *****************************************************************************

    // ** NB: ENFORCE NO NEGATIVE VALUES
    // ** If a deltaX is < 0, alert "Can't get there from here!" & make a recommendation

    // ** notify WQchartDlg ("Controller") to call getWaypointDICs() & getTMarkerAdjData()
//                    notifyWqObservers();
// ????                }
//            }
            } else {
                chemAdjustment1 = chemAdjustment2 = 0.0;
            }

  var myForm = document.getElementById('Reagents');
  var myTextArea = myForm.results;
  myTextArea.value = lowerReagent.cmpd+': '+chemAdjustment1.toFixed(2)+' kg'+'\n'+higherReagent.cmpd+': '+chemAdjustment2.toFixed(2)+' kg';

//document.write(lowerReagent.cmpd+': '+chemAdjustment1.toFixed(2)+' kg'+'\n'+higherReagent.cmpd+': '+chemAdjustment2.toFixed(2)+' kg');

}


// see: http://stackoverflow.com/questions/24814641/shiny-server-output-only-updates-after-clicking-in-textinput-box/24831529#24831529

// globals

var cbChecked   = [];
var cbUnchecked = [];
var cbFree      = [];

//var cbChecked   = Array();
//var cbUnchecked = Array();
//var cbFree      = Array();

//var cbChecked   = new Array();
//var cbUnchecked = new Array();
//var cbFree      = new Array();

cbChecked   = [];
cbUnchecked = ['nahco3','na2co3','naoh','caco3','caoh2','cao','plusCo2','minusCo2','hcl'];
cbFree      = [];

    
// ** if 2 reagents checked, disable all remaining cbs,
// ** else enable those not in conflict with any (one) checked cb

function maxReagentsLoop() {
        
  if(2 == cbChecked.length) {
        
    for(var k = 0; k < cbUnchecked.length; k++) {

      if(document.getElementById(cbUnchecked[k]).disabled === false) { // must be a non-complement of checked CBs
                    
        document.getElementById(cbUnchecked[k]).disabled = true;  // disable this non-complement
        cbFree.push(cbUnchecked[k]);    // add it to the list of non-complements
      }
     }
    } else {    // either 0 or 1 CB is checked
            
      if(cbFree.length > 0) { // if there are non-complements in the list
         for(var i = 0; i < cbFree.length; i++) {
           document.getElementById(cbFree[i]).disabled = false;  // re-enable them
         }
         cbFree.length = 0;                      // then clear the list
       } 
    }
        
        // ** account for case in which, with ONE check, can still change cb to TWO;
        // ** but with TWO reagents checked, cannot set cb to ONE without user
        // ** deciding which of the two checks to un-check
//      if(2 == checkedCBs.size())
//          this.cbNumReagents.setEnabled(false);
//      else
//          this.cbNumReagents.setEnabled(true);
}



// ** toggleCbs()
// ** NB: THIS version passes the CHECKBOX element to toggleCbs()
//        a version that better mimics my old Java code

function toggleCbs(cb) {

cbHit = cb.id;
cbHitFlag = cb.checked;

  // ** pass key reagent data to server to plot reagent vector
  var currentReagent = getEnumData(cbHit);
  // ** send: [unformatted name, formatted name, slope, checked-or-unchecked]
  var data = [currentReagent.name,currentReagent.cmpd,currentReagent.m,cbHitFlag];

//  Shiny.onInputChange("reagentData", data);

  if(cbHitFlag) {

    cbChecked.push(cbHit);
    cbUnchecked.splice(cbUnchecked.indexOf(cbHit),1);

    if(cbHit == 'nahco3') {
    } else if(cbHit == 'na2co3') {
      document.getElementById('caco3').disabled=true;
    } else if(cbHit == 'naoh') {
      document.getElementById('caoh2').disabled=true;
      document.getElementById('cao').disabled=true;
      document.getElementById('hcl').disabled=true;
    } else if(cbHit == 'caco3') {
      document.getElementById('na2co3').disabled=true;
    } else if(cbHit == 'caoh2') {
      document.getElementById('naoh').disabled=true;
      document.getElementById('cao').disabled=true;
      document.getElementById('hcl').disabled=true;
    } else if(cbHit == 'cao') {
      document.getElementById('naoh').disabled=true;
      document.getElementById('caoh2').disabled=true;
      document.getElementById('hcl').disabled=true;
    } else if(cbHit == 'plusCo2') {
      document.getElementById('minusCo2').disabled=true;
    } else if(cbHit == 'minusCo2') {
      document.getElementById('plusCo2').disabled=true;
    } else if(cbHit == 'hcl') {
      document.getElementById('naoh').disabled=true;
      document.getElementById('cao').disabled=true;
      document.getElementById('caoh2').disabled=true;
    }

  } else {

    cbChecked.splice(cbChecked.indexOf(cbHit),1);
    cbUnchecked.push(cbHit);

    if(cbHit == 'nahco3') {
    } else if(cbHit == 'na2co3') {
      document.getElementById('caco3').disabled=false;
    } else if(cbHit == 'naoh') {
      document.getElementById('caoh2').disabled=false;
      document.getElementById('cao').disabled=false;
      document.getElementById('hcl').disabled=false;
    } else if(cbHit == 'caco3') {
      document.getElementById('na2co3').disabled=false;
    } else if(cbHit == 'caoh2') {
      document.getElementById('naoh').disabled=false;
      document.getElementById('cao').disabled=false;
      document.getElementById('hcl').disabled=false;
    } else if(cbHit == 'cao') {
      document.getElementById('naoh').disabled=false;
      document.getElementById('caoh2').disabled=false;
      document.getElementById('hcl').disabled=false;
    } else if(cbHit == 'plusCo2') {
      document.getElementById('minusCo2').disabled=false;
    } else if(cbHit == 'minusCo2') {
      document.getElementById('plusCo2').disabled=false;
    } else if(cbHit == 'hcl') {
      document.getElementById('naoh').disabled=false;
      document.getElementById('cao').disabled=false;
      document.getElementById('caoh2').disabled=false;
    }

  }

// NB: Prefixed 'wq_map-' to conform to wq_map_module.R namespace
  Shiny.onInputChange("wq_map-checkedReagents", cbChecked);

  maxReagentsLoop();
}


// when hit button to calculate adjustment for two reagents
function fireAdjustmentCalc() {

//  var btnState = document.getElementById("adjustment").disabled=true;

    Shiny.onInputChange("adjustmentCalculation", Math.random());
//    Shiny.onInputChange("wq_map-adjustmentCalculation", Math.random());

}

//-->

//</script>
