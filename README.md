This is a Negative Binomial Generalised Linear Model (glm.nb) using the broom package. 
This produces the same outputs observed from Amelias stata code which is detailed below.



//..............................................................................
// SERRATIA SPP.
//..............................................................................


// BEFORE COVID -----------------------------------------------------------------

import excel "F:\Projects & programmes\COVID-19\HPRU impact of COVID\PAPER1_bsi_increases\3) Code\18) ITA\3) output\ser.xlsx", sheet("Sheet 1") firstrow clear

gen time=_n
gen t2=time-62 // Centre time on COVID 0 where covid starts
gen month = mod(_n-1, 12) + 1
preserve
keep if covid==1
gen predset=1 // just gives it a label
replace covid=0	// change to no COVID
replace items=.	// ensure no data (could use if statement for model)
save temp, replace
restore

append using temp

nbreg items i.month c.t2##i.covid, irr

// model
// compares between before and after
nbreg items i.month c.t2, irr

// REGRESS and then PREDICT (ALL DATA) to check the model
predictnl cases_pred=predict(n), se(se) ci(lb ub)
gen model_fit = cases_pred
keep year_month model_fit
duplicates drop
save "F:\Projects & programmes\COVID-19\HPRU impact of COVID\PAPER1_bsi_increases\3) Code\18) ITA\3) output\temp.dta", replace


// AFTER COVID -----------------------------------------------------------------

// Download your base download
import excel "F:\Projects & programmes\COVID-19\HPRU impact of COVID\PAPER1_bsi_increases\3) Code\18) ITA\3) output\ser.xlsx", sheet("Sheet 1") firstrow clear

// left join in the temp.dta file and join them by year_month and items
merge 1:1 year_month using "F:\Projects & programmes\COVID-19\HPRU impact of COVID\PAPER1_bsi_increases\3) Code\18) ITA\3) output\temp.dta"

// Generate your time t2 and month
gen time=_n
gen t2=time-62 // Centre time on COVID 0 where covid starts
gen month = mod(_n-1, 12) + 1

// Make a preserve for only those within COVID
preserve
keep if covid==1
gen predset=1 // just gives it a label
replace covid=0	// change to no COVID
replace items=.	// ensure no data (could use if statement for model)
save temp, replace
restore

// reappend the base
append using temp

// if you are predicting numbers from observed values then use this
nbreg items i.month c.t2##i.covid if covid==0, irr // neg binomial regression incident rate ratio

// Explanation: this is just telling us what the trend is prior to covid where there is no information in the Items post covid in the model (cound to cases) 
// this shows that there was a slight increase trend (significant) in month and in seasons for IRR.

// GEN COUNTERFACTUAL (NO INTERVENTION)
// predict pred // Gives the counterfactual based on model above 
predictnl cases_pred=predict(n), se(se) ci(lb ub) // prediction with SE and 95%CI
// cases_pred == predicted values based off cases prior to COVID-19

// THIS IS GENERATING date_month INTO A FORMAT TO BE USED AS THE X AXIS
gen date_month = monthly(year_month, "YM")
format date_month %tm  // Format as "YYYYmMM"

// THIS IS GENERATING THE ACTUAL CHART		 
twoway (line items pred date_month if predset==.) /// 
       (line cases_pred date_month if predset==1, lc(maroon)) /// 
	   (line model_fit date_month if predset==., lc(black) lpattern(dash)) ///
       , xline(`=tm(2020m2)') legend(order(1 "Observed" 2 "Counterfactual" 3 "Models fit") rows(1)) /// 
         ytitle("Episode number") ///
		 title("Interrupted Time Series Analysis of {it:Serratia} spp.")
		 
graph export "F:\Projects & programmes\COVID-19\HPRU impact of COVID\PAPER1_bsi_increases\3) Code\18) ITA\3) output\ser_plot.png", as(png) replace

save "F:\Projects & programmes\COVID-19\HPRU impact of COVID\PAPER1_bsi_increases\3) Code\18) ITA\3) output\ser.dta", replace
