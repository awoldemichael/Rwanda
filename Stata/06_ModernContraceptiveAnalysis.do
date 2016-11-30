/*-------------------------------------------------------------------------------
# Name:		06_ModernContraceptionAnalysis
# Purpose:	Create model of modern contraceptive use
# Author:	Tim Essam, Ph.D.
# Created:	2016/08/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

capture log close
log using "$pathlog/06_ModConAnalysis", replace
clear

use "$pathout/contraceptionAnalysis.dta"

*ssc install blindschemes, replace all
*set scheme plottig, permanently

* Look at summary statistics and some basic plots
	graph dot (mean) modernContra if flagContra == 1, over(lvdzone, sort(1))
	graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(district, sort(1))
	graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(province, sort(1))
	graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(ageGroup)
	graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(educYears)
	graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(religion, sort(1))

* Loop over covariates to see how contraception use varies across different categories
local stats ageGroup educ educSame /*
	*/ fecund parity religion famPlanExp distanceHC /*
	*/ wealthGroup empowerment married residStatus numChildUnd5

	foreach x of local stats {
		mean modernContra [iw = wweight] if flagContra == 1, over(`x') 
		}
*end


** Modern Contraceptive use across wealth distribution
	twoway (lpoly modernContra wealth if flagContra == 1 [aweight = wweight]),  /*
	*/ylabel(0.40(0.05)0.55) ysca(alt) xsca(alt) xlabel(, grid gmax) /*
	*/ legend(off) saving(main, replace)
	
	twoway histogram wealth if flagContra == 1, fraction ysca(alt reverse) ylabel(, nogrid)/*
	*/ fysize(25) xlabel(, grid gmax) saving(hx, replace)
	
	graph combine main.gph hx.gph, hole(2 4) imargin(0 0 0 0) 
	

	twoway (lpoly modernContra ageGap if flagContra == 1 & inrange(ageGap, -20, 10) [aweight = wweight]),  /*
	*/ylabel(0.40(0.05)0.55) ysca(alt) xsca(alt) xlabel(, grid gmax) /*
	*/ legend(off) saving(main, replace)
	
	twoway histogram ageGap if flagContra == 1 & inrange(ageGap, -20, 10), fraction ysca(alt reverse) ylabel(, nogrid)/*
	*/ fysize(25) xlabel(, grid gmax) saving(hx, replace)
	graph combine main.gph hx.gph, hole(2 4) imargin(0 0 0 0) 
	
	*Occupations by wife 
	graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(occupGroup, sort(1)) saving(occF, replace)
	graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(occupGroupHus, sort(1)) saving(occM, replace)
	graph combine occF.gph occM.gph, hole(3 4) imargin(0 0 0 0 )
	
* Specify covariates for exploratory regression models
	/* Categorical bases are as follows:
		ib(4).ageGroup 	--> 30-34 cohort
		ib(2).parity 	--> 3-4 Children 
		ib(0).fecund 	--> not fecund
		ib(0).educ 		--> no education
		ib(0).educPartner --> no education
		ib(1).religion --> Catholic
		ib(5).lvdzone --> Central Plateau Cassava and Coffee

	*/

	global demog "ib(4).ageGroup married numChildUnd5 residStatus urban"
	global health "ib(2).parity ib(0).fecund moreKidsWanted sameNumKids bedNetUse"
	global social "ib(1).religion famPlanRadio famPlanTV famPlanPrint distanceHC"
	global social2 "ib(1).religion famPlanExp empowerment"
	global humcap "ib(0).educ ib(0).educPartner ageGap wealth"
	global comm "dist_distanceHC dist_totChild dist_educYears catholic_dominant protestant_dominant adventist_dominant muslim_dominant"
	global geog2 "altitude2 ib(5).lvdzone"
	global stderr "cluster(dhsclust)"

	est clear
	eststo mcu_b1: logit modernContra $demog $health ib(1381).intdate if flagContra == 1, $stderr or
	eststo mcu_a1: reg modernContra $demog $health ib(1381).intdate if flagContra == 1, $stderr 
	eststo mcu_b2: logit modernContra $demog $health $social ib(1381).intdate if flagContra == 1, $stderr or
	eststo mcu_a2: reg modernContra $demog $health $social ib(1381).intdate if flagContra == 1, $stderr 
	eststo mcu_b3: logit modernContra $demog $health $social $humcap ib(1381).intdate if flagContra == 1, $stderr or
	eststo mcu_a3: reg modernContra $demog $health $social $humcap ib(1381).intdate if flagContra == 1, $stderr 
	eststo mcu_b4: logit modernContra $demog $health $social $humcap $comm ib(1381).intdate if flagContra == 1, $stderr or
	eststo mcu_a4: reg modernContra $demog $health $social $humcap $comm  ib(1381).intdate if flagContra == 1, $stderr 
	eststo mcu_b5: logit modernContra $demog $health $social $humcap $comm $geog2 ib(1381).intdate if flagContra == 1, $stderr or
	eststo mcu_a5: reg modernContra $demog $health $social $humcap $comm $geog2  ib(1381).intdate if flagContra == 1, $stderr 
	esttab mcu*, se star(* 0.10 ** 0.05 *** 0.01) label ar2 pr2 beta not /*eform(0 0 1 1 1)*/ compress
	esttab mcu_b* using "$pathreg/MCUwideAll_logit.csv", wide mlabels(none) ar2 pr2  eform label replace not
	esttab mcu_a* using "$pathreg/MCUwideAll_lpm.csv", wide mlabels(none) ar2 pr2  label replace not





	eststo sted2_9: logit modernContra $matchar $hhchar $hhag2 $demog female $chldchar $chealth $geog ib(1381).intdate if year == 2014, $stderr or 
	eststo sted2_10: logit modernContra $matchar $hhchar $hhag2 $demog female $chldchar $chealth $geog2 ib(1333).intdate if year == 2010, $stderr or 
	eststo sted2_11: logit modernContra $matchar $hhchar $hhag2 $demog female $chldchar $chealth $geog2 ib(1381).intdate if year == 2014, $stderr or 
	esttab sted*, se star(* 0.10 ** 0.05 *** 0.01) label ar2 pr2 beta not /*eform(0 0 1 1 1)*/ compress
	* export results to .csv
	esttab sted* using "$pathout/`x'WideAll.csv", wide mlabels(none) ar2 pr2 beta label replace not
	est clear




	ib(1381).intdate
	
