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
	
	