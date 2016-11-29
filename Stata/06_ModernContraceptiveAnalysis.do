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

ssc install blindschemes, replace all
set scheme plottig, permanently

* Look at summary statistics and some basic plots
graph dot (mean) modernContra if flagContra == 1, over(lvdzone, sort(1))
graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(district, sort(1))
graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(ageGroup)
graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(educYears)
graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(occupGroup, sort(1))
graph dot (mean) modernContra [pw = wweight] if flagContra == 1, over(religion, sort(1))


foreach x of varlist fecund religion famPlanExp distanceHC wealthGroup empowerment married{
	mean modernContra [iw = wweight] if flagContra == 1, over(`x') 
	}
*end


