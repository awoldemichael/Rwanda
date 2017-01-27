/*-------------------------------------------------------------------------------
# Name:		07_ModContraceptiveAnalysisTemporal.do
# Purpose:	Compare stunting results over time
# Author:	Tim Essam, Ph.D.
# Created:	2016/08/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/


clear
capture log close
log using "$pathlog/04_StuntingAnalysisTemporal.txt", replace

use "$pathout/MCU_DHS2014.dta", clear
g year = 2014
ren district district2015
append using "$pathout\MCU_DHS2010.dta"
replace year = 2010 if year == .


* Fix up districts
* 2010 district labels - 
label list shdistr SDISTRIC

#delimit ;
recode district (1 = 11 "Nyarugenge")
(2 = 12 "Gasabo")
(3 = 13 "Kicukiro")
(4 = 21 "Nyanza")
(5 = 22 "Gisagara")
(6 = 23 "Nyaruguru")
(7 = 24 "Huye")
(8 = 25 "Nyamagabe")
(9 = 26 "Ruhango")
(10 = 27 "Muhanga")
(11 = 28 "Kamonyi")
(12 = 31 "Karongi")
(13 = 32 "Rutsiro")
(14 = 33 "Rubavu")
(15 = 34 "Nyabihu")
(16 = 35 "Ngororero")
(17 = 36 "Rusizi")
(18 = 37 "Nyamasheke")
(19 = 41 "Rulindo")
(20 = 42 "Gakenke")
(21 = 43 "Musanze")
(22 = 44 "Burera")
(23 = 45 "Gicumbi")
(24 = 51 "Rwamagana")
(25 = 52 "Nyagatare")
(26 = 53 "Gatsibo")
(27 = 54 "Kayonza")
(28 = 55 "Kirehe")
(29 = 56 "Ngoma")
(30 = 57 "Bugesera"), gen(district2010);
#delimit cr

drop district
g district = .
replace district = district2010 if year == 2010
replace district = district2015 if year == 2014
la val district SHDISTRI

* 2015 district labels
label list SHDISTRI

global filter1 "flagContra == 1 [iw = wweight]"

foreach x of varlist religion lvdzone educ {
	mean modernContra if year == 2010 & $filter1, over(`x')
	mean modernContra if year == 2014 & $filter1, over(`x')
}


* Look at occupations over time and how mcu changed from year to year
eststo mcu2010: mean modernContra if year == 2010 & $filter1, over(district)
eststo mcu2014: mean modernContra if year == 2014 & $filter1, over(district)
coefplot mcu2010 mcu2014,  vertical cismooth bycoefs

* Survey set data for 2014 and build a matrix of results
matrix drop _all	
forvalues y = 2010(4)2014 {
	svyset [pw = wweight], psu(psu) strata(strata)
	
	local i = 0
	display in yellow "iterating over `y' estimates"
	foreach x of varlist ageGroup religion wealthGroup educ {
		
			svy:mean modernContra if year == `y', subpop(flagContra) over(`x')
			matrix A = r(table)'
			matrix B = e(_N)'
			matrix C_`i' = A, B
			
			local i = `i' + 1
			matrix drop A B
		}
	
		mean modernContra if flagContra == 1 & year == `y', over(lvdzone)
		matrix A = r(table)'
		matrix rownames A = 
		
		matrix B = e(_N)'
		matrix C = A, B
		matrix drop A B
			
	matrix mcu_`y' = C \ C_0 \ C_1 \ C_2 \ C_3
	*mat2txt, matrix(mcu_`y') saving("$pathreg/mcu_`y'") replace
}	
	
	* Create year flags for each matrix
	
	mat y2014 = J(34, 1, 2014)
	mat y2010 = J(34, 1, 2010)
	mat yflag = y2010 \ y2014

	mat mcu = mcu_2010 \ mcu_2014
	mat mcu = mcu, yflag
	mat colnames mcu = "mean" "se" "t" "pvalue" "lower" "upper" "df" "crit" "eform" "N" "year"
	mat2txt, matrix(mcu) saving("$pathreg/mcu_estimates") replace
	
	preserve
	xsvmat double mcu, norestore rownames(param) rowlabels(varlab) names(matcol)

		
	#delimit ;
	matrownames mcu_14 = "Urban Areas" 
	"Kivu" 
	"Western Congo-Nile Crest Tea" 
			"Northwestern Volcanic Irish Potato" 
			"Eastern Congo-Nile Highland Subsistence Farming"
			"Central Plateau Cassava and Coffee"
			"Northern Highland Beans and Wheat"
			"Central-Northern Highland Irish Potato, Beans and Vegetables"
			"Bugesera Cassava"
			"Eastern Plateau Mixed Agricultural"
			"Southeastern Plateau Banana"
			"Eastern Agropastoral"
			"Eastern Semi-Arid Agropastoral"
	#delimit cr
