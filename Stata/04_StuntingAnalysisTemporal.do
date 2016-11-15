/*-------------------------------------------------------------------------------
# Name:		04_StuntingAnalysisTemporal
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

use "$pathout/DHS_2015_Stunting.dta", clear
ren district district2015
append using "C:\Users\Tim\Documents\Rwanda\Dataout\DHS_2010_Stunting.dta"

* Fix up districts
* 2010 district labels - 
label list shdistr

#delimit ;
recode district (1 =11 "Nyarugenge")
(2 =12 "Gasabo")
(3 =13 "Kicukiro")
(4 =21 "Nyanza")
(5 =22 "Gisagara")
(6 =23 "Nyaruguru")
(7 =24 "Huye")
(8 =25 "Nyamagabe")
(9 =26 "Ruhango")
(10 =27 "Muhanga")
(11 =28 "Kamonyi")
(12 =31 "Karongi")
(13 =32 "Rutsiro")
(14 =33 "Rubavu")
(15 =34 "Nyabihu")
(16 =35 "Ngororero")
(17 =36 "Rusizi")
(18 =37 "Nyamasheke")
(19 =41 "Rulindo")
(20 =42 "Gakenke")
(21 =43 "Musanze")
(22 =44 "Burera")
(23 =45 "Gicumbi")
(24 =51 "Rwamagana")
(25 =52 "Nyagatare")
(26 =53 "Gatsibo")
(27 =54 "Kayonza")
(28 =55 "Kirehe")
(29 =56 "Ngoma")
(30 =57 "Bugesera"), gen(district2010);
#delimit cr

drop district
g district = .
replace district = district2010 if year == 2010
replace district = district2015 if year == 2014
la val district SHDISTRI

* 2015 district labels
label list SHDISTRI

* Check data overtime by district/livelihood zone
foreach x of varlist stunting2 stunted2 extstunted2 {
	egen `x'_dist2010 = mean(`x') if year == 2010, by(district)
	egen `x'_dist2015 = mean(`x') if year == 2014, by(district)
	egen `x'_lvd2010 = mean(`x') if year == 2010, by(lvdzone)
	egen `x'_lvd2015 = mean(`x') if year == 2014, by(lvdzone)
}
*end
graph dot (mean) stunted2_dist2010 stunted2_dist2015, over(district, sort(2))
graph dot (mean) stunted2_lvd2010 stunted2_lvd2015, over(lvdzone, sort(2))
