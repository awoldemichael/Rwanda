/*-------------------------------------------------------------------------------
# Name:		01_hhvars
# Purpose:	recode and rename household assets for use in stunting analysis
# Author:	Tim Essam, Ph.D.
# Created:	2016/08/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathlog/01_hhassets", replace
use "$pathhh/RWHR70FL.dta"

* Replicating work done in 02_RW_cleanDHS_hh.r
/* Create a unique id for merging (not that compliciated! -- see: 
   http://dhsprogram.com/data/Merging-Datasets.cfm 
   Note: This id will not be unique hab/c it is measured at the individual level
   for now. Need to collapse down to hh level to get mergability 
   726 that have conflicts due to concatenation   
   */
clonevar v001 = hv001
clonevar v002 = hv002
isid v001 v002

****** NOTE : a better solution is to simply rename the merging variables ******

* clean up sampling information
clonevar cluster 	= hv001
clonevar hhnum 		= hv002
clonevar monthint 	= hv006
clonevar yearint	= hv007
clonevar intdate	= hv008
clonevar psu		= hv021
clonevar strata		= hv022
clonevar province	= hv024
clonevar altitude	= hv040
clonevar district 	= shdistrict

g hhweight = hv005 / 1000000
g maleweight = hv028/1000000

* Syntax for setting weights *
* svyset psu [pw = hhweight], strata(strata)

la var hhweight "household weight"
la var maleweight "male weight"

* Fix value labels on rural
recode hv025 (1 = 0 "urban")(2 = 1 "rural"), gen(rural)

* HH size and demographics
clonevar hhsize = hv009
clonevar hhchildUnd5 = hv014

* HH assets
clonevar toilet = hv205
clonevar toiletShare = hv225
rename (hv206 hv207 hv208 hv209 hv210 hv211 hv212 hv243a)(electricity radio tv refrig bike moto car mobile)

recode hv213 (11 12 = 1 "earth, sand, dung")(33 34 35 96 = 0 "ceramic or better"), gen(dirtfloor)
clonevar hhrooms = hv216
g roomPC = hhrooms / hhsize
la var roomPC "rooms per hh size"

recode hv219 (1 = 0 "male")(2 = 1 "female"), gen(femhead)
clonevar agehead = hv220
replace agehead = . if agehead == 98

recode hv230a (2 3 4 = 0 "unobserved") (1 = 1 "observed"), gen(handwash)
recode hv237 (8 = .)(0 = 0 "no")(1 = 1 "yes"), gen(treatwater)

recode hv242 (0 = 0 "no")(1 = 1 "yes")(. = .), gen(kitchen)
clonevar bednet = hv227 

**********************************************
* HH Landholding for agricultural production *
**********************************************
recode hv244 (1 = 0 "owns ag land")(0 = 1 "landless"), gen(landless)

clonevar landowned = hv245
replace landowned = . if landowned == 998
replace landowned = (landowned / 10)
*histogram landowned

clonevar livestock = hv246

/*Create TLU (based on values from http://www.lrrd.org/lrrd18/8/chil18117.htm)
Notes: Sheep includes sheep and goats
Horse includes all draught animals (donkey, horse, bullock)
chxTLU includes all small animals (chicken, fowl, etc).*/
g camelVal 	= 0.70
g cattleVal = 0.50
g pigVal 	= 0.20
g sheepVal 	= 0.10
g horsesVal = 0.50
g mulesVal 	= 0.60
g assesVal 	= 0.30
g chxVal 	= 0.01

* Decode unknown values to be missing not zero (affects few obs) and strip labels
mvdecode hv246a hv246b hv246j, mv(98)
_strip_labels hv246a-hv246j

/* So it appears that hv246b has two components hv246i hv246j, being milk cows
   and bulls. Three (3) records do not follow the pattern but otherwise this seems
   to be the breakdown. Strategy is to use traditional cows + milk + bull to 
   calculate TLU 
   egen testcow = rowtotal( hv246i hv246j)
   assert testcow == hv246b
 */
   
rename (hv246a hv246c hv246d hv246e hv246f hv246g hv246h hv246i hv246j)/*
      */ (cowtrad horse goat sheep chicken pig rabbit cowmilk cowbull) 
*summarize results to check min / max
sum cowtrad-cowbull
	    
g tlucattle = (cowtrad + cowmilk + cowbull) * cattleVal	  
g tlusheep 	= (sheep + goat) * sheepVal
g tluhorses = (horse) * horsesVal
g tlupig 	= (pig) * pigVal
g tluchx 	= (rabbit + chicken) * chxVal

* Generate overall tlus
egen tlutotal = rsum(tlucattle tlusheep tluhorses tlupig tluchx)
la var tlutotal "Total tropical livestock units"

sum tlutotal
*histogram tlutotal if livestock ==1 & tlutotal < 10

* Wealth
clonevar wealthGroup = hv270
clonevar wealth = hv271
replace wealth = (wealth / 100000)

* Bank account?
clonevar bankAcount = hv247

* Smoker in house?
recode hv252 (0 = 0 "no smoker") (1 2 3 4 = 1 "smoker in house"), gen(smoker)

* Drop extra variables no longer needed
drop *Val

* drop extra data
aorder

#delimit ;
ds(ha0* ha1* ha2* ha3* ha4* ha5* ha6* ha7* hb* hc* 
	hmhid* hml* hv* hvidx* sh1* sh2* sh03* sh4* idxh*
	shel*), not;
keep `r(varlist)';
#delimit cr

* Check for value labels and clean up ones that do not make sense
local labCheck agehead bankAcount bednet bike car dirtfloor /*
		*/ district electricity femhead handwash landless /*
		*/  landowned livestock mobile moto province radio /*
		*/ refrig rural smoker strata toilet toiletShare /*
		*/ treatwater tv

* Most everything looks ok, not sure we need value labels but will leave for now
set more on	
foreach x of local labCheck {
	tab `x', mi nol
	
	* Adding a toggle if you need to slow down loop
	*more
	}
*

_strip_labels agehead

* Summary stats to check if you can match DHS report
* Matching stats on pp. 24 of report httpssmok://www.dhsprogram.com/pubs/pdf/FR316/FR316.pdf
svyset psu [pw = hhweight], strata(strata)

svy: mean radio, over(rural)
svy: mean livestock, over(rural)

* Sort and create coef plot of district values
svy: mean landless, over(district)
matrix plot = r(table)'
matsort plot 1 "down"
matrix plot = plot'
coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,]))


saveold "$pathout/DHS_hhvar.dta", replace
log close

