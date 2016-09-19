/*-------------------------------------------------------------------------------
# Name:		03_stunting
# Purpose:	Create stunting variables and dietary diversity variables
# Author:	Tim Essam, Ph.D.
# Created:	2016/08/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/


clear
capture log close
use "$pathkids\RWKR70FL.dta", clear
log using "02_stunting", replace

* Flag children selected for anthropmetry measures
g cweight = (v005/1000000)
clonevar anthroTag = v042
keep if anthroTag == 1
clonevar DHSCLUST = v001

clonevar stunting = hw5
clonevar stunting2 = hw70

foreach x of varlist stunting stunting2 {
	replace `x' = . if inlist(`x', 9998, 9996)
	replace `x' = `x' / 100
	}
*end

g byte stunted = (stunting < -2.0)
replace stunted = . if stunting == .

g byte stunted2 = (stunting2 < -2.0)
replace stunted2 = . if stunting2 == .

g byte extstunted = (stunting < -3.0)
replace extstunted =. if stunting == .

g byte extstunted2 = (stunting2 < -3.0)
replace extstunted2 = . if stunting2 == .

clonevar ageChild = hw1
clonevar age_group = v013

egen ageMonGroup = cut(ageChild), at(0, 6, 9, 12, 18, 24, 36, 48, 60) label

recode b4 (1 = 0 "male")(2 = 1 "female"), gen(female)

* Stunting averages grouping
egen ageg_stunting = mean(stunting), by(age_group)
egen age_stunting = mean(stunting), by(ageChild)
la var ageg_stunting "age group average for stunting"
la var age_stunting "age chunk average for stunting"

* religion
clonevar religion = v130
recode religion (4 5 7 96 = 4)
lab def rel 1 "catholic" 2 "protestant" 3 "adventist" 4 "other"
la values religion rel

* health outcomes
g byte diarrhea = (h11 == 2)

* Birth order and breastfeeding
clonevar precedBI 	= b11
clonevar succeedBI 	= b12
clonevar birthOrder = bord
clonevar dob 		= b3
clonevar ageFirstBirth = v212
clonevar bfDuration	= m4
clonevar bfMonths	= m5
clonevar breastfeeding = v404

*Place of delivery
g byte birthAtHome = inlist(m15, 11, 12)
recode h33 (0 8 = 0 "No")(1 2 3 = 1 "Yes"), gen(vitaminA)

recode s579 (0 8 = 0 "no")(1 = 1 "yes"), gen(childSick)
clonevar deliveryPlace = m15

clonevar birthWgt = m19
replace birthWgt = . if inlist(birthWgt, 9996, 9998, 9800)
replace birthWgt = birthWgt / 1000

clonevar birthWgtSource = m19a

* Keep elibigle children
g eligChild = 0
replace eligChild = 1 if (hw70 < 9996 & hw71 < 9996 & hw72 < 9996)
g eligChild2 = 0
replace eligChild2 =1 if (hw5 < 9996 & hw6 < 9996 & hw7 < 9996)

* How many children per household?
bys caseid: g numChild = _N if eligChild == 1

* Mother's bmi
g bmitmp = (v445/100)
egen motherBMI = cut(bmitmp), at(0, 18.5, 25.0, 50) label
la def bmi 0 "undernourished" 1 "normal" 2 "overweight"
la val motherBMI bmi

clonevar motherBWeight = v440 
replace motherBWeight = (motherBWeight / 100)

clonevar wantedChild = v367
recode h43 (0 8 = 0 "No")(1 = 1 "Yes"), gen(intParasites)

* Mother's education
clonevar motherEd = v106
clonevar motherEdYears = v107

*************************
*** Dietary Diversity ***
*************************

d v41*
/* NOTES: The recall is only for 24 hours so not sure how reliable
		  the metric is. Will calculate but may be misleading. WDDS calculation
The categories are: 1. Starchy staples (WDDS_starch) 
                    2. Dark green leafy vegetables (WDDS_veg_green) 
                    3. Other Vitamin A rich fruit and veg (WDDS_vitA)
                    4. Other fruit and veg (WDDS_veg_other)
                    5. Organ meat (WDDS_organ)
                    6. Meat and fish (WDDS_meat_fish)
                    7. Eggs (WDDS_eggs)
                    8. Legumes, nuts, and seeds (WDDS_legumes)  
                    9. Milk and milk products (WDDS_dairy)
*/

* Starch <- v414f, v414e
g byte starch = inlist(1, v414f, v414e) if !missing(v414f) | !missing(v414e)

* Dark green veggies <- v414j
g byte vegGreen = inlist(1, v414j) if !missing(v414j)

* Vitamin A fruit and veg
g byte vitA	= inlist(1, v414k, v414i) if !missing(v414k) | !missing(v414i)

* other fruit and veg
g byte othFruit = inlist(1, v414l) if !missing(v414l)

* Organ meat
g byte organ = inlist(1, v414m) if !missing(v414m)

* fish and meat
g byte meat = inlist(1, v414n, v414h) if !missing(v414n) | !missing(v414h)

* eggs
g byte eggs = inlist(1, v414g) if !missing(v414g)

* Legumes, nuts and seeds
g byte legumes = inlist(1, v414o) if !missing(v414o)

* Milk and related
g byte milk = inlist(1, v414p, v411, v414v) if !missing(v414p) | !missing(v411) | !missing(v414v)

sum starch - milk

* Create dietary diversity
egen dietdiv = rowtotal(starch vegGreen vitA othFruit organ meat eggs legumes milk)

* Keep subset of variables
#delimit ;
ds(stunting stunting2 stunted stunted2 ageChild 
	age_group female ageg_stunting age_stunting 
	religion diarrhea precedBI succeedBI 
	birthOrder dob ageFirstBirth bfDuration 
	bfMonths childSick deliveryPlace birthWgt 
	birthWgtSource v001 v002 eligChild
	ageMonGroup starch vegGreen vitA 
	othFruit organ meat eggs legumes milk
	dietdiv bmitmp motherBMI motherBWeight 
	motherEd breastfeeding birthAtHome
	motherEdYears DHSCLUST cweight wantedChild
	vitaminA intParasites;
#delimit cr
keep `r(varlist)'

compress
saveold "$pathout/DHS_child.dta", replace

* Merge in household information and livelihood information
merge m:1 v001 v002 using "$pathout/DHS_hhvar.dta"
ren DHSCLUST, lower

merge m:1 dhsclust using "$pathout/RWA_DHS_Livelihoods.dta", gen(_dhs_FEWS)

* Label the cmc codes (di 12*(2014 - 1900)+1)
la def cmc 1378 "Oct. 2014" 1379 "Nov. 2014" 1380 "Dec. 2014" 1381 "Jan. 2015" /*
*/ 1382 "Feb. 2015" 1383 "Mar. 2015" 1384 "Apr. 2015"
la val intdate cmc

* Survey set the data to account for complex sampling design
svyset psu [pw = cweight], strata(strata)

twoway (kdensity stunting2), xline(-2, lwidth(thin) /*
*/ lpattern(dash) lcolor("199 199 199")) by(lvdzone)

* Check stunting over standard covariates
svy:mean stunting2, over(district)
svy:mean stunted2, over(district)
matrix smean = r(table)

* Create locals for reference lines in coefplot
local stuntmean = smean[1,1]
local lb = smean[5, 1]
local ub = smean[6, 1]

matrix plot = r(table)'
matsort plot 1 "down"
matrix plot = plot'
coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) xline(`stuntmean' `lb' `ub')

* Create a table for export
matrix district = e(_N)'
matrix stunt = smean'
matrix gis = district, stunt
mat2txt, matrix(gis) saving("$pathxls/district_stunting.csv") replace
matrix drop _all

* running a few other statistics
svy:mean stunted2, over(female)
svy:mean stunted2, over(wealthGroup)
svy:mean stunted2, over(motherBMI female)
svy:mean stunted2, over(religion)
svy:mean stunted2, over(diarrhea)


preserve
collapse (mean) stunted2 (count) n = stunted2, by(lvdzone)
ren lvdzone LZNAMEE
export delimited "$pathxls/Stunting_livelihoodzones.csv", replace
restore

preserve
keep if eligChild == 1
keep v001 v002 stunted2 stunting2 latnum longnum urban_rura lznum lznamef lvdzone alt_gps dhsclust ageChild religion
export delimited "$pathxls\RWA_2014_DHS_stunting.csv", replace
restore

* Consider stunting over the livelihood zones.
mean stunted2, over(lvdzone)
cap matrix drop plot smean
matrix smean = r(table)
local stuntmean = smean[1,1]
local lb = smean[5, 1]
local ub = smean[6, 1]
matrix plot = r(table)'
matsort plot 1 "down"
matrix plot = plot'
coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) xline(`stuntmean' `lb' `ub')

set matsize 1000
pwmean stunting2, over(district) pveffects mcompare(tukey)
pwmean stunted2, over(district) pveffects mcompare(tukey)

* calculate moving average
preserve
collapse (sum) stunted2 (count) stuntN = stunted2, by(ageChild female)
drop if ageChild == .
sort female ageChild
xtset  female ageChild

bys female: g smoothStunt = (l2.stunted2 + l1.stunted2 + stunted2 + f1.stunted2 + f2.stunted2)/ /*
*/		(l2.stuntN + l1.stuntN + stuntN + f1.stuntN + f2.stuntN) 

tssmooth ma stuntedMA = (stunted2/stuntN), window(2 1 2)
xtline(stuntedMA smoothStunt)
restore
bob

* Appears to be a weak negative relationship w/ altitude
twoway(scatter stunting2 altitude)(lpolyci stunting2 altitude)

* How does stunting look by cluster?
twoway(scatter stunting dhsclust)(scatter clust_stunting dhsclust) 

* How does stunting against age cohorts
twoway(lpolyci stunting age)(scatter age_stunting age), by(female)
twoway(scatter stunting wealth)(lpolyci stunting wealth), by(diarrhea)

twoway(scatter stunting_bin age)(lpoly stunting_bin age if female == 1)/*
*/ (lpoly stunting_bin age if female == 0), by(province) 

* Geography?
twoway(scatter stunting alt_clust)(lpolyci stunting alt_clust), by(province rural)

export delimited "$pathout/stuntingAnalysis.csv", replace
saveold "$pathout/stuntingAnalysis.dta", replace

* Stunting regression analysis using various models; 
* First, try replicated the model Nada created in R
global hhchar "female ib(1).religion ageChild c.ageChild#c.ageChild birthOrder birthWgt ageFirstBirth motherBWeight ib(1).motherBMI agehead birthAtHome hhsize"
global assets "roomPC mobile landless bankAcount"
global health "diarrhea bednet toiletShare"
global livestock "cowtrad horse goat sheep chicken pig rabbit cowmilk cowbull"
global geog "altitude ib(37).district rural"
global geog2 "altitude ib(6).lvdzone rural"
global cluster "cluster(dhsclust)"


reg stunting2 $hhchar, $cluster
reg stunting2 $hhchar $assets $health, $cluster

