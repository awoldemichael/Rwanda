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
egen ageg_stunting = mean(stunting2), by(age_group)
egen age_stunting = mean(stunting2), by(ageChild)
la var ageg_stunting "age group average for stunting"
la var age_stunting "age chunk average for stunting"

* religion
clonevar religion = v130
recode religion (4 5 7 96 = 4)
lab def rel 1 "catholic" 2 "protestant" 3 "adventist" 4 "other"
la values religion rel

* health outcomes
g byte diarrhea = (h11 == 2)
g byte orsKnowledge = inlist(v416, 1, 2)
la var orsKnowledge "used ORS or heard of it"

* Birth order and breastfeeding
clonevar precedBI 	= b11
clonevar succeedBI 	= b12
clonevar birthOrder = bord
clonevar dob 		= b3
clonevar ageFirstBirth = v212
clonevar bfDuration	= m4
clonevar bfMonths	= m5
clonevar breastfeeding = v404
clonevar anemia = v457

* Antenatal care visits (missing for about 25% of sample)
recode m14 (3 = 2 "2-3 visits") (4/11 = 3 "4+ ANC visit")(. = 5 "missing"), gen(anc)
clonevar anc_Visits = m14

* Contraception use practices
	g byte modernContra = v313 == 3
	la var modernContra "Use modern method of contraception (binary)"

* Birth size
recode m18 (1 = 5 "very above")(2 = 4 "above ave")(3 = 3 "ave")(4 = 2 "below ave")/*
*/(5 = 1 "very below")(8 = .), gen(birthSize)

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
replace v445 = . if v445 == 9999
g bmitmp = (v445/100)
egen motherBMI = cut(bmitmp), at(0, 18.5, 25.0, 50) label
la def bmi 0 "undernourished" 1 "normal" 2 "overweight"
la val motherBMI bmi

clonevar motherBWeight = v440 
replace motherBWeight = . if inlist(motherBWeight, 9998, 9999)
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
	g byte starch = inlist(1, v414f, v414e) if !missing(v414f, v414e)

	* Dark green veggies <- v414j
	g byte vegGreen = inlist(1, v414j) if !missing(v414j)

	* Vitamin A fruit and veg
	g byte vitA	= inlist(1, v414k, v414i) if !missing(v414k, v414i)

	* other fruit and veg
	g byte othFruit = inlist(1, v414l) if !missing(v414l)

	* Organ meat
	g byte organ = inlist(1, v414m) if !missing(v414m)

	* fish and meat
	g byte meat = inlist(1, v414n, v414h) if !missing(v414n, v414h)

	* eggs
	g byte eggs = inlist(1, v414g) if !missing(v414g)

	* Legumes, nuts and seeds
	g byte legumes = inlist(1, v414o) if !missing(v414o)

	* Milk and related
	g byte milk = inlist(1, v414p, v411, v414v) if !missing(v414p, v411, v414v)

	sum starch - milk

* Create dietary diversity
	gen dd_flag = missing(v414f, v414e, v414j, v414k, v414i, v414l, v414m,/*
	*/ v414n, v414h, v414g, v414o, v414p, v411, v414v)
	egen dietdiv = rowtotal(starch vegGreen vitA othFruit organ meat eggs legumes milk)
	replace dietdiv = . if dd_flag == 1

*** END DIETARY DIVERSITY ***
* -- Notes on filtering diet diversity children -- *
	/* The data on the foods given are only collected for the last child living 
		with the mother, so you need to select if the child is 
		1) alive, 
		2) living with the mother, and 
		3) was the last born child that is living with the mother.
	(http://userforum.dhsprogram.com/index.php?t=msg&goto=9824&S=Google
	 http://userforum.dhsprogram.com/index.php?t=msg&goto=9598 ) */
		
* Flag children who meet criteria for dietary diversity questions
* Last child in the last 2 years living with mother
	preserve
			
			* age in months
				gen age = v008-b3
			
			* drop if too old or not alive
				*g byte child_age_fltr  = (age < 24 & b5 == 1)
				keep if (age < 24 & b5 == 1)
				
			* recode age into groups
				recode age (0/1=1 "0-1")(2/3=2 "2-3")(4/5=3 "4-5")/*
				*/ (6/8=4 "6-8")(9/11=5 "9-11")(12/17=6 "12-17") /*
				*/ (18/23=7 "18-23")(24/59=.), gen(child_age)
			
			* tab of all living children born in the last 2 years
				tab child_age
				tab child_age [iw=v005/1000000]

			* keep only those children living with mother ...
				*g byte live_with_moth_fltr = (b9 == 0)
				keep if b9 == 0
				
			* ... and keep the last born of those
				g byte lastBorn_fltr =  (_n > 0 & caseid == caseid[_n-1])
				drop if _n > 0 & caseid == caseid[_n-1] 
			
			* Flag those children who have dietary diversity data
				*g byte child_dietDiv = (child_age_fltr == 1 & /*
				**/live_with_moth_fltr ==1 & lastBorn_fltr) 
			
			tab dietdiv, mi
			
			* check the deonimnator
			tab child_age
			tab child_age [iw = cweight]

				#delimit ;
			ds(stunting stunting2 stunted stunted2 ageChild 
				age_group female ageg_stunting age_stunting 
				religion diarrhea precedBI succeedBI 
				birthOrder dob ageFirstBirth bfDuration 
				bfMonths deliveryPlace birthWgt 
				birthWgtSource v001 v002 eligChild
				ageMonGroup starch vegGreen vitA 
				othFruit organ meat eggs legumes milk
				dietdiv bmitmp motherBMI motherBWeight 
				motherEd breastfeeding birthAtHome eligChild2
				motherEdYears DHSCLUST cweight wantedChild anemia
				vitaminA intParasites extstunted* orsKnowledge 
				modernContra starch-child_age);
			#delimit cr
			keep `r(varlist)'

			compress
			saveold "$pathout/DHS_child_under24mo.dta", replace
			
			* merge in livelihood info
			use "$pathout/RWA_DHS_Livelihoods.dta", clear 
			merge 1:m v001 v002 using "$pathout/DHS_child_under24mo.dta", gen(_stunt) keep(match)
			
			g year = 2011
			save "$pathout/RWA_DHS_2014_under24mo_analysis.dta", replace
			
	restore







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
	motherEdYears DHSCLUST cweight wantedChild anemia
	vitaminA intParasites extstunted* orsKnowledge modernContra);
#delimit cr
keep `r(varlist)'

compress
saveold "$pathout/DHS_child.dta", replace

* Merge in household information and livelihood information
saveold "$pathout/stunting.dta", replace
use "$pathout/RWA_DHS_Livelihoods.dta", clear 

merge 1:m v001 v002 using "$pathout/stunting.dta", gen(_stunt)
*ren DHSCLUST, lower

g year = 2014
save "$pathout/DHS_2015_analysis.dta", replace
