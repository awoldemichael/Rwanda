/*-------------------------------------------------------------------------------
# Name:		03a_StuntingAnalysis
# Purpose:	Plot data and run stunting anlaysis models
# Author:	Tim Essam, Ph.D.
# Created:	2016/08/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/
clear
capture log close
log using "$pathlog/03a_StuntingAnalysis", replace
use "$pathout/DHS_2015_analysis.dta", clear

* Label the cmc codes (di 12*(2014 - 1900)+1)
recode intdate (1371 1369 = 1378)
	la def cmc 1378 "Oct. 2014" 1379 "Nov. 2014" 1380 "Dec. 2014" 1381 "Jan. 2015" /*
	*/ 1382 "Feb. 2015" 1383 "Mar. 2015" 1384 "Apr. 2015"
	la val intdate cmc

* What does the within cluster distribution of stunting scores look like?
egen clust_stunt = mean(stunting2), by(strata)
egen alt_stunt = mean(stunting2), by(altitude)
g dist_HealthFac = (dist_nearest_HealthFac / 1000)
la var dist_HealthFac "Geodesic distance between cluster offset and nearest facility"

* Basic plots
twoway(scatter clust_stunt strata)
twoway(scatter stunting2 strata)

* Summary of z-scores by altitudes
twoway (scatter alt_stunt altitude, sort mcolor("192 192 192") msize(medsmall) /*
	*/ msymbol(circle) mlcolor("128 128 128") mlwidth(thin)) (lpolyci alt_stunt /*
	*/ altitude [aweight = cweight] if inrange(altitude, 1198, 2508), clcolor("99 99 99") clwidth(medthin)), /*
	*/ ytitle(Stunting Z-score) ytitle(, size(small) color("128 128 128")) /*
	*/ xtitle(, size(small) color("128 128 128")) title(Stunting outcomes appear /*
	*/ to worsen with increases in elevation., size(small) color("99 99 99") /*
	*/ span justification(left))

* Summary of stunting by wealth
twoway (scatter stunting2 wealth, sort mcolor("192 192 192") msize(medsmall)/* 
	*/msymbol(circle) mlcolor("128 128 128") mlwidth(thin)) (lpolyci stunting2 /*
	*/wealth [aweight = cweight], clcolor("99 99 99") clwidth(medthin)), /*
	*/ytitle(Stunting Z-score) ytitle(, size(small) color("128 128 128")) /* 
	*/yline(-2, lwidth(medium) lcolor("99 99 99")) xtitle(, size(small) /*
	*/color("128 128 128")) xline(0, lwidth(medium) lcolor("99 99 99")) /*
	*/title(Stunting outcomes appear to positively correlate with /*
	*/elevation., size(small) color("99 99 99") span justification(left)) /*
	*/ysca(alt) xsca(alt) xlabel(, grid gmax) legend(off) saving(main, replace)

	twoway histogram stunting2, fraction xsca(alt reverse) ylabel(, grid gmax) horiz fxsize(25)  saving(hy, replace)
	twoway histogram wealth, fraction ysca(alt reverse) ylabel(, nogrid)/*
	*/ fysize(25) xlabel(, grid gmax) saving(hx, replace)
* Combine graphs together to put histograms on x/y axis
	graph combine hy.gph main.gph hx.gph, hole(3) imargin(0 0 0 0) 

* Survey set the data to account for complex sampling design
	svyset psu [pw = cweight], strata(strata)

	twoway (kdensity stunting2), xline(-2, lwidth(thin) /*
	*/ lpattern(dash) lcolor("199 199 199")) by(lvdzone)

* Show the distribituion of education on z-scores
	twoway (kdensity stunting2 if motherEd ==0)(kdensity stunting2 if motherEd ==1) /*
	*/ (kdensity stunting2 if motherEd ==2)(kdensity stunting2 if motherEd ==3) /*
	*/ , xline(-2, lwidth(thin) lpattern(dash) lcolor("199 199 199"))

* Check stunting over standard covariates
svy:mean stunting2, over(district)
svy:mean stunted2, over(district)
matrix smean = r(table)
matrix district = smean'
mat2txt, matrix(district) saving("$pathxls/stunting_dist") replace

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

* Check stunting over livelihood zones
svy:mean stunting2, over(lvdzone)
	svy:mean stunted2, over(lvdzone)
	matrix smean = r(table)
	matrix lvdzone = smean'
mat2txt, matrix(lvdzone) saving("$pathxls/stunting_lvd") replace

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
pwmean stunting2, over(lvdzone) pveffects mcompare(tukey)
pwmean stunted2, over(district) pveffects mcompare(tukey)

* calculate moving average
preserve
	collapse (sum) stunted2 (count) stuntN = stunted2, by(ageChild female)
	drop if ageChild == . | ageChild<6
	sort female ageChild
	xtset  female ageChild

	bys female: g smoothStunt = (l2.stunted2 + l1.stunted2 + stunted2 + f1.stunted2 + f2.stunted2)/ /*
	*/		(l2.stuntN + l1.stuntN + stuntN + f1.stuntN + f2.stuntN) 

	tssmooth ma stuntedMA = (stunted2/stuntN), window(2 1 2)
	xtline(stuntedMA smoothStunt)
restore


export delimited "$pathout/stuntingAnalysis.csv", replace
saveold "$pathout/stuntingAnalysis.dta", replace

* Stunting regression analysis using various models; 
	g agechildsq = ageChild^2
	la var rural "rural household" 
	g altitude2 = altitude/1000
	la var altitude2 "altitude divided by 1000"
	egen hhgroup = group(v001 v002) if eligChild == 1
save "$pathout/DHS_2015_stunting.dta", replace

* Create groups for covariates as they map into conceptual framework for stunting
	global matchar "motherBWeight motherBMI motherEd femhead orsKnowledge"
	global hhchar "wealth improvedSanit improvedWater bnetITNuse landless"
	global hhchar2 "mobile bankAcount improvedSanit improvedWater bnetITNuse"
	global hhag "tlutotal"
	global hhag2 "cowtrad goat sheep chicken pig rabbit cowmilk cowbull"
	global demog "hhsize agehead hhchildUnd5"
	global chldchar "ageChild agechildsq birthOrder birthWgt"
	global chealth "intParasites vitaminA diarrhea anemia"
	global geog "altitude2 rural dist_HealthFac"
	global geog2 "altitude2 ib(1).lvdzone dist_HealthFac"
	global cluster "cluster(dhsclust)"
	global cluster2 "cluster(hhgroup)"

* STOP: Check all globals for missing values!
sum $matchar $hhchar $hhag $demog female $chldchar $chealth

* Be continuous versus binary
est clear
eststo sted1_0: reg stunting2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog ib(1381).intdate, $cluster 
eststo sted1_1: reg stunting2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog2 ib(1381).intdate, $cluster 
eststo sted2_3: logit stunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog ib(1381).intdate, $cluster or 
eststo sted2_4: logit stunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog2 ib(1381).intdate, $cluster or
eststo sted2_5: logit extstunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog ib(1381).intdate, $cluster or 
eststo sted2_6: logit extstunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog2 ib(1381).intdate, $cluster or 
esttab sted*, se star(* 0.10 ** 0.05 *** 0.01) label ar2 pr2 beta not /*eform(0 0 1 1 1)*/ compress
* export results to .csv
esttab sted* using "$pathout/`x'Wide.csv", wide mlabels(none) ar2 beta label replace not

* by gender
est clear
eststo sted2_1, title("Stunted 1"): reg stunting2 $matchar $hhchar $hhag $demog $chldchar $chealth $geog2 ib(1381).intdate if female == 1, $cluster 
eststo sted2_2, title("Stunted 2"): reg stunting2 $matchar $hhchar $hhag $demog $chldchar $chealth $geog2 ib(1381).intdate if female == 0, $cluster2 

* Regional variations
est clear
local i = 0
levelsof adm1name, local(levels)
foreach x of local levels {
	local name =  strtoname("`x'")
	eststo stunt_`name', title("Stunted `x'"): reg stunting2 $matchar $hhchar /*
	*/ $hhag $demog female $chldchar $chealth $geog if adm1name == "`x'", $cluster 
	local i = `++i'
	}
*
esttab stunt_*, se star(* 0.10 ** 0.05 *** 0.01) label ar2 beta
coefplot stunt_East || stunt_North || stunt_South || stunt_West, drop(_cons ) /*
*/ xline(0) /*mlabel format(%9.2f) mlabposition(11) mlabgap(*2)*/ byopts(row(1)) 



*** --- 2014 Subset analysis for those with dietary diversity recall variables ***

clear
	use "$pathout/RWA_DHS_2014_under24mo_analysis.dta", replace
	tab lvdzone, mi
	
	* Stunting regression analysis using various models; 
	g agechildsq = ageChild^2
	la var rural "rural household" 
	g altitude2 = altitude/1000
	la var altitude2 "altitude divided by 1000"

* Create groups for covariates as they map into conceptual framework for stunting
	global matchar "motherBWeight motherBMI motherEd femhead orsKnowledge"
	global hhchar "wealth improvedSanit improvedWater bnetITNuse landless"
	global hhchar2 "mobile bankAcount improvedSanit improvedWater bnetITNuse"
	global hhag "tlutotal"
	global hhag2 "cowtrad goat sheep chicken pig rabbit cowmilk cowbull"
	global demog "hhsize agehead hhchildUnd5"
	global chldchar "ageChild agechildsq birthOrder birthWgt"
	global chealth "intParasites vitaminA diarrhea anemia"
	global geog "altitude2 rural"
	global geog2 "altitude2 ib(1).lvdzone "
	global cluster "cluster(dhsclust)"
	global cluster2 "cluster(hhgroup)"


sum stunting2 stunted2 extstunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth

* Be continuous versus binary
est clear
	eststo sted1_0: reg stunting2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog ib(1381).intdate, $cluster 
	eststo sted1_1: reg stunting2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog2 ib(1381).intdate, $cluster 
	eststo sted2_3: logit stunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog ib(1381).intdate, $cluster or 
	eststo sted2_4: logit stunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog2 ib(1381).intdate, $cluster or
	eststo sted2_5: logit extstunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog ib(1381).intdate, $cluster or 
	eststo sted2_6: logit extstunted2 dietdiv $matchar $hhchar $hhag $demog female $chldchar $chealth $geog2 ib(1381).intdate, $cluster or 
	esttab sted*, se star(* 0.10 ** 0.05 *** 0.01) label ar2 pr2 beta not /*eform(0 0 1 1 1)*/ compress
	* export results to .csv
esttab sted* using "$pathout/`x'Wide2014_under24.csv", wide mlabels(none) ar2 pr2 beta label replace not



