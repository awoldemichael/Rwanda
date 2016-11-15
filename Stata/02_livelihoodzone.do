/*-------------------------------------------------------------------------------
# Name:		02_livelihoodzone
# Purpose:	Import and recode livelihoods zone 
# Author:	Tim Essam, Ph.D.
# Created:	2016/08/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close

import delimited using "$pathout/RWA_DHS_Livelihoods.csv", clear
log using "$pathlog/04_livelihoodzone.txt", replace

encode lznamee, gen(lvhood_zone)

/* These DHS cluster offsets fall outside of livelihood zones or within
	National Park boundaries. We use the nearest livelihood zone (from FEWSNET)
	as the replacement value */
	
replace lvhood_zone = 9 if inlist(dhsclust, 418, 161, 296, 81, 171)
replace lvhood_zone = 3 if inlist(dhsclust, 101, 317)
replace lvhood_zone = 16 if inlist(dhsclust, 199, 347, 281)


* Recode zones to match those from FEWS NET
* https://github.com/tessam30/RwandaLAM/blob/master/Datain/LivelihoodZones_FEWS.csv
#delimit ;
recode lvhood_zone (15 = 0 "Urban Area")
				   (9 = 1 "Lake Kivu Coffee")
				   (16 = 2 "Western Congo-Nile Crest Tea")
				   (12 = 3 "Northwestern Volcanic Irish Potato")
				   (6 = 4 "Eastern Congo-Nile Highland Subsistence Farming")
				   (3 = 5 "Central Plateau Cassava and Coffee")
				   (11 = 6 "Northern Highland Beans and Wheat")
				   (4 = 7 "Central-Northern Highland Irish Potato, Beans and Vegetables")
				   (2 = 8 "Bugesera Cassava")
				   (7 = 9 "Eastern Plateau Mixed Agricultural")
				   (14 = 10 "Southeastern Plateau Banana")
				   (5 = 11 "Eastern Agropastoral")
				   (8 = 12 "Eastern Semi-Arid Agropastoral"),
				   gen(lvdzone);
#delimit cr
drop lvhood_zone				   
la var lvdzone "livelihood zones (from FEWSNET)"

merge 1:m dhsclust using "$pathout/DHS_hhvar.dta", gen(_lvd)
saveold "$pathout/RWA_DHS_Livelihoods.dta", replace

* Import 2010 data and perform similar jooin
import delimited using "$pathout/RWA_DHS2010_Livelihoods.csv", clear
encode lznamee, gen(lvhood_zone2010)

/* These DHS cluster offsets fall outside of livelihood zones or within
	National Park boundaries. We use the nearest livelihood zone (from FEWSNET)
	as the replacement value */
	
replace lvhood_zone2010 = 9 if inlist(dhsclust, 181, 117, 116)
replace lvhood_zone2010 = 15 if inlist(dhsclust, 386)

* Recode zones to match those from FEWS NET
#delimit ;
recode lvhood_zone2010 (14 = 0 "Urban Area")
				   (9 = 1 "Lake Kivu Coffee")
				   (15 = 2 "Western Congo-Nile Crest Tea")
				   (11 = 3 "Northwestern Volcanic Irish Potato")
				   (5 = 4 "Eastern Congo-Nile Highland Subsistence Farming")
				   (2 = 5 "Central Plateau Cassava and Coffee")
				   (10 = 6 "Northern Highland Beans and Wheat")
				   (3 = 7 "Central-Northern Highland Irish Potato, Beans and Vegetables")
				   (1 = 8 "Bugesera Cassava")
				   (6 = 9 "Eastern Plateau Mixed Agricultural")
				   (13 = 10 "Southeastern Plateau Banana")
				   (4 = 11 "Eastern Agropastoral")
				   (7 = 12 "Eastern Semi-Arid Agropastoral"),
				   gen(lvdzone);
#delimit cr
drop lvhood_zone2010				   
la var lvdzone "livelihood zones (from FEWSNET)"

merge 1:m dhsclust using "$pathout/DHS_hhvar2010.dta", gen(_lvd2010)

saveold "$pathout/RWA_DHS2010_Livelihoods.dta", replace

