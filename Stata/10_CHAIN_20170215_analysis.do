* CHAIN Data processing
* Tasks: create unique ID, create sector counts, flags
* Author: Tim Essam, USDA
* Date: 2017/02/27

clear
capture log close


* Load data from Baboyma's email as of 2017/02/15
* subject: 	Re: URGENT - MAPS needed for Minister meeting on Thursday
import excel "C:\Users\Tim\Documents\Rwanda\GIS\CHAIN_data_20170215.xls", sheet("data") firstrow

  * Create unique ids
	egen uniqueID = group(provid distid sectid partner intervention )
	isid uniqueID

  * Test
	bys provid distid sectid partner intervention: gen test = _n
	tab test, mi
	
  * Create sector counts for nutrition Tech Area programs
	egen sect_Nutrition = count(techarea) if techarea == "Nutrition-Specific", by(sectid distid provid)
	
  * Create flags to filter nutrition sector counts (for ArcMap)
   g byte nutr_flag = (sect_Nutrition!=.)
   
  * Create family planning related counts
   g byte famplan_flag = regexm(intervention, /*
   */ "(Counselling for Family Planning|Health Education|Promotion of Health Products|Sexual and Reproductive Health and Right)") == 1
   
  * Create sector counts for the family planning interventions (allowing for overlap if techarea varies)
	egen sect_famplan = count(intervention) if famplan_flag == 1, by(sectid distid provid)

  * Label some variables
  la var sect_Nutrition "count of nutrition techarea activities by sector"
  la var nutr_flag "filter for nutrition techarea"
  la var famplan_flag "filter for family planning interventions"
  la var sect_famplan "count of family planning interventions by sector"
  order uniqueID
  
  * Export a cut of data
  export delimited using "C:\Users\Tim\Documents\Rwanda\GIS\CHAIN\CHAIN_data_20170215_processed.csv", replace

 
