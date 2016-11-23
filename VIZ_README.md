# Outputs and code location for files.

## Naming conventions
* WFP are files that mainly use the World Food Programme's Comprehensive Food Security and Vulnerability Analysis
* DHS are files that mainly use the Demographic & Health Surveys
* CHAIN are files related to Rwanda's Community Health and Integrated Nutrition project
* Files marked "A" are more anaytical files
* Files mared "P" are files that have production-level plots
* All other files are data cleanup/wrangling files.

## Data Prep Files:
1. `R\RWA_WFP_runAll.R`: wrapper for files 00-05
* "RWA_WFP_00_setup.R": libraries + global vars
* "RWA_WFP_01_importKids.R": import/clean 2015 CFSVA children's data             
* "RWA_WFP_02_importMother.R": import/clean 2015 CFSVA women's data          
* "RWA_WFP_03_importHH.R": import/clean 2015 CFSVA household data               
* "RWA_WFP_03b_uniqueHHid.R": helper file to find a unique id       
* "RWA_WFP_04_importKids2012.R": import/clean 2012 CFSVA children's data          
* "RWA_WFP_06_importMoms2012.R": import/clean 2012 CFSVA women's data          
* "RWA_WFP_07_importHH2012.R": import/clean 2012 CFSVA household data 

## Analysis Files:
(or files that calculate averages)
"RWA_WFP_A01_calcStunting.R"          "RWA_WFP_A02_stuntingModels.R"       
"RWA_WFP_A03_stuntingModelsMusings.R" "RWA_WFP_A04_calcGrowing.R"          
"RWA_WFP_A05_FCSModels.R"             "RWA_WFP_A06_stunting2012Models.R"   
"RWA_WFP_A07_wavecomparison.R"        "RWA_WFP_A08_PCA.R"   
                       
"DHSvariable_overlap.R"              
"RWA_CHAIN_interventioncorr.R"        "RWA_clustering.R"                   
"RWA_compareCHAIN.R"                  "RWA_DHS_importInterpRaster.R"       
"RWA_DHS_P06_stuntingSpark.R"         
         
               
"RWA_WFP_P01_diets.R"                 "RWA_WFP_P02_stuntingCHAIN.R"        
"RWA_WFP_P03_plotIndivDist.R"         "RWA_WFP_P04_stuntingChoros.R"       
"RWA_WFP_P05_testStuntingDistrib.R"   "RWA_WFP_P06_stuntingChg.R"          
"RWA_WFP_README.md"                   "RWA_WFP_runAll.R"  

###`RWA_WFP_P01_.R`