# Outputs and code location for files.

## Naming conventions
* WFP are files that mainly use the World Food Programme's Comprehensive Food Security and Vulnerability Analysis
* DHS are files that mainly use the Demographic & Health Surveys
* CHAIN are files related to Rwanda's Community Health and Integrated Nutrition project
* Files marked "A" are more anaytical files
* Files mared "P" are files that have production-level plots
* All other files are data cleanup/wrangling files.

## Dependencies:
1. `R\RWA_WFP_runAll.R`
* 


                       
 [9] "attributes.R"                        "DHSvariable_overlap.R"              
[11] "RWA_CHAIN_interventioncorr.R"        "RWA_clustering.R"                   
[13] "RWA_compareCHAIN.R"                  "RWA_DHS_importInterpRaster.R"       
[15] "RWA_DHS_P06_stuntingSpark.R"         "RWA_WFP_00_setup.R"                 
[17] "RWA_WFP_01_importKids.R"             "RWA_WFP_02_importMother.R"          
[19] "RWA_WFP_03_importHH.R"               "RWA_WFP_03b_uniqueHHid.R"           
[21] "RWA_WFP_04_importKids2012.R"         "RWA_WFP_05_importGeo.R"             
[23] "RWA_WFP_06_importMoms2012.R"         "RWA_WFP_07_importHH2012.R"          
[25] "RWA_WFP_A01_calcStunting.R"          "RWA_WFP_A02_stuntingModels.R"       
[27] "RWA_WFP_A03_stuntingModelsMusings.R" "RWA_WFP_A04_calcGrowing.R"          
[29] "RWA_WFP_A05_FCSModels.R"             "RWA_WFP_A06_stunting2012Models.R"   
[31] "RWA_WFP_A07_wavecomparison.R"        "RWA_WFP_A08_PCA.R"                  
[33] "RWA_WFP_P01_diets.R"                 "RWA_WFP_P02_stuntingCHAIN.R"        
[35] "RWA_WFP_P03_plotIndivDist.R"         "RWA_WFP_P04_stuntingChoros.R"       
[37] "RWA_WFP_P05_testStuntingDistrib.R"   "RWA_WFP_P06_stuntingChg.R"          
[39] "RWA_WFP_README.md"                   "RWA_WFP_runAll.R"  

###`RWA_WFP_P01_.R`