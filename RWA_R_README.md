# Outputs and code location for Rwanda stunting analysis.
Laura Hughes, lhughes@usaid.gov

## Naming conventions
* WFP are files that mainly use the World Food Programme's Comprehensive Food Security and Vulnerability Analysis
* DHS are files that mainly use the Demographic & Health Surveys
* CHAIN are files related to Rwanda's Community Health and Integrated Nutrition project
* Files marked "A" are more anaytical files
* Files mared "P" are files that have production-level plots
* All other files are data cleanup/wrangling files.

---

## Data Prep Files:
1. `R\RWA_WFP_runAll.R`: wrapper for files 00-05
* `RWA_WFP_00_setup.R`: libraries + global vars
* `RWA_WFP_01_importKids.R`: import/clean 2015 CFSVA children's data             
* `RWA_WFP_02_importMother.R`: import/clean 2015 CFSVA women's data          
* `RWA_WFP_03_importHH.R`: import/clean 2015 CFSVA household data               
* `RWA_WFP_03b_uniqueHHid.R`: helper file to find a unique id       
* `RWA_WFP_04_importKids2012.R`: import/clean 2012 CFSVA children's data          
* `RWA_WFP_06_importMoms2012.R`: import/clean 2012 CFSVA women's data          
* `RWA_WFP_07_importHH2012.R`: import/clean 2012 CFSVA household data 

## Analysis Files:
(or files that calculate averages)

1. `RWA_WFP_A01_calcStunting.R`          
* `RWA_WFP_A02_stuntingModels.R`       
* `RWA_WFP_A03_stuntingModelsMusings.R` 
* `RWA_WFP_A04_calcGrowing.R`          
* `RWA_WFP_A05_FCSModels.R`             
* `RWA_WFP_A06_stunting2012Models.R`   
* `RWA_WFP_A07_wavecomparison.R`        
* `RWA_WFP_A08_PCA.R`   
                       
"DHSvariable_overlap.R"              
"RWA_CHAIN_interventioncorr.R"        "RWA_clustering.R"                   
"RWA_compareCHAIN.R"                  "RWA_DHS_importInterpRaster.R"       
        
         
## Plotting files:               

1. `RWA_WFP_P01_diets.R`                 

* `RWA_WFP_P02_stuntingCHAIN.R` 

* `RWA_WFP_P03_plotIndivDist.R`         

* `RWA_WFP_P04_stuntingChoros.R`       

* `RWA_WFP_P05_testStuntingDistrib.R`   
![stuntingViolin](/exported_img/violin.png)

* `RWA_WFP_P06_stuntingChg.R`: 

* `RWA_DHS_P07_stuntingSpark.R`: Changes in DHS over time
![stuntingSpark](/exported_img/DHS_stunting_spark.png)


---

## Modifications for polished products

#### basemap
* supah fade: 25 pt. white + 35 pt gaussian blur
* terrain: 60% transparency
* outside borders: 50% transparent white fill
* rivers: 1 pt.
* shadow Adm0: 75% @ 100K, 0.05" 0.05" 0.04"

#### CHAIN/stunting overlay
* Label CHAIN project
* Label 'total'
* Label nat'l avg.
* round square edges 0.02"
* drop shadow squares & total circles multiply 75 @ 90% K, 0.02" 0.02" 0.02"
* Adjust CHAIN project labels
* decrease/add stroke squares, 90% K, 0.15 pt.
* add stroke circles, 90% K, 0.1 pt
* duplicate horizontal axis guides and surround text
* change % labels to white where appropriate
* add maps
* move total over, duplicate district names

#### Interpolation histogram for stunting
* remove all clipping masks
* group raster slices together
* join Adm0 fragments together (cmd-j)
* clip to Adm0
* resize to 7.12" wide; left corner at 1.6828 in, 0.5821

#### Livelihood zones
* resize to 7.1386 in.


#### FCS heatmap extraordinaire
* Annoyingly, things won't perfectly align b/c the text changes the plottable area.  Therefore adding dummy text which has to be removed; removing random unnecessary text.
* remove clipping paths
* clip density functions to outer surface
* create blend of distribution surfaces
* chiclet-ize heatmaps: round corners, 0.035"
* individual maps: drop shadow multiply 75 @ 90% K, 0.02" 0.02" 0.02"
* individual maps: apply 0.1 pt. border + multiply 75% @ #6c191b, 0.02" 0.02" 0.02"
* remove other urban areas from Kigali City map
* Adjust the placements so equal horizontally
* Adjust the vertical position of the distributions (since they're slightly misaligned)
* add scale bars
