# Rwanda stunting analysis -----------------------------------------
Script to pull stunting data and associated household- or child-level data for Rwanda from the CFSVA dataset

Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis by the World Food Programme
* Available at http://microdata.statistics.gov.rw/index.php/catalog/70
* Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016

Laura Hughes, lhughes@usaid.gov, 14 September 2016
with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)

Copyright 2016 by Laura Hughes via MIT License

For the CFSVA and Nutrition Survey 2012, data collection was conducted during the lean season
(March-April) that followed a relatively good harvest. 

### To Do:
* clean hh module
* merge hh + kids
* choropleth: stunting by district, livelihood zone
* reproducible choropleth function (frontier)
* regressions: stunting
* regressions: FCS/dd
* wealth by occupation / livelihood zone (over time)
* how has stunting changed over time, where?
* why has stunting changed over time?
* heatmap of fcs (with distribution plots-- day/food grp, FCS hist?) over dist, lz
* heatmap of dd over dist, lz
* FCS hist
* choropleth FCS/dd?
* heatmap function (llamar)
* pull individual diets (dd, MAD) for children 6 mo. - 2 y and add to models / compare dd over hh
* add in hh composition data (breakdown of M/F, ages)
* check food expend == per capita
* build up asset indices
* FAO price indices: are changes due to external market volatility?
* food groups by wealth plot

### Done:

#### Data prep
* clean children's module
* clean women's module
* merge kids + women
* figure out survey weights for CFSVA

#### Reproducible code
* calc point estimate function (llamar)
* factorize (llamar)

#### Vis. products