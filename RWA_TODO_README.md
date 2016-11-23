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
- [ ] choropleth: stunting by district, livelihood zone
- [ ] reproducible choropleth function (frontier)
- [ ] regressions: stunting
    - [ ] cluster standard errors
    - [ ] recenter/scale to standardize coefficients
- [ ] regressions: FCS/dd
- [ ] wealth by occupation / livelihood zone (over time)
- [ ] how has stunting changed over time, where?
- [ ] why has stunting changed over time?
- [ ] heatmap of fcs (with distribution plots-- day/food grp, FCS hist?) over dist, lz
- [ ] heatmap of dd over dist, lz
- [ ] FCS hist
- [ ] choropleth FCS/dd?
- [ ] reproducible heatmap function (llamar)
- [ ] pull individual diets (dd, MAD) for children 6 mo. - 2 y and add to models / compare dd over hh
- [ ] add in hh composition data (breakdown of M/F, ages)
- [ ] check food expend == per capita
- [ ] quantile per capita expenditures
- [ ] build up asset indices (? -- not enough raw data?)
- [ ] FAO price indices: are changes due to external market volatility?
- [ ] food groups by (FCS, wealth, % total expenditures on food) plot
- [ ] group good/bad coping, if going to use
- [ ] compare w/ district-level data on projects
- [ ] fix themes of llamar wrt fonts

### Done:

#### Data prep
- [x] clean children's module
- [x] clean women's module
- [x] clean hh module
- [x] merge kids + women
- [x] merge hh + kids
- [x] figure out survey weights for CFSVA

#### Reproducible code
- [x] calc point estimate function (llamar)
- [x] factorize (llamar)

#### Vis. products

