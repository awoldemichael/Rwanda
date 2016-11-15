# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_06_importHH2012.R: import household-level data from 2012 dataset
#
# Script to pull stunting data and associated household- or
# child-level data for Rwanda from the CFSVA dataset
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 14 November 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

# Import in the raw data --------------------------------------------------
# Raw data is contained in three files:
# children's data: 'cfsva-2015-child-DB- annex.sav'
# household-level data:  'cfsva-2015-master-DB- annex.sav'
# women's data: 'cfsva-2015-mother-DB- annex.sav'


ch2012_raw = read_sav(paste0(baseDir, 'RW_2012_CFSVA/cfsvans-2012- children-v01.sav'))


# 2009 data are unprocessed.  Also doesn't include Kigali / urban areas.
# ch2009 = read_sav(paste0(baseDir, 'RW_2009_CFSVA/Section 13 enfants.sav'))




# Checking how weights should be applied ----------------------------------



# Notes on data -----------------------------------------------------------

# stunting is based on 2006 WHO children's growth standards
# stunting was calculated by WFP in SPSS.
# Key stunting variables for 2015 data is Stunted_global (binary stunted) and HAZWHO (height-for-age z-score based on the WHO distribution)

# select variables --------------------------------------------------------
# Majority of household explanatory variables will be pulled from the household-level data.
ch2012 = ch2012_raw %>% 
 select(
  )


# clean vars --------------------------------------------------------------



# old stuff ---------------------------------------------------------------
cfsva2012 = svydesign(id = ~v_code, strata = ~d_code, weights = ~FINAL_PopWeight, data = ch2012)
svyby(~G_Stunted, design = cfsva2012, by = ~fews_code, svymean, na.rm = TRUE)
svyby(~G_Stunted, design = cfsva2012, by = ~d_code, svymean, na.rm = TRUE)
