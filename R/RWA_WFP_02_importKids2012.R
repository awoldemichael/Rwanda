# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_02_importKids2012.R: import children's data from 2012 dataset
#
# Script to pull stunting data and associated household- or
# child-level data for Rwanda from the CFSVA dataset
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 14 September 2016
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

# According to CFSVA final report, they used a two-stage survey design, with the districts as the primary cut and villages as the secondary:
# (from CFSVA 2015 detailed survey methodolgy)
# To facilitate comparison with existing studies, the CFSVA 2015 was designed to provide statistically representative
# and precise information at the district level. In addition, it was decided to include both urban and rural households
# and not to exclude the capital province Kigali. The sampling frame was organized according to the 30 districts.
# Subsequently, a two-stage cluster sample procedure was applied.
# In the first stage, 25 villages per district were randomly selected with probability to be selected proportional to the
# population size. In the second stage, ten households in each of the 25 villages in the 30 provinces were selected for
# participation in the survey. A systematic random sampling technique was chosen for this stage. The team leader,
# together with the village head, listed all households in the village. Based on this list, a systematic random sample
# was utilized to pick ten households to be interviewed and three reserve households should any of the first ten
# households be missing at the time of the interview or not agree to participate. Households were eligible for
# participation in the assessment if living in one of the selected villages at the time of the interviews.
# Thus, ten households, from 25 villages, from 30 provinces were chosen to participate in the survey, amounting up
# to 7,500 households.

# And from the NISR explanation of the data:
# "Taking into consideration the two-stage cluster sampling methodology described above, adjustment weights were
# computed to provide results representative at country level. The household probability of being selected in the
# sample is equal to the product of a household’s probability of being selected in a village by the probability of the
# village of being sampled. The inverse of this probability is the design weight. The design weight was adjusted for
# the expected and actual number of households in the surveyed villages and was used in the complex sample
# calculations. The design weight was divided by the product of the total number of households in the population
# divided by the number of sampled households. The resulting weight was used in all non-complex sample analyses."

# Based on this info, it seems like the primary strata = 30 districts (S0_D_Dist) and the enumeration areas are the villages (S0_G_Vill)


# Seems right on target w/ the strata being the 30 districts (first sampling division)
# Comparing final numbers from the CFSVA to the ones I calculated
# Point of difference: report says there are 4058 children measured, but the smaple only contains 3810.
# Guessing (?) difference is that there were 4058 eligible, but only 3810 measured / valid. 
# Raw children's file very clearly has 280 NAs, and numbers check out.


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
