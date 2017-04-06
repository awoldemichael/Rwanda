# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_runAll.R: wrapper to run all the associated pkgs
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

setwd('~/GitHub/Rwanda/R/')
exportDir = '~/Documents/USAID/Rwanda/processeddata/'  

# SETUP -------------------------------------------------------------------


# Load setup functions / vars ---------------------------------------------
source('RWA_WFP_00_setup.R')


# IMPORT ------------------------------------------------------------------


# Import / clean individual children-level data --------------------------------------------
source('RWA_WFP_01_importKids.R')

# Import / clean individual women's data --------------------------------------------
source('RWA_WFP_02_importMother.R')

# Import / clean hh-level data --------------------------------------------
source('RWA_WFP_03_importHH.R')


# Import shapefiles for choropleths ---------------------------------------
source('RWA_WFP_05_importGeo.R')


# ANALYSE -----------------------------------------------------------------

# Calculate point estimates for stunting
source('RWA_WFP_A01_calcStunting.R')

# Run stunting linear models

# PLOT --------------------------------------------------------------------


# REMOVE GUNK -------------------------------------------------------------
rm(x, ch_test, cutoff, has_MHN_KEY, RWA_lakes)


# export ------------------------------------------------------------------
write.csv(hh, paste0(exportDir, 'RWA_CFSVA_2015_hh.csv'))

write.csv(ch_hh, paste0(exportDir, 'RWA_CFSVA_2015_child.csv'))
