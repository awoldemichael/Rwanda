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
  

# Load setup functions / vars ---------------------------------------------
source('RW_WFP_00_setup.R')

# Import / clean hh-level data --------------------------------------------
source('RW_WFP_01_importHH.R')

# Import / clean individual children-level data --------------------------------------------
source('RW_WFP_02_importKids.R')
