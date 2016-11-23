# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_run2012.R: wrapper to run all the associated files for the 2012 data.
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


# SETUP -------------------------------------------------------------------


# Load setup functions / vars ---------------------------------------------
source('RWA_WFP_00_setup.R')


# Import data -------------------------------------------------------------

source('RWA_WFP_07_importHH2012.R')


# Analyse data ------------------------------------------------------------


