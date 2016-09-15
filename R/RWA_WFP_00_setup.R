# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_00_setup.R: load packages and functions for analysis.
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


# Setup local directory ---------------------------------------------------

baseDir = '~/Documents/USAID/Rwanda/rawdata/RW_2015_CFSVA/'


# Define colors, vars -----------------------------------------------------
stunting_colour = 'YlOrRd'
stunting_range = c(0, 1)

# load appropriate packages -----------------------------------------------
library(dplyr)
library(haven)
library(ggplot2)
library(tidyr)
library(lubridate)
library(llamar)
