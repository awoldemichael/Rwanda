# Rwanda stunting analysis -----------------------------------------
#
# RWA_WFP_A01_calcStunting.R
#
# Script to calculate point estimate data for stunting data for Rwanda from the CFSVA dataset
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


# Calculate estimates at the Admin2 level ---------------------------------

stunting_admin2 = calcPtEst(ch, 'isStunted', by_var = 'admin2',
                            psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

# Calculate estimates for livelihood zones ---------------------------------

stunting_lz = calcPtEst(ch, 'isStunted', by_var = 'livelihood_zone',
                            psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')
