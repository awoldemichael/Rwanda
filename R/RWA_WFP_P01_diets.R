# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_05_P_diets.R: import household-level data
#
# Script to create plots for the dietary diversity and FCS scores of households within the 2015 CFSVA
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


# load data ---------------------------------------------------------------
source('~/GitHub/Rwanda/R/RWA_WFP_runAll.R')



# NOTES -------------------------------------------------------------------
# * Includes all households, not just those with children.

# By livelihood zone
fcs_byLZ = fcs_heatmap(df = hh, region_var = 'lz_name', plot_map = TRUE, admin0 = RWA_admin0, region_coords = RWA_LZ$df,
                filename = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/FCS_CFSVA.pdf',
                width_indivPlots = c(0.075, 0.65, 0.2, 0.075),
                width = 8.5, height = 5.5)

# By district
fcs_byDist = fcs_heatmap(df = hh, region_var = 'admin2', map_region_var = 'District',
                plot_map = TRUE, admin0 = RWA_admin0, region_coords = RWA_admin2$df,
                filename = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/FCS_CFSVA_admin2.pdf',
                width_indivPlots = c(0.075, 0.65, 0.2, 0.075),
                width = 8.5, height = 5.5)

# dietary diversity -------------------------------------------------------

x = fcs_heatmap(df = hh, region_var = 'lz_name', FCS_var = 'HDDS_24h', use_FCSWts = FALSE,
                FCS_range = c(0, 13), poor_FCS = 4, borderline_FCS = 6,
                       plot_map = FALSE,
                       width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(x)

x = fcs_heatmap(df = hh, region_var = 'lz_name', FCS_var = 'DDS', use_FCSWts = FALSE,
                FCS_range = c(0, 8), poor_FCS = 1, borderline_FCS = 1,
                plot_map = FALSE,
                width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(x)

x = fcs_heatmap(df = hh, region_var = 'lz_name', FCS_var = 'GDDS', use_FCSWts = FALSE,
                FCS_range = c(0, 4), poor_FCS = 1, borderline_FCS = 2,
                plot_map = FALSE,
                width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(x)
