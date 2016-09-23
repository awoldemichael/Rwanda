# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_05_importGeo.R: import household-level data
#
# Script to import shapefiles for making choropleths of CFSVA data
# 
# Administrative data from the Rwanda National Insitute of Statistics Rwanda
# Available at http://geodata.nisr.opendata.arcgis.com/datasets?q=Rwanda
#
# Livelihood zone polygons from FEWS NET
# Available at 
#
# Laura Hughes, lhughes@usaid.gov, 23 September 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

library(frontier)
library(maptools)

# setup file location -----------------------------------------------------
baseDir_geo = '~/Documents/USAID/Rwanda/geodata/'

# import Admin0 -----------------------------------------------------------
RWA_admin0 = shp2df(baseDir =  baseDir_geo,
                    folderName = 'Rwanda_Admin0',
                    layerName = 'National_Boundary_2001', 
                    exportData = FALSE)

# import Admin1 -----------------------------------------------------------
RWA_admin1 = shp2df(baseDir =  baseDir_geo,
                    folderName = 'Rwanda_Admin1',
                    layerName = 'Province_Boundary_2006',
                    exportData = FALSE)


# import Admin2 -----------------------------------------------------------
RWA_admin2 = shp2df(baseDir =  baseDir_geo,
                    folderName = 'Rwanda_Admin2',
                    layerName = 'District_Boundary_2006',
                    exportData = FALSE)

# import FEWS NET livelihood zones ----------------------------------------
RWA_LZ = shp2df(baseDir =  baseDir_geo,
                    folderName = 'RW_LivelihoodZones_FEWS_2012',
                    layerName = 'RW_LHZ_2012',
                    exportData = FALSE)


