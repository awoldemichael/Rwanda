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
# Available at http://www.fews.net/east-africa/rwanda/livelihood-zone-map/july-2012
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
                    getCentroids = FALSE,
                    reproject = TRUE,
                    projection = '+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                    exportData = FALSE)

# import Admin1 -----------------------------------------------------------
RWA_admin1 = shp2df(baseDir =  baseDir_geo,
                    folderName = 'Rwanda_Admin1',
                    layerName = 'Province_Boundary_2006',
                    reproject = TRUE,
                    projection = '+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                    exportData = FALSE)


# import Admin2 -----------------------------------------------------------
RWA_admin2 = shp2df(baseDir =  baseDir_geo,
                    folderName = 'Rwanda_Admin2',
                    layerName = 'District_Boundary_2006',
                    labelVar = 'District',
                    reproject = TRUE,
                    projection = '+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                    exportData = FALSE)

# import FEWS NET livelihood zones ----------------------------------------
RWA_LZ = shp2df(baseDir =  baseDir_geo,
                folderName = 'RW_LivelihoodZones_FEWS_2012',
                layerName = 'RW_LHZ_2012',
                labelVar = 'LZNAMEE',
                reproject = TRUE,
                projection = '+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                exportData = FALSE)

RWA_LZ$df = RWA_LZ$df %>% 
  mutate(livelihood_zone = case_when(RWA_LZ$df$LZNAMEE %like% 'Tea' ~ 'West Congo-Nile Crest Tea Zone',
                                     RWA_LZ$df$LZNAMEE %like% 'Wheat' ~ 'Northern Highland Beans and Wheat Zone',                          
                                     RWA_LZ$df$LZNAMEE %like% 'Eastern Congo' ~ 'East Congo-Nile Highland Subsistence Farming Zone',
                                     RWA_LZ$df$LZNAMEE %like% 'Volcanic' ~ 'Northwest Volcanic Irish Potato Zone',                            
                                     RWA_LZ$df$LZNAMEE %like% 'Mixed' ~ 'Eastern Plateau Mixed Agriculture Zone',
                                     RWA_LZ$df$LZNAMEE %like% 'Eastern Ag' ~ 'Eastern Agropastoral Zone',                                       
                                     RWA_LZ$df$LZNAMEE %like% 'Central-Northern' ~ 'Central-Northern Highland Irish Potato, Beans and Vegetable Zone',
                                     RWA_LZ$df$LZNAMEE %like% 'Kivu' ~ 'Lake Kivu Coffee Zone',
                                     RWA_LZ$df$LZNAMEE %like% 'Banana' ~ 'Southeastern Plateau Banana Zone',
                                     RWA_LZ$df$LZNAMEE %like% 'Bugesera' ~ 'Bugesera Cassava Zone',                                           
                                     RWA_LZ$df$LZNAMEE %like% 'Central Plateau' ~ 'Central Plateau Cassava and Coffee Zone',
                                     RWA_LZ$df$LZNAMEE %like% 'Semi-Arid' ~ 'Eastern Semi-Arid Agropastoral Zone',                             
                                     RWA_LZ$df$LZNAMEE %like% 'Urban' ~ 'Kigali city',
                                     TRUE ~ NA_character_),
         lz_name = case_when(RWA_LZ$df$LZNAMEE %like% 'Tea' ~ 'W. Congo-Nile Crest Tea',
                                    RWA_LZ$df$LZNAMEE %like% 'Wheat'            ~ 'N. Highland Beans/Wheat',                          
                                    RWA_LZ$df$LZNAMEE %like% 'Eastern Congo'    ~ 'E. Congo-Nile Highland Subsistence',
                                    RWA_LZ$df$LZNAMEE %like% 'Volcanic'         ~ 'N.W. Volcanic Irish Potato',                            
                                    RWA_LZ$df$LZNAMEE %like% 'Mixed'            ~ 'E. Plateau Mixed Agriculture',
                                    RWA_LZ$df$LZNAMEE %like% 'Eastern Ag'       ~ 'E. Agropastoral',                                       
                                    RWA_LZ$df$LZNAMEE %like% 'Central-Northern' ~ 'C.-N. Highland Irish Potato/Beans/Veg.',
                                    RWA_LZ$df$LZNAMEE %like% 'Kivu'             ~ 'Lake Kivu Coffee',
                                    RWA_LZ$df$LZNAMEE %like% 'Banana'           ~ 'S.E. Plateau Banana',
                                    RWA_LZ$df$LZNAMEE %like% 'Bugesera'         ~ 'Bugesera Cassava',                                           
                                    RWA_LZ$df$LZNAMEE %like% 'Central Plateau'  ~ 'C. Plateau Cassava/Coffee',
                                    RWA_LZ$df$LZNAMEE %like% 'Semi-Arid'        ~ 'E. Semi-Arid Agropastoral',                             
                                    RWA_LZ$df$LZNAMEE %like% 'Urban'            ~ 'Kigali City',
                                    TRUE ~ NA_character_))

RWA_LZ$centroids = RWA_LZ$centroids %>% 
  mutate(label = case_when(RWA_LZ$centroids$label %like% 'Tea' ~ 'West Congo-Nile Crest Tea Zone',
                           RWA_LZ$centroids$label %like% 'Wheat' ~ 'Northern Highland Beans and Wheat Zone',                          
                           RWA_LZ$centroids$label %like% 'Eastern Congo' ~ 'East Congo-Nile Highland Subsistence Farming Zone',
                           RWA_LZ$centroids$label %like% 'Volcanic' ~ 'Northwest Volcanic Irish Potato Zone',                            
                           RWA_LZ$centroids$label %like% 'Mixed' ~ 'Eastern Plateau Mixed Agriculture Zone',
                           RWA_LZ$centroids$label %like% 'Eastern Ag' ~ 'Eastern Agropastoral Zone',                                       
                           RWA_LZ$centroids$label %like% 'Central-Northern' ~ 'Central-Northern Highland Irish Potato, Beans and Vegetable Zone',
                           RWA_LZ$centroids$label %like% 'Kivu' ~ 'Lake Kivu Coffee Zone',
                           RWA_LZ$centroids$label %like% 'Banana' ~ 'Southeastern Plateau Banana Zone',
                           RWA_LZ$centroids$label %like% 'Bugesera' ~ 'Bugesera Cassava Zone',                                           
                           RWA_LZ$centroids$label %like% 'Central Plateau' ~ 'Central Plateau Cassava and Coffee Zone',
                           RWA_LZ$centroids$label %like% 'Semi-Arid' ~ 'Eastern Semi-Arid Agropastoral Zone',                             
                           RWA_LZ$centroids$label %like% 'Urban' ~ 'Kigali city',
                           TRUE ~ NA_character_))

# import lakes ------------------------------------------------------------
RWA_lakes = shp2df(baseDir =  baseDir_geo,
                   folderName = 'Rwanda basemaps',
                   layerName = 'RWA_Lakes',
                   getCentroids = FALSE,
                   reproject = TRUE,
                   projection = '+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
                   exportData = FALSE)



