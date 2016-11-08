# Rwanda stunting analysis -----------------------------------------
# Compare CHAIN sector-level data to livelihood zones ---------------------
#
# RWA_compareCHAIN.R
#
# Script to translate CHAIN location data into livelihood zones.
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 8 November 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------


# DEPENDS: previous functions to run: ----------------------------------------------
# setwd('~/GitHub/Rwanda/R/')
# source('RWA_WFP_00_setup.R')
# source('RWA_WFP_03_importHH.R')
library(jsonlite)



# setup options -----------------------------------------------------------
ta_list = c('NS', 'WS') # technical areas: nutrition specific or wash-specific
intervention_list = c('PHP', 'HE')

# convert sectors to LZ ---------------------------------------------------

# translate sectors into livelihood zones.
# Note: relying on the WFP's classification of sectors and livelihood zones.
# Assumes a sector is in a SINGLE livelihood zone.  Appears based on maps to be mostly correct, with some exceptions in the Eastern Agro-pastoral zones.

admin3_codebk = hh %>% 
  select(admin1, admin2, admin3, livelihood_zone) %>% 
  distinct()





# import sector-level data ------------------------------------------------

# Instructions on importing from json file directly:
# remove '\' from file
# replace '{"' with '{'
# replace '"}' with '}'

# Import data from Baboyma's dataset
sectors = jsonlite::fromJSON('~/GitHub/RwandaCHAIN/www/data/intervention-location_2016-11-08.json', flatten = T, simplifyMatrix = T, simplifyDataFrame = T)
sectors = sectors$data

interventions = jsonlite::fromJSON('~/GitHub/RwandaCHAIN/www/data/intervention-list.json', flatten = T, simplifyMatrix = T, simplifyDataFrame = T)


# merge together names ----------------------------------------------------
sectors = left_join(sectors, interventions, c("intervention" = "icode",
                                              "techoffice" = "tacode"))

# Filter out nutrition-related info ------------------------------------------
stunting_interv = sectors %>% 
  filter(techoffice %in% ta_list | intervention %in% intervention_list) %>% 
  select(techoffice, techarea, icode = intervention, intervention = intervention.y,
         province, district, sector, partner)

# Collapse down to the sector-level
stunting_interv = stunting_interv %>% 
  group_by(intervention, province, district, sector) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

stunting_interv_tot = stunting_interv %>% 
  group_by(intervention, province, district, sector) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


# merge with geodata ------------------------------------------------------


