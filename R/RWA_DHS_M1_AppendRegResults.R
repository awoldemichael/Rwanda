# Purpose: attached regression results to livelihood zones
# By; Tim Essam
# Date: 2017/01/10

library(foreign)
library(tidyverse)
library(RCurl)
library(data.table) # if using %like% for matching livelihood zone names

# load the fews net livelihoods zones
  setwd("~/Rwanda/GIS/Shapefiles")
  lvd <- read.dbf("RW_LHZ_2012.dbf")
  
  reg_results <- read.csv(
    text = getURL("https://raw.githubusercontent.com/tessam30/Rwanda/master/Export/Results_lvdzones.csv")
    )

# Merge in regression results to .dbf data, livelihood name is the "merge" var
  lvd2 = left_join(lvd, reg_results, c("LZNAMEE" = "lvd"))
  glimpse(lvd2)
  
# Save new dbf (other shapefile names changed to align)
  write.dbf(lvd2, file = "RW_LHZ_REG_2012.dbf")

