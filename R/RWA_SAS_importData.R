# RWA_SAS_01_importData.R
# Imports and manipulates the 2013 Rwadna Seasonal Agriculture surveys.
# Laura Hughes, lhughes@usaid.gov
# 28 March 2017


# setup -------------------------------------------------------------------

baseDir = '~/Documents/USAID/Rwanda/rawdata/RW_2013_SAS/rsas-2013-data-stata-Season A/'

files = list.files(path = baseDir)

library(haven)
library(dplyr)
library(tidyr)


# import data -------------------------------------------------------------
allData = NULL

for (file in files) {
  print(paste0("importing ", file))
  df = read_stata(paste0(baseDir, file))  
  glimpse(df)
  
  allData[[file]]  = df
  
  View(allData[[file]])
}
