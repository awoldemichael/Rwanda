# Rwanda stunting analysis -----------------------------------------
#
#
# Script to pull interpolated stunting data from an emperical Bayesian
# krigging of processed stunting data
# 
# Data are from the 2014/2015 Demographic and Health Surveys

# Laura Hughes, lhughes@usaid.gov, 14 September 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

# helpful article on working w/ raster data http://neondataskills.org/R/Raster-Data-In-R/


# set params --------------------------------------------------------------

fill_colour = 'BuPu' # RColorBrewer palette
fill_limits = c(0, 1)

# load libraries ----------------------------------------------------------
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# import data -------------------------------------------------------------

base_dir = '~/Documents/USAID/Rwanda/processeddata/RWA_DHS2014-2015_stuntedrasters/'

raw_stunted_raster = raster(paste0(base_dir, 'rwastuntings1.tif'))

# Pull out just the data from the matrix
stunted_matrix = as.matrix(raw_stunted_raster)

# linearize
stunted_array = data.frame(pct = as.vector(raw_stunted_raster))

ggplot(stunted_array, aes(x = pct)) + 
  geom_histogram(binwidth = 0.01, 
                 aes(y = ..density..,
                   fill = ..x..)) +
  geom_density() +
  scale_fill_gradientn(colours = brewer.pal(9, fill_colour), 
                       values = fill_limits)
