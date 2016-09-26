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


# load libraries ----------------------------------------------------------
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)

# import data -------------------------------------------------------------

base_dir = '~/Documents/USAID/Rwanda/processeddata/RWA_DHS2014-2015_stuntedrasters/'

raw_stunted_raster = raster(paste0(base_dir, 'rwastuntings1.tif'))

# Pull out just the data from the matrix
stunted_matrix = as.matrix(raw_stunted_raster)

# linearize
stunted_array = data.frame(pct = as.vector(raw_stunted_raster))

ggplot(stunted_array, aes(x = pct)) + 
  # -- histogram to get the color bars --
  geom_histogram(binwidth = 0.01, 
                 aes(y = ..density..,
                   fill = ..x..)) +
  
  # -- density to get the surface --
  geom_density(size = 0.125, colour = grey90K) +
  
  # -- scales --
  scale_fill_gradientn(colours = stunting_pal, 
                       limits = stunting_range,
                       name = NULL,
                       labels = scales::percent) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.8, by = 0.2),
                     limits = c(0.0, 0.80)) +
  # -- themes --
  theme_xgrid() +
  theme(legend.position = c(0.1, 0.4),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 12, 
                                   family = font_light, hjust = 0, color = grey60K))


# save plots --------------------------------------------------------------

save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_DHS_interpHist.pdf', 
          width = 8.5, height = 4.5)
