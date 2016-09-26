# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_P03_plotIndivDist.R: save maps of each indiv district
#
# Script to make choropleths highlighting each of the districts in Rwanda 
# 
# Administrative data from the Rwanda National Insitute of Statistics Rwanda
# Available at http://geodata.nisr.opendata.arcgis.com/datasets?q=Rwanda
#
# Laura Hughes, lhughes@usaid.gov, 23 September 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

# Define global opts ------------------------------------------------------

highlight_colour = brewer.pal(11, 'Spectral')[2] # fill colour for each district
rw_size = 2; # number of inches of outputted pdf


# Find all district names --------------------------------------------------

districts = unique(RWA_admin2$df$District)


# plot-n-save -------------------------------------------------------------

for (i in seq_along(districts)){
  
  df = RWA_admin2$df %>% 
    filter(District == districts[i])
  
  ggplot(df, aes(x = long, y = lat, group = group)) + 
    
    # -- base fill the country --
    geom_polygon(fill = grey15K, data = RWA_admin0$df) +
    # -- themes --
    theme_void() + 
    coord_equal() +
    
    
    # -- choropleth over regions --
    geom_polygon(fill = highlight_colour) 
  
  save_plot(paste0('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/', districts[i], '_raw.pdf'),
            width = rw_size, 
            height = rw_size)
  
}