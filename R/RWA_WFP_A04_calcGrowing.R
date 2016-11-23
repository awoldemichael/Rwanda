# Rwanda stunting analysis -----------------------------------------
#
# RWA_WFP_A04_calcGrowing.R
#
# Script to calculate point estimate data for growing habits data for Rwanda from the CFSVA dataset
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 14 November 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------


# import data -------------------------------------------------------------
setwd('~/GitHub/Rwanda/R/')
source('RWA_WFP_run2015.R')

# Calculate estimates at the Admin2 level ---------------------------------
# Can't weight by sampling weights, since don't have the village ID.

growing_admin2 = hh %>% 
  select(contains('growing'), admin2, own_livestock, own_cattle, manage_livestock) %>% 
           group_by(admin2) %>% 
           summarise_each(funs(mean(., na.rm = TRUE)))

growing_map = left_join(RWA_admin2$df, growing_admin2, by = c('District' = 'admin2'))

growing_labels = left_join(RWA_admin2$centroids, growing_admin2, by = c('label' = 'admin2'))

vars = colnames(growing_admin2 %>% select(-admin2))

for(fill_var in vars){

ggplot(growing_map) +
  geom_polygon(aes_string(x = 'long', y = 'lat',
                            group = 'group', order = 'order',
                            fill = fill_var)) +
  geom_path(aes_string(x = 'long', y = 'lat',
                         group = 'group', order = 'order'),
            size = 0.2,
            colour = 'white') +
  coord_equal() +
  theme_void() +
  theme(legend.position = 'none') +
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu'), limits = c(0, 1)) +
  geom_text(aes_string(x = 'long', y = 'lat', label = paste0('percent(', fill_var, ', 0)'), 
                       group = 'label', colour = fill_var),
            family = 'Lato', 
            size = 3, 
            data = growing_labels) +
   scale_colour_text(growing_labels[[fill_var]]) +
  ggtitle(fill_var)
  
  save_plot(filename = paste0('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/', fill_var, '.pdf'),
                              width = 6, height = 6)
}


# by lz -------------------------------------------------------------------

growing_lz = hh %>% 
  select(contains('growing'), livelihood_zone, own_livestock, own_cattle, manage_livestock) %>% 
  group_by(livelihood_zone) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

growing_map = left_join(RWA_LZ$df, growing_lz, by = c('livelihood_zone' = 'livelihood_zone'))

growing_labels = left_join(RWA_LZ$centroids, growing_lz, by = c('label' = 'livelihood_zone'))

vars = colnames(growing_lz %>% select(-livelihood_zone))

for(fill_var in vars){
  
  ggplot(growing_map) +
    geom_polygon(aes_string(x = 'long', y = 'lat',
                            group = 'group', order = 'order',
                            fill = fill_var)) +
    geom_path(aes_string(x = 'long', y = 'lat',
                         group = 'group', order = 'order'),
              size = 0.2,
              colour = 'white') +
    coord_equal() +
    theme_void() +
    theme(legend.position = 'none') +
    scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu'), limits = c(0, 1)) +
    geom_text(aes_string(x = 'long', y = 'lat', label = paste0('percent(', fill_var, ', 0)'), 
                         group = 'label', colour = fill_var),
              family = 'Lato', 
              size = 3, 
              data = growing_labels) +
    scale_colour_text(growing_labels[[fill_var]]) +
    ggtitle(fill_var)
  
  save_plot(filename = paste0('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/', fill_var, '_lz.pdf'),
            width = 6, height = 6)
}
