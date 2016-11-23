# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_P04_stuntingChoros.R: plot choropleths of stunting
#
# Script to make choropleths highlighting each of the districts
# and livelihood zones in Rwanda for DHS and CFSVA data
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

# Define global opts ------------------------------------------------------

rw_width = 7.1397 # output width


# import averages ---------------------------------------------------------
# !!!!! NOTE: weighted averages used for the CFSVA and DHS, including for the livelihood zones.
setwd('~/GitHub/Rwanda/R')

source('RWA_WFP_run2015.R')

# Plot Maps --------------------------------------------------------------------
plot_choro = function(df, 
                      admin0, # base map
                      clipping_mask, # clipping mask (us. country)
                      centroids = NA,
                      centroids_var = 'region_name',
                      lakes = NA, # inland water
                      bounding_x = NA,
                      bounding_y = NA,
                      plot_base = TRUE,
                      fill_var = 'id',
                      exportPlot = FALSE, 
                      fileName = 'map.pdf', 
                      stroke_width = 0.075,
                      stroke_colour = grey75K,
                      size_label = 4,
                      label_y = 0.05,
                      fill_scale = NA,
                      fill_limits = NA,
                      bg_fill = '#f6f8fb',#d3dceb', # water #ebf0f9
                      lakes_fill = '#0067b9', # inland water
                      base_fill = grey15K, # underlying country
                      font_normal = 'Lato',
                      font_light = 'Lato Light',
                      # alpha = 0.7, 
                      title = NA,
                      plotWidth = 10.75, plotHeight = 9) {
  
  p = ggplot(df, aes(x = long, y = lat, group = group)) + 
    
    # -- choropleth over regions --
    geom_polygon(aes_string(fill = fill_var)) +
    geom_path(colour = stroke_colour, size = stroke_width) +
    
    
    
    
    # -- Admin 0 outline (for clipping if needed) --
    geom_path(colour = stroke_colour, size = stroke_width*3,
              data = clipping_mask) +
    
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    
    # -- themes --
    theme_void() + 
    theme(
      legend.position = c(0.2, 0.7),
      plot.margin = margin(0)) 
  
  
  # -- add lakes and inland water --
  if(!is.na(lakes)) {
    p = p +
      geom_polygon(fill = lakes_fill, data = lakes) 
  }
  
  # -- add title --
  if(!is.na(title)){
    p = p +
      ggtitle(title)
  }
  
  # -- scale color by fill_scale to create choropleth --
  if(!is.na(fill_scale)) {
    if(is.na(fill_limits)) {
      fill_limits = c(0, 1)
    }
    p = p +  
      scale_fill_gradientn(colours = fill_scale, 
                           limits = fill_limits)    
  }
  
  # -- Add labels at centroids --
  if(!is.na(centroids)) {
    # define color based on value
    # colours = df %>% 
    # mutate(case_when(df[[fill_var]] > mean(df[[fill_var]]) ~ '#ffffff',
    # TRUE ~ 'black'))
    
    df_avg =  df %>% 
      group_by_(centroids_var) %>% 
      summarise_(var_pct = paste0('llamar::percent(mean(', fill_var,'), ndigits = 0)'))
    
    centroids = left_join(centroids, df_avg, by = c('label' = centroids_var))
    
    p = p +
      geom_text(aes(label = label, x = long, y = lat, group = 1),
                size = size_label,
                colour = grey90K,
                family = font_normal,
                data = centroids) +
      geom_text(aes(label = var_pct, x = long, y = lat, group = 1),
                size = size_label,
                colour = grey90K,
                family = font_light,
                nudge_y = -label_y,
                data = centroids)
  }
  
  # -- resize by bounding_box --
  if(!is.na(bounding_x) & !is.na(bounding_y)) {
    p = p +
      coord_equal(xlim = bounding_x, ylim = bounding_y)
  }
  
  # -- export ==
  if (exportPlot == TRUE) {
    ggsave(plot = p,
           filename = fileName, 
           width = plotWidth, height = plotHeight, units = "in", 
           bg = "transparent", 
           paper = "special", useDingbats = FALSE, compress = FALSE, dpi = 300)
  }
  
  return(p)
  
}


# DHS, by district --------------------------------------------------------
rw_polygons = left_join(RWA_admin2$df, stunting_admin2, by = c('District' = 'admin2'))

plot_choro(rw_polygons,          
           admin0 = RWA_admin0,         
           clipping_mask = RWA_admin0, 
           centroids = RWA_admin2$centroids,
           fill_var = 'stunting_dhs',
           centroids_var = 'District',
           fill_scale = stunting_pal,
           fill_limits = stunting_range, 
           plot_base = FALSE,
           exportPlot = TRUE, 
           plotWidth = rw_width,
           plotHeight = rw_width,
           fileName = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_stunted_admin2_dhs.pdf')

# CFSVA, by district --------------------------------------------------------

plot_choro(rw_polygons,          
           admin0 = RWA_admin0,         
           clipping_mask = RWA_admin0, 
           centroids = RWA_admin2$centroids,
           fill_var = 'stunting_cfsva',
           centroids_var = 'District',
           fill_scale = stunting_pal,
           fill_limits = stunting_range, 
           plot_base = FALSE,
           exportPlot = TRUE, 
           plotWidth = rw_width,
           plotHeight = rw_width,
           fileName = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_stunted_admin2_cfsva.pdf')


# DHS, by lz --------------------------------------------------------
rw_polygons_lz = left_join(RWA_LZ$df, stunting_lz, by = c('livelihood_zone' = 'livelihood_zone'))

plot_choro(rw_polygons_lz,          
           admin0 = RWA_admin0,         
           clipping_mask = RWA_admin0, 
           centroids = RWA_LZ$centroids,
           fill_var = 'stunting_dhs',
           centroids_var = 'livelihood_zone',
           fill_scale = stunting_pal,
           fill_limits = stunting_range, 
           plot_base = FALSE,
           exportPlot = TRUE, 
           plotWidth = rw_width,
           plotHeight = rw_width,
           fileName = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_stunted_LZ_dhs.pdf')


# CFSVA, by lz ------------------------------------------------------------

plot_choro(rw_polygons_lz,          
           admin0 = RWA_admin0,         
           clipping_mask = RWA_admin0, 
           centroids = RWA_LZ$centroids,
           fill_var = 'stunting_cfsva',
           centroids_var = 'livelihood_zone',
           fill_scale = stunting_pal,
           fill_limits = stunting_range, 
           plot_base = FALSE,
           exportPlot = TRUE, 
           plotWidth = rw_width,
           plotHeight = rw_width,
           fileName = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_stunted_LZ_cfsva.pdf')

