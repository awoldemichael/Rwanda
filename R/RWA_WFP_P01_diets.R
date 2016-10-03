# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_05_P_diets.R: import household-level data
#
# Script to create plots for the dietary diversity and FCS scores of households within the 2015 CFSVA
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 14 September 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------


# load data ---------------------------------------------------------------
source('~/GitHub/Rwanda/R/RWA_WFP_runAll.R')


# create plot -------------------------------------------------------------
#' @param df data frame containing the raw, household-level food consumption data. Assumes the individual food group consumption data will be located in separate columns (e.g. legumes, vegetables, etc.)
#' @param region_var string containing the name of the region variable within df
#' @param FCS_var string containing the variable name with calculated FCS
#' @param food_vars list containing the column names containing individual food group consumption
#' 
#' @param na.rm T/F on whether to remove NAs from average
#' @param use_sampleWts whether or not to apply sample weights to the average values by region and for the country
#' @param psu_var string containing the primary sampling unit variable name
#' @param strata_var string containing the strata variable name
#' @param weight_var string containing the weight variable name
#' @param use_FCSWts whether or not to weight each food group by its contribution to FCS
#' @param plot_relativeAvg whether to use relative averages or straight averages in main heatmap
#' 
#' @param poor_FCS cutoff for a 'poor' FCS score
#' @param borderline_FCS cutoff for a 'borderline' FCS score
#' 
#' @param avg_colour colour palette for the main heatmap of the average values of food consumption
#' @param FCS_colour colour palette for the heatmap of avg. FCS by region and distribution
#' @param FCS_range list containing the limist of colors for FCS_colour; default is c(0, 112) -- full range of FCS scores
#' @param alpha_hist alpha (opacity) level for the KDE histogram of the FCS scores
#' 
#' @param admin0 data frame containing lat/long of base of the map. see `frontier::shp2df` for help importing shapefiles.
#' @param region_coords data frame containing lat/long of the regions of interest to map. see `frontier::shp2df` for help importing shapefiles.
#' @param map_base colour to fill the base of the map
#' @param map_accent colour to fill the highlighted region of the map
#' 
#' @param width_indivPlots list contianing the fraction each panel of the plot should occupy
#' 
#' @param filename (optional) filename to save the plots
#' @param width (optional) width of the exported plot
#' @param height (optional) height of the exported plot
#' @param units (optional) units for the width/height of exported plot
#' @param scale (optional) scaling factor for the exported plot
#' 

fcs_heatmap <- function(df,
                        region_var,
                        FCS_var = 'FCS',
                        food_vars = c('staples_days', 'pulse_days', 'meat_days', 'milk_days',
                                      'veg_days', 'oil_days', 'fruit_days', 'sugar_days'),
                        
                        # -- averaging options --
                        na.rm = FALSE,
                        use_sampleWts = FALSE,
                        psu_var = NA, strata_var = NA, weight_var = NA,
                        use_FCSWts = TRUE,
                        plot_relativeAvg = TRUE,
                        
                        # -- FCS values --
                        poor_FCS = 21,
                        borderline_FCS =  35,
                        
                        # -- colour options --
                        avg_colour = PlBl, # diverging palette
                        FCS_colour = c(brewer.pal(9, 'YlGnBu'), '#081d58', '#081d58', '#081d58', '#081d58'),
                        FCS_range = c(0, 112),
                        alpha_hist = 0.65,
                        font_light = 'Lato Light',
                        font_normal = 'Lato',
                        label_size = 3,
                        heat_stroke_size = 0.15,
                        
                        # -- map options --
                        plot_map = TRUE,
                        admin0 = NA,
                        region_coords = NA,
                        map_base = grey15K,
                        map_accent = '#d53e4f',
                        
                        # -- plot layout options --
                        width_indivPlots = c(0.1, 0.6, 0.2, 0.1),
                        
                        # -- file saving options --
                        filename = NA,
                        width = NA, 
                        height = NA, 
                        units = 'in', 
                        scale = 1) {
  
  # SETUP: checks and initialize vars --------------------------------------------
  if(plot_map == TRUE) {
    # check that the shapefiles for the maps exist
    if(is.na(admin0) | is.na(region_coords)) {
      stop('admin0 or region_coords not specified')
    }
  }
  
  if(use_sampleWts == TRUE) {
    # check that sample weighting params are specified
    if(is.na(psu_var) | is.na(weight_var) | is.na(strata_var)){
      stop('parameters to use sample weights are not specified')
    }
  }
  
  # Decide whether to weight main heatmap by their weight in calculating FCS score. 
  # Weights are from the World Food Programme https://www.wfp.org/content/technical-guidance-sheet-food-consumption-analysis-calculation-and-use-food-consumption-score-food-s
  if(use_FCSWts == TRUE){
    staples_weight = 2
    oil_weight =  0.5
    pulse_weight =  3
    sugar_weight =  0.5 
    veg_weight =  1
    milk_weight =  4
    meat_weight = 4 
    fruit_weight =  1 
  } else { # (weight equally)
    staples_weight = 1
    oil_weight =  1
    pulse_weight =  1
    sugar_weight =  1 
    veg_weight =  1
    milk_weight =  1
    meat_weight = 1 
    fruit_weight =  1 
  }
  
  # PART 0: calculate weighted averages for values ------------------------
  if(use_sampleWts == TRUE) {
    FCS_region = llamar::calcPtEst(df, 'FCS', by_var = region_var,
                                   psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')
    
  } else { # (calculate straight averages)
    
    # (a) -- average FCS by region --
    if(na.rm == TRUE) {
      # Exclude missing values
      FCS_region = df %>% 
        filter_(paste0('!is.na(', FCS_var,')')) %>% 
        group_by_(region_var) %>% 
        summarise_(.dots = list(N = 'n()', 
                                FCS_avg = paste0('mean(', FCS_var, ')')))
    } else{
      FCS_region = df %>% 
        group_by_(region_var) %>% 
        summarise_(.dots = list(N = 'n()', 
                                FCS_avg = paste0('mean(', FCS_var, ')')))
    }
    
    # (b) -- average # of days consumed each food, nationally --
    # (c) -- average # of days consumed each food, by region --
    
    
    fcs_heat = hh_raw %>% 
      group_by(regionName = livelihood_zone) %>% 
      mutate(staples_days = Starch,
             oil_days = Oil,
             pulse_days = Pulses,
             sugar_days = Sugar,
             veg_days = Vegetables,
             milk_days = Milk,
             meat_days = Meat,
             fruit_days = Fruit) %>% 
      summarise(staples = mean(staples_days) * staples_weight,
                oils = mean(oil_days) * oil_weight,
                pulses = mean(pulse_days) * pulse_weight,
                sugar = mean(sugar_days) * sugar_weight, 
                vegetables = mean(veg_days) * veg_weight,
                dairy = mean(milk_days) * milk_weight,
                meat = mean(meat_days) * meat_weight, 
                fruits  = mean(fruit_days) * fruit_weight, 
                fcs = mean(FCS)) %>% 
      arrange(desc(fcs))
    
    
    fcs_avg = hh_raw %>% 
      summarise(staples = mean(staples_days) * staples_weight,
                oils = mean(oil_days) * oil_weight,
                pulses = mean(pulse_days) * pulse_weight,
                sugar = mean(sugar_days) * sugar_weight, 
                vegetables = mean(veg_days) * veg_weight,
                dairy = mean(milk_days) * milk_weight,
                meat = mean(meat_days) * meat_weight, 
                fruits  = mean(fruit_days) * fruit_weight, 
                fcs = mean(FCS)) %>% 
      arrange(desc(fcs))
    
    
    rel_fcs_heat = fcs_heat %>% 
      mutate(staples = staples - fcs_avg$staples,
             oils = oils - fcs_avg$oils,
             pulses = pulses - fcs_avg$pulses,
             sugar = sugar - fcs_avg$sugar,
             vegetables = vegetables - fcs_avg$vegetables,
             dairy = dairy - fcs_avg$dairy,
             meat = meat - fcs_avg$meat,
             fruits  = fruits - fcs_avg$fruits)
  }
  
  # PART 1: individual maps ----------------------------------------------
  maps = ggplot()
  
  # PART 2: heatmap of food consumption by food group + region ------------
  FCS_heat = ggplot()
  
  # PART 3: distribution of FCS scores by region --------------------------
  FCS_hist = ggplot()
  
  # PART 4: average FCS score by region -----------------------------------
  FCS_avg = 
    ggplot(FCS_region, aes_string(y = paste0('forcats::fct_reorder(', region_var, ', FCS_avg)'), 
                                  x = '1',
                                  fill = 'FCS_avg')) +
    # -- heatmap --
    geom_tile(colour = 'white', size = heat_stroke_size) +
    
    # -- labels --
    geom_text(aes(label = round(FCS_avg, 0)),
              colour = 'white',
              family = font_normal,
              size = label_size) +
    
    # -- scales --
    coord_fixed(ratio  = 1) +
    scale_fill_gradientn(colours = FCS_colour, limits = FCS_range) + 
    
    # -- themes --
    theme_blank()
  
  # MERGE, PLOT, and SAVE --------------------------------------------------
  if(plot_map == TRUE){
    p = gridExtra::grid.arrange(maps, FCS_heat, FCS_hist, FCS_avg, ncol = 4, widths = width_indivPlots)
  } else {
    p = gridExtra::grid.arrange(FCS_heat, FCS_hist, FCS_avg, ncol = 3, widths = width_indivPlots[1:3])
  }
  
  # -- calls llamar::save_plot to save the plot --
  if (!is.na(filename)){
    save_plot(filename, width, height, units, scale)
  }
  
  return(p)
}




# NOTES -------------------------------------------------------------------
# * Includes all households, not just those with children.

hh_raw = hh_raw %>% 
  mutate(staples_days = Starch,
         oil_days = Oil,
         pulse_days = Pulses,
         sugar_days = Sugar,
         veg_days = Vegetables,
         milk_days = Milk,
         meat_days = Meat,
         fruit_days = Fruit) %>%   
  factorize(children_raw, 'S0_D_Dist', 'admin2') %>% 
  factorize(children_raw, 'livezone', 'livelihood_zone')




# -- plot --
widthDDheat = 3.25*2*1.15
heightDDheat = 3*2
widthDDavg = 1.85
fcsRange = c(30, 60)

fcsOrder = rev(rel_fcs_heat$regionName)

View(t(hh_raw  %>% select(contains('days')) %>% summarise_each(funs(mean))))

foodOrder = c('staples', 'oils', 
              'vegetables', 'meat',
              'sugar', 'dairy', 'fruits', 'pulses')

rel_fcs_heat = rel_fcs_heat %>% 
  gather(food, rel_mean, -regionName, -fcs)

rel_fcs_heat$regionName = 
  factor(rel_fcs_heat$regionName,
         fcsOrder)

rel_fcs_heat$food = 
  factor(rel_fcs_heat$food,
         foodOrder)


# Main heatmap
q = ggplot(rel_fcs_heat) +
  geom_tile(aes(x = food, y = regionName, fill = rel_mean), 
            color = 'white', size = 1) +
  scale_fill_gradientn(colours = avg_colour, 
                       limits = c(-8.2,8.2)) +
  
  geom_text(aes(y = food, x = regionName, label = round(rel_mean,1)), size = 4) +
  
  # -- labels --
  ggtitle('FCS, relative to the national average') +
  
  # -- force plot to have square tiles --
  coord_fixed(ratio = 1) +
  
  # -- themes --
  theme_xylab() +
  
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 18, family = 'Segoe UI', hjust = 0, color = grey60K))







hh_copy = data.frame(livelihood_zone = c(rep('West Congo-Nile Crest Tea Zone', 7500), 
                                         rep('Lake Kivu Coffee Zone', 7500)),
                     FCS = c(hh$FCS, hh$FCS))

p = ggplot(hh, aes(x = FCS)) +
  # -- total density distribution --
  geom_density(size = 0.25, colour = grey60K,
               fill = grey30K,
               data = hh_copy, 
               alpha = alpha_fill) +
  
  # -- gradient shading of color -- 
  geom_histogram(aes(x = x, y = 4 *..density.., fill = ..x..),
                 binwidth = 1,
                 data = data.frame(x = 1:112),
                 alpha = alpha_fill) +
  
  # -- reference lines of poor and borderline FCS scores --
  geom_vline(xintercept = poor_FCS, 
             colour = grey90K, size = 0.1) +
  geom_vline(xintercept = borderline_FCS, 
             colour = grey90K, size = 0.1) +
  
  # -- annotation --
  annotate('text', x = poor_FCS, y = .05, 
           label = 'poor',
           hjust = 0.5, family = font_light, 
           size = 2, colour = grey60K) +
  
  # -- density distribution (stroke) --
  geom_density(size = 0.25, colour = grey90K) +
  
  # -- density distribution (for clipping) --
  # **! keep as the outer most element for ease of clipping in AI.
  # geom_density(fill = 'dodgerblue') +
  
  facet_wrap(~fct_reorder(livelihood_zone, FCS), ncol = 1) +
  
  theme_xaxis() +
  theme(strip.text = element_text(size = 8),
        panel.margin = unit(0, 'lines')) + 
  
  scale_fill_gradientn(colours = FCS_colour)
