
# Compare stunting values from the World Bank's DHS analysis to ou --------
# Laura Hughes, lhughes@usaid.gov, 6 June 2017, USAID GeoCenter



# Intro -------------------------------------------------------------------
# In our analysis of the 2010 and 2014/2015 Rwanda Demographic and Heealth Surveys,
# we observed that most of the areas showed decreases in stunting between 2014/2015 and 2010.
# Our analysis included all children under 5, and children were grouped by FEWS NET livelihood zones.

# At the same time, the World Bank did a separate study, which indicated that 

# These plots are intended to highlight the uncertainty in the measurements and to highlight where there 
# is agreement between the DHS and the CFSVA, and between the two age cohorts of children.



# setup -------------------------------------------------------------------

# set wd
setwd("~/Rwanda/")

library(tidyverse)
library(haven)
library(readxl)
library(stringr)
library(forcats)
library(extrafont)

grey60K = 'grey'
grey75K = '#878787'
grey90K = '#4d4d4d'

theme_xgrid <- function(font_normal = 'Lato',
                        font_semi = 'Lato',
                        font_light = 'Lato Light',
                        legend.position = 'none',
                        legend.direction = 'horizontal',
                        panel_spacing = 3, # panel spacing, in lines
                        font_axis_label = 12,
                        font_axis_title = font_axis_label * 1.15,
                        font_facet = font_axis_label * 1.15,
                        font_legend_title = font_axis_label, 
                        font_legend_label = font_axis_label * 0.8,
                        font_subtitle = font_axis_label * 1.2,
                        font_title = font_axis_label * 1.3,
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE
) {
  
  check_font = function(font_name){
    installed_fonts = extrafont::fonts()
    
    if (font_name %in% installed_fonts) {
      return(TRUE) 
    } else {
      return(FALSE)
    }
  }
  
  replace_font = function(font_name, default_font = 'sans') {
    if(check_font(font_name) == FALSE) {
      # font isn't installed
      return(default_font)
    } else{
      return(font_name)
    }
  }
  
  
  set_aesthetics = function(font_normal, font_semi, font_light, projector) {
    # Decide whether to use projector mode or not.
    if(projector == FALSE) {
      # Define standard colours for grid lines and text
      normal_grid_stroke = 0.1
      
      normal_grid = grey60K
      
      text_colour = grey60K
      subtitle_colour = grey75K
      title_colour = grey90K
      
      grid_stroke = normal_grid_stroke
      grid_colour = normal_grid
      
      # Check if fonts are defined.
      font_normal = replace_font(font_name = font_normal)
      font_semi = replace_font(font_name = font_semi)
      font_light = replace_font(font_name = font_light)
      
    } else {
      # Define standard colours for grid lines and text
      projector_grid_stroke = 0.25
      
      projector_grid = grey75K
      
      text_colour = grey75K
      subtitle_colour = grey90K
      title_colour = grey90K
      
      grid_stroke = projector_grid_stroke
      grid_colour = projector_grid
      
      # Check if fonts are defined; use darker font.
      font_normal = replace_font(font_name = font_normal)
      font_semi = replace_font(font_name = font_normal)
      font_light = replace_font(font_name = font_normal)
    }
    
    return(list(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                grid_stroke = grid_stroke, grid_colour = grid_colour, 
                text_colour = text_colour, subtitle_colour = subtitle_colour, title_colour = title_colour))
  }
  
  # -- Set the aesthetics (common to all the themes) --
  aesthetics = set_aesthetics(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                              projector = projector)
  
  # -- Unpack aesthetics --
  list2env(aesthetics, environment())
  
  
  # -- Choose background colour --
  background_colour = ifelse(grey_background == TRUE, background_colour, NA)
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # -- theme --  
  theme(
    title = element_text(size = font_title, colour = title_colour, family = font_normal),
    plot.subtitle = element_text(size = font_subtitle, colour = subtitle_colour, family = font_semi),
    text = element_text(family = font_light, colour = text_colour, hjust = 0.5),
    
    axis.line = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.x = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.x = element_text(size = font_axis_title, colour = text_colour, family = font_semi), 
    axis.text.y = element_text(size = font_axis_label, colour = text_colour, family = font_light), 
    axis.title.y = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = text_colour, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = text_colour, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.spacing = unit(panel_spacing, "lines"), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_line(size = grid_stroke, colour = grid_colour), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = subtitle_colour, hjust = 0.025), 
    strip.background = element_blank())
}


# import data -------------------------------------------------------------
# Statistics were calculated by Tim Essam (tessam@usaid.gov) in Stata. 
# calculated by district (weighted) using [aw=cweight] syntax in Stata
# All children without flag by DHS included in calculation; grouped by all children or children < 24 months
dhs2 = read_tsv('processeddata/stunting_by_district_under2.txt') %>% 
  mutate(age_filter = 'under2')

dhs5 = read_tsv('processeddata/stunting_by_district.txt') %>% 
  mutate(age_filter = 'under5')

dhs = bind_rows(dhs2, dhs5)

# Data pulled from the WB's report.  Note that numbers are similar but slightly different than our DHS under 2 numbers,
# suggesting the WB used additional filters based on their regression model (?)
wb = read_excel('processeddata/wb_stunting_dhs.xlsx')


# clean up dhs data -------------------------------------------------------

dhs2 =  dhs %>% 
  mutate(district = str_replace(X1, 'stunted2:', '')) %>% 
  rename(year = c1, N = r1, avg = b, lb = ll, ub = ul) %>% 
  select(district, year, age_filter, avg, lb, ub, N) %>% 
  group_by(district) %>% 
  mutate(avg2014 = lead(avg),
         diff = avg2014 - avg) %>% 
  arrange(desc(diff))

dist_order = dhs2 %>% filter(!is.na(diff))

dhs2$district = factor(dhs2$district, levels = dist_order$district)


# dot plots by province of the averages -----------------------------------

  ggplot(dhs2 %>% filter(age_filter == 'under2'), aes(x = avg, y = district, 
                 fill = age_filter, 
                 shape = as.factor(year))) +
  
  geom_segment(aes(xend = avg2014, yend = district), size = 0.5) +
  geom_segment(aes(x = lb, xend = ub, yend = district),
               size = 2, alpha = 0.15) +
  
  geom_point(size = 4) +
  
  scale_shape_manual(values = c(21,22)) +
  theme_bw()
