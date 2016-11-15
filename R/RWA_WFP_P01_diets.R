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



# NOTES -------------------------------------------------------------------
# * Includes all households, not just those with children.

# By livelihood zone
fcs_byLZ = fcs_heatmap(df = hh, region_var = 'lz_name', plot_map = TRUE, admin0 = RWA_admin0, region_coords = RWA_LZ$df,
                filename = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/FCS_CFSVA.pdf',
                width_indivPlots = c(0.075, 0.65, 0.2, 0.075),
                width = 8.5, height = 5.5)

# By district
fcs_byDist = fcs_heatmap(df = hh, region_var = 'admin2', map_region_var = 'District',
                plot_map = TRUE, admin0 = RWA_admin0, region_coords = RWA_admin2$df,
                filename = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/FCS_CFSVA_admin2.pdf',
                width_indivPlots = c(0.075, 0.65, 0.2, 0.075),
                width = 8.5, height = 5.5)


# by livelihood zone, divided urban/rural ---------------------------------
fcs_rural = fcs_heatmap(df = hh %>% filter(rural_cat == 'Rural'), region_var = 'lz_name', admin0 = RWA_admin0, region_coords = RWA_LZ$df,
                       plot_map = FALSE,
                       width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(fcs_rural)

# Filtering >= 80 hh
fcs_urban = fcs_heatmap(df = hh %>% filter(rural_cat == 'Urban', 
                                           livelihood_zone %in% c('Lake Kivu Coffee Zone',
                                                                  'Southeastern Plateau Banana Zone',
                                                                  'Northwest Volcanic Irish Potato Zone',
                                                                  'Central Plateau Cassava and Coffee Zone',
                                                                  'Kigali city'
                                                                  )), region_var = 'lz_name', admin0 = RWA_admin0, region_coords = RWA_LZ$df,
                        plot_map = FALSE,
                        width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(fcs_urban)

# dietary diversity -------------------------------------------------------

x = fcs_heatmap(df = hh, region_var = 'lz_name', FCS_var = 'HDDS_24h', use_FCSWts = FALSE,
                FCS_range = c(0, 13), poor_FCS = 4, borderline_FCS = 6,
                       plot_map = FALSE,
                       width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(x)

x = fcs_heatmap(df = hh, region_var = 'lz_name', FCS_var = 'DDS', use_FCSWts = FALSE,
                FCS_range = c(0, 8), poor_FCS = 1, borderline_FCS = 1,
                plot_map = FALSE,
                width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(x)

x = fcs_heatmap(df = hh, region_var = 'lz_name', FCS_var = 'GDDS', use_FCSWts = FALSE,
                FCS_range = c(0, 4), poor_FCS = 1, borderline_FCS = 2,
                plot_map = FALSE,
                width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(x)



# FCS by wealth -----------------------------------------------------------

x = hh %>% select(food_expend, monthly_expend, sh_food_expend, hh_size, 
                  staples_days, milk_days, meat_days, veg_days, fruit_days, 
                  pulse_days, FCS, wealth_idx_cat,
                  # vitAfruitveg_days, protein_days, ironrich_days,
                  sugar_days, oil_days) %>% mutate(x = (food_expend/hh_size)/monthly_expend) %>% mutate(ratio = x/sh_food_expend)

x = x %>% gather(food, days, -food_expend, -wealth_idx_cat, -monthly_expend, -sh_food_expend, -hh_size, -x, -ratio, -FCS)


ggplot(x, aes(x = FCS, y = days, colour = food)) +
  stat_smooth(span = .75) +
  # coord_cartesian(xlim = c(0, 1), ylim = c(0, 7)) +
  scale_x_continuous(labels = scales::percent) +
  xlab('percent of per capita income spent on food')


ggplot(x, aes(x = FCS, y = days, colour = food)) +
  geom_smooth(span = 1e6) +
  # coord_cartesian(xlim = c(0, 1), ylim = c(0, 7)) +
  # scale_x_continuous(labels = scales::percent) +
  xlab('FCS')


ggplot(ch, aes(x = age_months, y = stuntingZ, colour = sex)) +
  geom_smooth() +
  # coord_cartesian(xlim = c(0, 1), ylim = c(0, 7)) +
  # scale_x_continuous(labels = scales::percent) +
  xlab('percent of per capita income spent on food')

ggplot(ch, aes(x = age_months, y = isStunted, colour = sex)) +
  geom_smooth() +
  # coord_cartesian(xlim = c(0, 1), ylim = c(0, 7)) +
  # scale_x_continuous(labels = scales::percent) +
  xlab('percent of per capita income spent on food')


# 2012 --------------------------------------------------------------------
hh2012_plot = removeAttributes(hh2012)
hh2012_plot = factorize(hh2012_plot, hh2012, 'fews_code', 'lz')

x = fcs_heatmap(df = hh2012_plot, region_var = 'lz', FCS_var = 'FCS', na.rm = TRUE,low_FCS_top = F,
                use_FCSWts = TRUE,
                plot_map = FALSE, 
                staples_var = 'Starches',
                fruit_var = 'Fruits',
                pulse_var = 'Pulses',
                veg_var = 'Vegs',
                sugar_var = 'Sugar',
                milk_var = 'Milk',
                meat_var = 'Protein',
                oil_var = 'Oil',
                width_indivPlots = c(0.65, 0.25, 0.1))
grid.arrange(x)


ggplot(hh2012, aes(FCS)) +
  geom_density() + 
  facet_wrap(~fews_code)
fcs_byLZ = fcs_heatmap(df = hh, region_var = 'lz_name', plot_map = TRUE, admin0 = RWA_admin0, region_coords = RWA_LZ$df,
                       filename = '~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/FCS_CFSVA.pdf',
                       width_indivPlots = c(0.075, 0.65, 0.2, 0.075),
                       width = 8.5, height = 5.5)



# choropleth: FCS, food security --------------------------------------------------------------
fcs_byLZ = hh %>% 
  filter(!is.na(FCS)) %>% 
  group_by(livelihood_zone) %>% 
  summarise(fcs = mean(FCS), 
            months_FA = mean(months_food_access),
            n = n())

food_access_byLZ = hh %>% 
  filter(!is.na(FCS)) %>% 
  group_by(livelihood_zone, food_access_year_cat) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(livelihood_zone) %>% 
  mutate(pct = n/sum(n))

# Flip "No food access issues" to "Any Food Access Issues"
library(data.table)
food_access_byLZ = food_access_byLZ %>% 
  mutate(pct = ifelse(food_access_year_cat %like% 'No food', 
                      1 - pct, 
                      pct), 
         food_access_year_cat = ifelse(food_access_year_cat %like% 'No food', 
                                       'Any food access issue', 
                                       as.character(food_access_year_cat)))


# FCS choropleth ----------------------------------------------------------
fcs_map = full_join(RWA_LZ$df, fcs_byLZ, by = "livelihood_zone")
fcs_label = full_join(RWA_LZ$centroids, fcs_byLZ, by = c("label" = "livelihood_zone"))

ggplot(fcs_map, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = fcs, group = group, order = order)) +
  geom_path(aes(group = group, order = order),
            colour = 'white', size = 0.1) +
  geom_text(aes(label = round(fcs, 0)),
            size = 5,
            colour = 'white', 
            family = 'Lato', 
            data = fcs_label) +
  scale_fill_gradientn(colours = c(brewer.pal(9, 
                                    "YlGnBu"), 
                                   "#081d58", "#081d58", "#081d58", "#081d58"),
                       limits = c(0, 112)) +
  coord_equal() +
  theme_blank()

save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/FCS_LZmap_CFSVA.pdf',
          height = 6, width = 6)


# Num Months Food Access choropleth ----------------------------------------------------------

ggplot(fcs_map, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = months_FA, group = group, order = order)) +
  geom_path(aes(group = group, order = order),
            colour = 'white', size = 0.1) +
  geom_text(aes(label = round(months_FA, 1)),
            size = 5,
            colour = 'white', 
            family = 'Lato', 
            data = fcs_label) +
  scale_fill_gradientn(colours = brewer.pal(9, 'OrRd'),
                                   limits = c(0, 3)) +
  coord_equal() +
  theme_blank()

save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/FoodAccessMonth_LZmap_CFSVA.pdf',
          height = 6, width = 6)


# Food Access choropleth ----------------------------------------------------------
food_access_map = full_join(RWA_LZ$df, food_access_byLZ, by = "livelihood_zone")
food_access_label = full_join(RWA_LZ$centroids, food_access_byLZ, by = c("label" = "livelihood_zone"))

food_access_map$food_access_year_cat = 
  fct_relevel(food_access_map$food_access_year_cat, 
              "Any food access issue", "Seasonal food access issues",
              "Acute food access issues",   
              "Chronic food access issues")

food_access_label$food_access_year_cat = fct_relevel(food_access_label$food_access_year_cat, 
            "Any food access issue", "Seasonal food access issues",
            "Acute food access issues",   
            "Chronic food access issues")

food_access_label = food_access_label %>% filter(!is.na(food_access_year_cat))
food_access_map = food_access_map %>% filter(!is.na(food_access_year_cat))

ggplot(food_access_map, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = pct, group = group, order = order)) +
  geom_path(aes(group = group, order = order),
            colour = 'white', size = 0.1) +
  geom_text(aes(label = percent(pct, 0),
                colour = pct),
            size = 4,
            family = 'Lato', 
            data = food_access_label) +
  scale_fill_gradientn(colours = brewer.pal(9, 'OrRd'),
                       limits = c(0, 0.7)) +
  scale_colour_text(food_access_label$pct) +
  coord_equal() +
  facet_wrap(~food_access_year_cat) +
  theme_blank() +
  theme(strip.background = element_blank(),
        strip.text = element_text(family = 'Lato Light', colour = grey75K, size = 16, hjust = 0))

save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/FoodAccessCat_LZmap_CFSVA.pdf',
          height = 6, width = 6)
