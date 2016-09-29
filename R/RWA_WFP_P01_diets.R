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
staples_weight = 2
oil_weight =  0.5
pulse_weight =  3
sugar_weight =  0.5 
veg_weight =  1
milk_weight =  4
meat_weight = 4 
fruit_weight =  1 


staples_weight = 1
oil_weight =  1
pulse_weight =  1
sugar_weight =  1 
veg_weight =  1
milk_weight =  1
meat_weight = 1 
fruit_weight =  1 
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
ggplot(rel_fcs_heat) +
  geom_tile(aes(x = food, y = regionName, fill = rel_mean), 
            color = 'white', size = 1) +
  scale_fill_gradientn(colours = PlBl, 
                       limits = c(-8.2,8.2)) +
  # geom_text(aes(y = food, x = regionName, label = round(rel_mean,1)), size = 4) +
  ggtitle('FCS, relative to the national average') +
  theme_xylab() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 18, family = 'Segoe UI', hjust = 0, color = grey60K))


