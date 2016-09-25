# Rwanda stunting analysis -----------------------------------------
#
# RWA_WFP_A01_calcStunting.R
#
# Script to calculate point estimate data for stunting data for Rwanda from the CFSVA dataset
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


# Calculate estimates at the Admin2 level ---------------------------------

stunting_admin2_cfsva = calcPtEst(ch, 'isStunted', by_var = 'admin2',
                            psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')



# import DHS stunting admin2 results ---------------------------------------------
stunting_admin2_dhs = data.frame(read.table('~/GitHub/Rwanda/Export/stunting_dist.txt')) 

# separate district column
stunting_admin2_dhs = stunting_admin2_dhs %>% 
  mutate(name = row.names(stunting_admin2_dhs)) %>% 
  separate(name, into = c('x', 'admin2'), by = ':')

# -- merge DHS and CFSVA results --
stunting_admin2 = full_join(stunting_admin2_dhs, stunting_admin2_cfsva, by = 'admin2') %>% 
  rename(stunting_dhs = b,
         stunting_cfsva = isStunted,
         lb_dhs = ll,
         ub_dhs = ul,
         lb_cfsva = lb,
         ub_cfsva = ub)

# -- Plot difference in CFSVA and DHS --
ggplot(stunting_admin2, aes(x = stunting_dhs, y = stunting_cfsva)) + 
  geom_abline(slope = 1, intercept = 0, colour = 'red') +
  geom_rect(aes(xmin = lb_dhs, xmax = ub_dhs, ymin = lb_cfsva, ymax = ub_cfsva),
            alpha = 0.2) +
  geom_segment(aes(xend = stunting_dhs, y = lb_cfsva, yend = ub_cfsva), alpha = 0.3) +
  geom_segment(aes(yend = stunting_cfsva, x = lb_dhs, xend = ub_dhs), alpha = 0.3) +
  geom_point(size = 3, colour = 'dodgerblue') + 
  coord_equal() +
  xlab('DHS') +
  ylab('CFSVA') +
  ggtitle('districts') +
  theme_xygridlight()

# Calculate estimates for livelihood zones ---------------------------------

stunting_lz_cfsva = calcPtEst(ch, 'isStunted', by_var = 'livelihood_zone',
                              psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

# import DHS stunting livelihood zone results ---------------------------------------------
stunting_lz_dhs = data.frame(read.delim('~/GitHub/Rwanda/Export/stunting_lvd.txt'))

# Fix merging name issues for livelihood zones
# %like% requires library(data.table)
stunting_lz_dhs = stunting_lz_dhs %>% 
  mutate(livelihood_zone = case_when(stunting_lz_dhs$X %like% 'Tea' ~ 'West Congo-Nile Crest Tea Zone',
                   stunting_lz_dhs$X %like% 'Wheat' ~ 'Northern Highland Beans and Wheat Zone',                          
                   stunting_lz_dhs$X %like% 'Eastern Congo' ~ 'East Congo-Nile Highland Subsistence Farming Zone',
                   stunting_lz_dhs$X %like% 'Volcanic' ~ 'Northwest Volcanic Irish Potato Zone',                            
                   stunting_lz_dhs$X %like% 'Mixed' ~ 'Eastern Plateau Mixed Agriculture Zone',
                   stunting_lz_dhs$X %like% 'Eastern Ag' ~ 'Eastern Agropastoral Zone',                                       
                   stunting_lz_dhs$X %like% 'Central-Northern' ~ 'Central-Northern Highland Irish Potato, Beans and Vegetable Zone',
                   stunting_lz_dhs$X %like% 'Kivu' ~ 'Lake Kivu Coffee Zone',
                   stunting_lz_dhs$X %like% 'Banana' ~ 'Southeastern Plateau Banana Zone',
                   stunting_lz_dhs$X %like% 'Bugesera' ~ 'Bugesera Cassava Zone',                                           
                   stunting_lz_dhs$X %like% 'Central Plateau' ~ 'Central Plateau Cassava and Coffee Zone',
                   stunting_lz_dhs$X %like% 'Semi-Arid' ~ 'Eastern Semi-Arid Agropastoral Zone',                             
                   stunting_lz_dhs$X %like% 'Urban' ~ 'Kigali city',
                   TRUE ~ NA_character_))

# -- Merge DHS and CFSVA results -- 
stunting_lz = full_join(stunting_lz_dhs, stunting_lz_cfsva, by = 'livelihood_zone') %>% 
  rename(stunting_dhs = b,
         stunting_cfsva = isStunted,
         lb_dhs = ll,
         ub_dhs = ul,
         lb_cfsva = lb,
         ub_cfsva = ub)

# -- Plot difference b/w DHS and CFSVA --
ggplot(stunting_lz, aes(x = stunting_dhs, y = stunting_cfsva)) + 
  geom_abline(slope = 1, intercept = 0, colour = 'red') +
  geom_rect(aes(xmin = lb_dhs, xmax = ub_dhs, ymin = lb_cfsva, ymax = ub_cfsva),
            alpha = 0.2) +
  geom_segment(aes(xend = stunting_dhs, y = lb_cfsva, yend = ub_cfsva), alpha = 0.3) +
  geom_segment(aes(yend = stunting_cfsva, x = lb_dhs, xend = ub_dhs), alpha = 0.3) +
  geom_point(size = 3, colour = 'dodgerblue') + 
  coord_equal()  +
  xlab('DHS') +
  ylab('CFSVA') +
  ggtitle('livelihood zones') +
  theme_xygridlight()
