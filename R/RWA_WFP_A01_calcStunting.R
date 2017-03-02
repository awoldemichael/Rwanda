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


# Dependencies ------------------------------------------------------------
# setwd('~/GitHub/Rwanda/R')
# 
# # Load setup functions / vars ---------------------------------------------
# source('RWA_WFP_00_setup.R')
# 
# 
# # IMPORT ------------------------------------------------------------------
# 
# 
# # Import / clean individual children-level data --------------------------------------------
# source('RWA_WFP_01_importKids.R')
# 
# # Import / clean individual women's data --------------------------------------------
# source('RWA_WFP_02_importMother.R')
# 
# # Import / clean hh-level data --------------------------------------------
# source('RWA_WFP_03_importHH.R')
# 
# 
# # Import shapefiles for choropleths ---------------------------------------
# source('RWA_WFP_05_importGeo.R')

# Calculate estimates at the Admin0 level ---------------------------------

stunting_admin0_cfsva = calcPtEst(ch, 'isStunted', 
                                  psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

stuntingZ_admin0_cfsva = calcPtEst(ch, 'stuntingZ', 
                                  psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

# Calculate estimates at the Admin1 level ---------------------------------

stunting_admin1_cfsva = calcPtEst(ch, 'isStunted', by_var = 'admin1',
                                  psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

stuntingZ_admin1_cfsva = calcPtEst(ch, 'stuntingZ', by_var = 'admin1',
                                   psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

# Calculate estimates at the Admin2 level ---------------------------------

stunting_admin2_cfsva = calcPtEst(ch, 'isStunted', by_var = 'admin2',
                            psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

stuntingZ_admin2_cfsva = calcPtEst(ch, 'stuntingZ', by_var = 'admin2',
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
         stunting_cfsva = unweighted_avg,
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
  theme_xygrid()

# Calculate estimates for livelihood zones ---------------------------------

stunting_lz_cfsva = calcPtEst(ch, 'isStunted', by_var = 'livelihood_zone',
                              psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

stuntingZ_lz_cfsva = calcPtEst(ch, 'stuntingZ', by_var = 'livelihood_zone',
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
         stunting_cfsva = unweighted_avg,
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
  theme_xygrid()


# Deciding factor levels --------------------------------------------------

# Comparing avg. % stunted and average stunting score to average:

# -- stunting score-- 
# Nat'l.
# stuntingZ         se    N
# 1 -1.612125 0.02841725 3810

# Closest livelihood zones:
# 6  Central-Northern Highland Irish Potato, Beans and Vegetable Zone -1.611422 0.10197842 213
# 7                                         Eastern Agropastoral Zone -1.626921 0.11836627 174
# 8                                             Lake Kivu Coffee Zone -1.662574 0.05873020 492

# -- % stunted --
# isStunted         se    N
# 1 0.3670844 0.01002844 3810

# 6                                         Eastern Agropastoral Zone 0.3910140 0.04925721
# 7  Central-Northern Highland Irish Potato, Beans and Vegetable Zone 0.3799819 0.03584594
# 8                                             Lake Kivu Coffee Zone 0.3688482 0.02093194

# Either Central-Northern Highland or Lake Kivu both seem like reasonable bases.  Lake Kivu has larger sample size.

# Comparing to the DHS:

# nat'l = 37.9%
# 5    Central-Northern Highland Irish 0.3847564 0.03945387
# 6   Central Plateau Cassava and Coff 0.3843477 0.01874473
# 7                   Bugesera Cassava 0.3776399 0.04181264
# 8                   Lake Kivu Coffee 0.3578203 0.02753781

# So... Central-Northern maybe best compromise?

stuntingZ_lz_cfsva %>% 
  mutate(diff = (avg - -1.612125)/-1.612125) %>% 
  select(livelihood_zone, diff)

stunting_lz_cfsva %>% 
  mutate(diff = (avg - 0.3670844)/0.3670844) %>% 
  select(livelihood_zone, diff)

stunting_lz_dhs %>% 
  arrange(desc(b)) %>% 
  mutate(diff = (b - 0.379)/0.379) %>% 
  select(X, diff)

#                                                                     z-score     stunted% (CFSVA)  stunted% (DHS)
#  Central-Northern Highland Irish Potato, Beans and Vegetable Zone  0.035135026  -0.0004359292    0.015188417
#                                             Lake Kivu Coffee Zone  0.004804891   0.0312934236   -0.055883008

# Base for Districts ------------------------------------------------------

stuntingZ_admin2_cfsva %>% 
  mutate(diff = (avg - -1.612125)/-1.612125) %>% 
  select(admin2, diff)

# -- stunting score-- 
# Nat'l.
# stuntingZ         se    N
# 1 -1.612125 0.02841725 3810

# Closest livelihood zones:
# 13  Rwamagana -1.5019323 0.19625335  142
# 14    Musanze -1.5043064 0.21840844  131
# 15      Ngoma -1.5321657 0.12158794  143
# 16     Rusizi -1.5642673 0.08652256  163
# 17     Nyanza -1.6396471 0.07696595  132
# 18    Gicumbi -1.6443207 0.12425222  122
# 19  Nyagatare -1.6477930 0.13039699  147
# 20 Nyamasheke -1.6849830 0.12827976  162

# -- % stunted --
# isStunted         se    N
# 1 0.3670844 0.01002844 3810

# Closest districtss:
stunting_admin2_cfsva %>% 
  mutate(diff = (avg - 0.3670844)/0.3670844) %>% 
  select(admin2, diff)

# 11    Gicumbi 0.3979798 0.03968020 122 
# 12  Nyagatare 0.3893289 0.05354429 147 
# 13     Rusizi 0.3719445 0.02730158 163 
# 14    Musanze 0.3489746 0.07108791 131 

# so... Rusizi?

# Comparing to the DHS:

# nat'l = 37.9%

stunting_admin2_dhs %>% 
  mutate(diff = (b - 0.379)/0.379) %>% 
  select(admin2, diff)

# 13      Ngoma 0.4093115
# 14    Muhanga 0.4089531
# 15   Bugesera 0.3985176
# 16  Nyagatare 0.3892319
# 17    Musanze 0.3800318
# 18    Gicumbi 0.3709482
# 19   Gisagara 0.3606789
# 20     Rusizi 0.3531730

# % diff from avg. value:
#     District    avg%        Z-score     DHS_avg%
#     Gicumbi     0.08416435  0.01997095 -0.021244934
# *   Nyagatare   0.06059791  0.02212482  0.026997230
# *   Rusizi      0.01323971 -0.02968609 -0.068145145
# **  Musanze    -0.04933422 -0.06687980  0.002722322
#     Nyanza     -0.07730967  0.01707197 -0.099184934
#     Nyamasheke -0.07829360  0.04519376 -0.096727361
#     Ngoma      -0.08618574 -0.04959870  0.079977652


# Base for provinces ------------------------------------------------------
# North!

# stunting %
# 2    Northern 0.3888858 0.03046762 
# 3     Eastern 0.3506154 0.02088620 

# stunting score
# 3     Eastern -1.558357 0.05364396 
# 4    Northern -1.655170 0.08967943 
