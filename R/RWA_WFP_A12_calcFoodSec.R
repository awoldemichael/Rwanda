
# setup -------------------------------------------------------------------
setwd('~/GitHub/Rwanda/R/')
exportDir = '~/Documents/USAID/Rwanda/processeddata/'  


# source DHS stats --------------------------------------------------------


# source CFSVA data -------------------------------------------------------
source('RWA_WFP_run2015.R')


food_access_lz = hh %>% 
  group_by(livelihood_zone, food_access_year_cat) %>% 
  summarise(foodAccess_N = n()) %>% 
  ungroup() %>% 
  group_by(livelihood_zone) %>% 
  mutate(foodAccess_pct = foodAccess_N / sum(foodAccess_N))


food_access_dist = hh %>% 
  group_by(admin2, food_access_year_cat) %>% 
  summarise(foodAccess_N = n()) %>% 
  ungroup() %>% 
  group_by(admin2) %>% 
  mutate(foodAccess_pct = foodAccess_N / sum(foodAccess_N))



cari_lz = hh %>% 
  group_by(livelihood_zone, CARI_cat) %>% 
  summarise(CARI_N = n()) %>% 
  ungroup() %>% 
  group_by(livelihood_zone) %>% 
  mutate(CARI_pct = CARI_N / sum(CARI_N))


cari_dist = hh %>% 
  group_by(admin2, CARI_cat) %>% 
  summarise(CARI_N = n()) %>% 
  ungroup() %>% 
  group_by(admin2) %>% 
  mutate(CARI_pct = CARI_N / sum(CARI_N))

dist = full_join(cari_dist, food_access_dist)
lz = full_join(cari_lz, food_access_lz)



# export ------------------------------------------------------------------

write.csv(dist, paste0(exportDir, 'RWA_CFSVA_2015_foodSec_district.csv'))
write.csv(lz, paste0(exportDir, 'RWA_CFSVA_2015_foodSec_lz.csv'))
