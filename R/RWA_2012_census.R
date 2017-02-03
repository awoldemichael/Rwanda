# RWA_2012_census

library(haven)
library(dplyr)
library(llamar)
library(ggplot2)
library(viridis)



# read in data ------------------------------------------------------------
census = read_sav('~/Documents/USAID/Rwanda/rawdata/RWA_2012_census.sav')

source("~/GitHub/Rwanda/R/RWA_WFP_05_importGeo.R")


# clean up census data ----------------------------------------------------

# pull religion, sex, age, geo
cen = census %>% 
  dplyr::select(L01, district = L02, sector = L03,DUI, SUI,
         InclusionProbability_1_, L07, P02, P11,
        P03,  SampleWeight_Final_, cell_phone = H20,
         age = P05, resid_situation = P06)
  # dplyr::select(prov = L01, district = L02, sector = L03, 
  #        resid_area = L07, popNum = P01, relationship = P02, 
  #        sex = P03,  SampleWeight_Final_, cell_phone = H20,
  #        age = P05, resid_situation = P06)



attr(census$L01, 'labels')

cen = cen %>% 
factorize(census, 'L01', 'prov') %>% 
  factorize(census, 'L07', 'urban') %>% 
  factorize(census, 'P03', 'sex') %>% 
  factorize(census, 'P11', 'religion') %>% 
  factorize(census, 'P02', 'relationship') %>% 
  factorize(census, 'SUI', 'sector_name') %>% 
  mutate(age_grp = cut(age, seq(0,120, 5)))


# Admin3 seems confused.
codebk = data.frame(code = attr(census[['SUI']], "labels"),
                    names = names(attr(census[['SUI']], "labels")))

cen$admin3 = fct_infreq(factor(cen$SUI, levels = codebk$code,
                              labels = codebk$names))

cen = cen %>% mutate(admin3 = stringr::str_to_title(admin3))


# summarise ---------------------------------------------------------------



relig = cen %>% 
  filter(religion %in% c("Protestant", "Catholic")) %>% 
  group_by(religion, age_grp) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(religion) %>% 
  mutate(pct = n/sum(n),
    count = ifelse(religion == 'Catholic', n, -n))

# As total #s
ggplot(relig, aes(y = n, x = age_grp, fill = religion)) +
  geom_bar(stat = 'identity', alpha = 0.4, data = relig %>% filter(religion == 'Catholic')) +
  geom_bar(stat = 'identity', alpha = 0.4, data = relig %>% filter(religion == 'Protestant')) +
  coord_flip() +
  theme_xgrid(legend.position = c(0.8, 0.8))

# As percent
ggplot(relig, aes(y = pct, x = age_grp, fill = religion)) +
  geom_bar(stat = 'identity', alpha = 0.4, data = relig %>% filter(religion == 'Catholic')) +
  geom_bar(stat = 'identity', alpha = 0.4, data = relig %>% filter(religion == 'Protestant')) +
  coord_flip() +
theme_xgrid(legend.position = c(0.8, 0.8))



# Religion by sector ------------------------------------------------------
# map map map

# standardize names
sort(setdiff(unique(cen$admin3), unique(RWA_admin3$df$Sector)))
sort(setdiff(unique(RWA_admin3$df$Sector), unique(cen$admin3)))

cen  = cen %>% 
mutate(admin3_fixed = case_when(cen$admin3 %like% 'Buganda' ~ 'Ruganda',
                           cen$admin3 %like% 'Gishari' ~  'Gishali',        
                           cen$admin3 %like% 'Kanjogo' ~ 'Kanjongo',
                           cen$admin3 %like%  'Mageragere'   ~ 'Mageregere',           
                           cen$admin3 %like% 'Mimuli' ~'Mimuri',
                           cen$admin3 %like%  'Musheli' ~ 'Musheri',             
                           cen$admin3 %like% 'Nayamugali' ~ 'Nyamugari',
                           cen$admin3 %like% 'Nyakariro' ~ 'Nyakaliro',
                           cen$admin3 %like% 'Shyorongi' ~ 'Shyrongi',
                           TRUE ~ NA_character_)) %>% 
  mutate(admin3_fixed = ifelse(is.na(admin3_fixed), admin3, admin3_fixed))

sector = cen %>% 
  # filter(religion %in% c("Protestant", "Catholic")) %>% 
  group_by(religion, admin3_fixed) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(admin3_fixed) %>% 
  mutate(pct = n/sum(n),
         religion = ifelse(religion %like% "Tradit", "Animist", as.character(religion)))

religions = unique(sector$religion)

pal = brewer.pal(9, 'PuBuGn')
pal = rev(plasma(10))
limits = c(0, max(sector$pct))

for(selReligion in religions) {
  relig_map = left_join(RWA_admin3$df, sector %>% filter(religion == selReligion), by = c('Sector' = 'admin3_fixed'))
  
  relig_labels = left_join(RWA_admin3$centroids, sector %>% filter(religion == selReligion), by = c('label' = 'admin3_fixed'))
  
  relig_labels = map_colour_text(relig_labels, 'pct',  pal, limits)

  ggplot(relig_map) +
    geom_polygon(aes_string(x = 'long', y = 'lat',
                            group = 'group', order = 'order',
                            fill = 'pct')) +
    geom_path(aes_string(x = 'long', y = 'lat',
                         group = 'group', order = 'order'),
              size = 0.2,
              colour = 'white') +
    coord_equal() +
    theme_void() +
    theme(legend.position = 'bottom') +
    scale_fill_gradientn(colours = pal, limits = limits) +
    geom_text(aes_string(x = 'long', y = 'lat', label = 'percent(pct)',
                         group = 'label', colour = 'text_colour'),
              family = 'Lato',
              size = 1,
              data = relig_labels %>% filter(pct >= 0.5)) +
    scale_colour_identity() +
    ggtitle(selReligion)
  
  save_plot(filename = paste0('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_', selReligion),
            saveBoth = TRUE,
            width = 6, height = 6)
}
