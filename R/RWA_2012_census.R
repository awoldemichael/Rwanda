# RWA_2012_cen12sus

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(llamar)
library(geocen12ter)
library(ggplot2)
library(viridis)
library(data.table)



# read in data ------------------------------------------------------------

census2012 = read_sav('~/Documents/USAID/Rwanda/rawdata/RWA_2012_census.sav')
census2002 = read_sav('~/Documents/USAID/Rwanda/rawdata/RWA_2002_census.sav')

# EICV: no religion
# eicv = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2013-14_EICV4/cs_s0_s5_household.sav')
# eicv = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2013-14_EICV4/cs_s1_s2_s3_s4_s6a_s6e_s6f_person.sav')


# Import/clean geodata ----------------------------------------------------


# Import geo data
source("~/GitHub/Rwanda/R/RWA_WFP_05_importGeo.R")

# Fix Iburengerazuba == Western

RWA_admin3$df = RWA_admin3$df %>% mutate(prov = ifelse(Province == "Iburengerazuba", "Western Province", as.character(Province)))




# Reimporting sector-level data for 2002 data, since shapefile has apparently changed.
# HOWEVER-- upon looking at the data, it appears as though the admin structure was consolidated in the 2002 cen12sus to the 2006 maps. 
# baseDir_geo = '~/Documents/USAID/Rwanda/geodata/'
# proj_string = '+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' # projection to use in all maps
# 
# admin3_2002 = shp2df(baseDir =  baseDir_geo,
#                     folderName = 'Rwanda_Admin3_2002',
#                     layerName = 'Sector_Boundary_2002', 
#                     getcen12troids = TRUE,
#                     reproject = TRUE,
#                     projection = proj_string,
#                     exportData = FALSE)
# 

# clean up 2012 census data ----------------------------------------------------

# pull religion, sex, age, geo
cen12 = census2012 %>% 
  dplyr::select(L01, district = L02, sector = L03,DUI, SUI,
         InclusionProbability_1_, L07, P02, P11,
        P03,  SampleWeight_Final_, cell_phone = H20,
         age = P05, resid_situation = P06)
  # dplyr::select(prov = L01, district = L02, sector = L03, 
  #        resid_area = L07, popNum = P01, relationship = P02, 
  #        sex = P03,  SampleWeight_Final_, cell_phone = H20,
  #        age = P05, resid_situation = P06)


attr(census2012$L01, 'labels')

# Attach labels to the names, divide age into cohorts.
cen12 = cen12 %>% 
factorize(census2012, 'L01', 'prov') %>% 
  factorize(census2012, 'L07', 'urban') %>% 
  factorize(census2012, 'P03', 'sex') %>% 
  factorize(census2012, 'P11', 'religion') %>% 
  factorize(census2012, 'P02', 'relationship') %>% 
  factorize(census2012, 'SUI', 'sector_name') %>% 
  mutate(age_grp = cut(age, seq(-1, 100, by = 5)),
         age_grp2 = age_grp)


# Relabel age cohorts so it's prettier
cen12 = separate(cen12, age_grp2, into = c("age_lb", "age_ub"), sep=",") 
cen12 = cen12 %>% 
  mutate(age_lb = str_replace_all(age_lb, "\\(", ""),
         age_ub = str_replace_all(age_ub, "\\]", ""),
         age_label = paste0(as.numeric(age_lb) + 1, " - ", age_ub))




# Admin3 seems confused.
# Convert Admin 3 names.
codebk = data.frame(code = attr(census2012[['SUI']], "labels"),
                    names = names(attr(census2012[['SUI']], "labels")))

cen12$admin3 = forcats::fct_infreq(factor(cen12$SUI, levels = codebk$code,
                              labels = codebk$names))

cen12 = cen12 %>% mutate(admin3 = stringr::str_to_title(admin3))


# standardize names
sort(setdiff(unique(cen12$admin3), unique(RWA_admin3$df$Sector)))
sort(setdiff(unique(RWA_admin3$df$Sector), unique(cen12$admin3)))


# Manually correct the Sector names to match the shapefile.
cen12  = cen12 %>% 
  mutate(admin3_fixed = case_when(cen12$admin3 %like% 'Buganda' ~ 'Ruganda',
                                  cen12$admin3 %like% 'Gishari' ~  'Gishali',        
                                  cen12$admin3 %like% 'Kanjogo' ~ 'Kanjongo',
                                  cen12$admin3 %like%  'Mageragere'   ~ 'Mageregere',           
                                  cen12$admin3 %like% 'Mimuli' ~'Mimuri',
                                  cen12$admin3 %like%  'Musheli' ~ 'Musheri',             
                                  cen12$admin3 %like% 'Nayamugali' ~ 'Nyamugari',
                                  cen12$admin3 %like% 'Nyakariro' ~ 'Nyakaliro',
                                  cen12$admin3 %like% 'Shyorongi' ~ 'Shyrongi',
                                  TRUE ~ NA_character_)) %>% 
  mutate(admin3_fixed = ifelse(is.na(admin3_fixed), admin3, admin3_fixed))

sort(setdiff(unique(cen12$admin3_fixed), unique(RWA_admin3$df$Sector)))
sort(setdiff(unique(RWA_admin3$df$Sector), unique(cen12$admin3_fixed)))

# Clean up 2002 census data -----------------------------------------------

cen02  = census2002 %>% 
  mutate(District = stringr::str_to_title(novdistr),
         admin3 = stringr::str_to_title(novsect),
         DUI = as.numeric(str_replace_all(District_Code, '0', '')),
         SUI = as.numeric(str_replace(Sector_Code, '0', '')),
         age = P06,
         age_grp = cut(age, seq(-1, 100, by = 5)),
         age_grp2 = age_grp)

# Tidy up age labels
cen02 = separate(cen02, age_grp2, into = c("age_lb", "age_ub"), sep=",") 

cen02 = cen02 %>% 
  mutate(age_lb = str_replace_all(age_lb, "\\(", ""),
         age_ub = str_replace_all(age_ub, "\\]", ""),
         age_label = paste0(as.numeric(age_lb) + 1, " - ", age_ub))



sort(setdiff(unique(cen02$admin3), unique(RWA_admin3$df$Sector)))
sort(setdiff(unique(RWA_admin3$df$Sector), unique(cen02$admin3)))

# !!!! Gishari seems to be code for both Gishali (E. Prov) and Gashari (W. Prov.) 
# Manually correcting, as well as the rest of the Sector
cen02 = cen02 %>% 
  mutate(admin3_fixed = case_when(cen02$admin3 %like% 'Kabagari' ~ 'Kabagali',
                                  cen02$admin3 %like% 'Gishari' & cen02$District == 'Rwamagana' ~  'Gishali',
                                  cen02$admin3 %like% 'Gishari' & cen02$District == 'Karongi'~  'Gashari',
                                  cen02$admin3 %like%  'Mageragere'  ~ 'Mageregere',           
                                  cen02$admin3 %like% 'Mimuli' ~'Mimuri',
                                  cen02$admin3 %like%  'Musheli' ~ 'Musheri',             
                                  cen02$admin3 %like% 'Nyakariro' ~ 'Nyakaliro',
                                  cen02$admin3 %like% 'Nyakiliba' ~ 'Nyakiriba',
                                  cen02$admin3 %like% 'Nyamugali' ~ 'Nyamugari',
                                  cen02$admin3 %like% 'Rugalika' ~ 'Rugarika',
                                  cen02$admin3 %like% 'Rusarabuge' ~ 'Rusarabuye',
                                  cen02$admin3 %like% 'Shyorongi' ~ 'Shyrongi',
                                  TRUE ~ NA_character_)) %>% 
  mutate(prov = case_when(cen02$novprov %like% "PROVINCE DE L'EST" ~ 'Eastern Province',
                                  cen02$novprov %like% "PROVINCE DE L'OUEST" ~  'Western Province',
                                  cen02$novprov %like%  'PROVINCE DU NORD'   ~ 'Northern Province',           
                                  cen02$novprov %like% 'PROVINCE DU SUD' ~'Southern Province',
                                  cen02$novprov %like%  'VILLE DE KIGALI' ~ 'Kigali City',             
                                  TRUE ~ NA_character_)) %>% 
  mutate(admin3_fixed = ifelse(is.na(admin3_fixed), admin3, admin3_fixed)) %>% 
  factorize(cen02, 'P12', 'religion') %>% 
  mutate( # to english
    religion = ifelse(religion %like% "Prot", "Protestant", 
                      ifelse(religion %like% 'Cath', "Catholic", as.character(religion))))

sort(setdiff(unique(cen02$admin3_fixed), unique(RWA_admin3$df$Sector)))
sort(setdiff(unique(RWA_admin3$df$Sector), unique(cen02$admin3_fixed)))


# calculate percent of religion, over time --------------------------------
# Since this is a slice of the census and not the actual census, going with the published numbers in the report.
# pct = cen12 %>% 
#   group_by(religion, SampleWeight_Final_) %>% 
#   summarise(n = n()) %>%
#   mutate(count = n*SampleWeight_Final_) %>% 
#   ungroup() %>% 
#   mutate(pct = n/sum(n))


# Pop chart by religion ---------------------------------------------------------------

relig = cen12 %>% 
  filter(religion %in% c("Protestant", "Catholic"),
         !is.na(age_grp)) %>% 
  group_by(religion, age_label, age_grp) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(religion) %>% 
  mutate(order = dense_rank(age_grp)) %>% 
  mutate(pct = n/sum(n))

write.csv(relig, '~/Documents/USAID/Rwanda/processeddata/relig_byAge2012.csv')

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



# MAP: Religion by sector ------------------------------------------------------
# map map map

# merge census data together ----------------------------------------------
cen12 = cen12 %>% 
  select(religion, admin3_fixed, prov, weight = SampleWeight_Final_, DUI, SUI) %>% 
  mutate(year = 2012)

cen02 = cen02 %>% 
  select(religion, admin3_fixed, prov, District, DUI, SUI, weight = Weight) %>% 
  mutate(year = 2002)


sector12 = cen12 %>% 
  group_by(religion, admin3_fixed, prov, DUI, SUI, weight) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(admin3_fixed, prov) %>% 
  mutate(pct = n/sum(n),
         pop = n * weight,
         totalPop = sum(n) * weight, # At sector level
         religion = ifelse(religion %like% "Tradit", "Animist", as.character(religion)),
         year = 2012)


sector02 = cen02 %>% 
  group_by(religion, admin3_fixed, prov, District, DUI, SUI, weight) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(admin3_fixed, prov) %>% 
  mutate(pct = n/sum(n),
         pop = n * weight,
         totalPop = sum(n) * weight, # At sector level
         religion = ifelse(religion %like% "Tradit", "Animist", as.character(religion)),
         year = 2002)

sectors = bind_rows(sector02, sector12) %>% 
  filter(religion %in% c("Catholic", "Protestant"))



pal = RColorBrewer::brewer.pal(9, 'PuBuGn')
limits = c(0, max(sectors$pct))
popLimits = log10(c(min(sectors$totalPop), max(sectors$totalPop)))

popPal = rev(brewer.pal(11, "PuOr")[1:6])

religions = unique(sectors$religion)

for(selReligion in religions) {

  for (selYear in unique(sectors$year)){
    
    # Merging based on IDs, which seem to be more consistent.
    relig_map = left_join(RWA_admin3$df, sectors %>% filter(religion == selReligion, year == selYear), by = c('Sect_ID' = 'SUI', 'prov' = 'prov', 'Dist_ID' = 'DUI'))
    
    relig_labels = left_join(RWA_admin3$centroids, sectors %>% filter(religion == selReligion, year == selYear), by = c('label' = 'admin3_fixed'))
    
    relig_labels = map_colour_text(relig_labels, 'pct',  pal, limits)
    
    p = ggplot(relig_map) +
      geom_polygon(aes(x = long, y = lat, group = as.numeric(group), order = order, fill = pct)) +
      geom_path(aes_string(x = 'long', y = 'lat',
                           group = 'as.numeric(group)', order = 'order'),
                size = 0.05,
                colour = 'white') +
      coord_equal() +
      theme_void() +
      theme(legend.position = 'bottom') +
      scale_fill_gradientn(colours = pal, limits = limits) +
      geom_text(aes_string(x = 'long', y = 'lat', label = paste0('percent(pct,0)'),
                           group = 'label', colour = 'text_colour'),
                family = 'Lato',
                size = 1,
                data = relig_labels %>% filter(pct >= 0.75)) +
      scale_colour_identity() +
      ggtitle(selReligion)
    
    save_plot(filename = paste0('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_', selReligion, '_', selYear),
              saveBoth = TRUE,
              width = 6, height = 6)
  
    p = ggplot(relig_map) +
      geom_polygon(aes(x = long, y = lat, group = as.numeric(group), order = order, fill = log10(totalPop))) +
      geom_path(aes_string(x = 'long', y = 'lat',
                           group = 'as.numeric(group)', order = 'order'),
                size = 0.05,
                colour = 'white') +
      coord_equal() +
      theme_void() +
      theme(legend.position = 'bottom') +
      scale_fill_gradientn(colours = popPal, limits = popLimits) +
      scale_colour_identity() +
      ggtitle('population')
    
    save_plot(filename = paste0('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_population', selYear),
              saveBoth = TRUE,
              width = 6, height = 6)
    
    }
}