# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_07_importHH2012.R: import household-level data from 2012 dataset
#
# Script to pull stunting data and associated household- or
# child-level data for Rwanda from the CFSVA dataset
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 14 November 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

# Import in the raw data --------------------------------------------------
# Raw data is contained in three files:
# children's data: 'cfsva-2015-child-DB- annex.sav'
# household-level data:  'cfsva-2015-master-DB- annex.sav'
# women's data: 'cfsva-2015-mother-DB- annex.sav'


# Previous dependencies ---------------------------------------------------
setwd('~/GitHub/Rwanda/R/')

source('RWA_WFP_00_setup.R')
source('RWA_WFP_04_importKids2012.R')
source('RWA_WFP_06_importMoms2012.R')

hh2012_raw = read_sav(paste0(baseDir, 'RW_2012_CFSVA/cfsvans-2012- household-v01.sav'))


# 2009 data are unprocessed.  Also doesn't include Kigali / urban areas.
# ch2009 = read_sav(paste0(baseDir, 'RW_2009_CFSVA/Section 13 enfants.sav'))







# select variables --------------------------------------------------------

hh2012 =  hh2012_raw %>% 
  select(
    # -- basics --
    hh_id,
    int_date = inter_date,
    month = Interview_Cat,
    weight = FINAL_norm_weight,
    strata_ID,
    
    
    wb_site = LWH, # World Bank Intevention Sites
    
    # -- demographics --
    hh_size = HH_size, # same as hh_size_computed
    crowding = crowding_index,
    Q102, # female headed
    head_age  = Q103,
    
    # -- education --
    Q104, # literate head
    Q105, # education head
    
    # -- WASH --
    #Q205_1, # type of toilet
    improved_water,
    impr_toilet = improved_toilet,
    share_toilet = Q206, # shared toilet
    # Q209_1, # water source
    Q215_1, # distance from water
    Q216, # water treatment
    
    # -- geography --
    p_code, d_code, s_code, v_code, # province, district
    Urban, Urban_NEW, final_urban, int_urban, # SOOO I have no idea which of these is right.  Using Urban_NEW since it is the one w/ only 2 classes of urban/rural, but I have no idea if that's correct. There's no pattern to the 4 "urban" variables.
    agro_climat, 
    fews_code,
    altitude = Q_altitude, # village height in m
    Q_sfi, # soil fertility index
    Q_se, # soil erosion
    suitability, # slope suitability
    
    # -- food --
    # CSI_cat = CSI_categories,  
    months_food_access = Months_FA,
    
    FCS, FCS_category,
    
    starch_days = Starches, 
    meat_days = Protein, 
    milk_days = Milk,
    oil_days = Oil,
    veg_days = Vegs,
    fruit_days = Fruits,
    pulse_days = Pulses,
    sugar_days = Sugar,
    
    # -- wealth --
    wealth_idx_num = NFAC1_1,  # GWI is the similar as NFAC1_1, but terciles instead of quintiles.
    pc_exp_year,
    pc_income_year, # same as pc_income (but yearly, not monthly) but different than pc_income_NEW Unclear how or why.
    
    # -- livelihoods --
    # livelihood_NEW, # SO: this contains similar info, but the categories don't align w/ the pc_income brackets they defined.  So using livelihood
    livelihood, 
    num_jobs = Q301, #! check same def
    
    # -- ag --
    Land_cat,
    Q401_1, # farm land
    Q401_2, # size land
    hh_garden = Q410,
    TLU_cat,
    Livestock_Ownership, # maybe TLUs? Has NAs.  :(  Could combine w/ own_livestock
    
    
    growing_beans = beans_YN,
    growing_maize = maize_YN,
    growing_s_potato = sw_potato_YN,
    growing_cassava = cassava_YN,
    growing_i_potato = ir_potato_YN,
    growing_sorghum = sorghum_YN,
    
    # -- finances --
    # Q1202, Q1204, QA1206+#food_assistance + financial_assistance + ag_assistance
    food_assistance = food_assitance,
    financial_assistance = financial_assitance,
    ag_assistance = Ag_assitance,
    
    
    # -- connectivity --
    Q_hospital,
    road_dist_cat = Q_roads_km,
    market_dist_cat = Q_market
  )



# merge everything together -----------------------------------------------
# checking everything is unqiue
length(unique(hh2012$hh_id)) == nrow(hh2012)
length(unique(females2012$hh_id)) == nrow(females2012) # duplicative, but maybe okay.

# canary column for children: sex of child sum(is.na(ch2012$Q202_09))
# canary column for women: sum(is.na(females2012$moth_ID))
# canary column for men: sum(is.na(hh2012$Q_altitude))

# test merge:
# k = left_join(ch2012, females2012, 
#               by = c("hh_id", "weight", "mother_id", "p_code", 
#                      "d_code", "s_code", "v_code", "t_code", "final_urban", "fews_code"))
# 
# if(nrow(k) != nrow(ch2012)) {
#   print('ch2012 / females2012 merge fail. Redundant/nondescript ids')
# } else if (sum(is.na(k$moth_ID)) > 0){
#   print('ch2012 / females2012 merge fail: mother NAs')
# } else if (sum(is.na(k$Q202_09)) > 0){
#   print('ch2012 / females2012 merge fail: mother NAs')
# }

# Lots of mothers that don't merge.  Porque?  Motherless children, or something else?
# 875 problems initially.
# So there are 282 children, all in Kigali, with hh_id = 0, and no 
# 349 children have missing info from their mom (code 88), or mother is dead (code 99) :(. Assuming code 77 is also a no-go.
# strata_id is missing in hh and children's, but set to 0 in females.
# rest seem to be code mismatches.  Not sure I can fix.


# k = left_join(ch2012, hh2012, 
#               by = c("hh_id", "weight", "strata_ID", "p_code", "d_code", "s_code", 
#                      "v_code", "final_urban", "hh_size", 
#                      # "fews_code",
#                      "livelihood", "Q102", "Q105", "Q215_1", "impr_toilet", "FCS", 
#                      "head_age"))
# 308 total.
# 19 fews code mis-match.  Some Kigali kids labeled as being in Eastern Agropastoral Zone
# 282 children with hhid==0 (wtf?)
# 7 children w/ hh_id 1 but different sampling weights and interview dates, neither of which matches the one in hh.
# Not sure there's anything to be done...

# nrow(k) == nrow(ch2012)
# sum(is.na(k$Q202_09)) # kids remain intact.
# sum(is.na(k$Q_altitude))
# glimpse(k %>% filter(hh_id !=0, is.na(Q_altitude)))
# # 
# t(hh2012 %>% filter(hh_id == 7899) %>% select(one_of(c("hh_id", "weight", "strata_ID", "p_code", "d_code", "s_code", 
#   "v_code", "final_urban", "fews_code", "hh_size", 
#   "livelihood", "Q102", "Q105", "Q215_1", "impr_toilet", "FCS"))))

# merge for real. ---------------------------------------------------------
ch_hh2012 = left_join(ch2012, hh2012, 
                      by = c("hh_id", "weight", "strata_ID", "p_code", "d_code", "s_code", 
                             "v_code", "final_urban", "hh_size", "head_age",
                             "livelihood", "Q102", "Q105", "Q215_1", "impr_toilet", "FCS"))


ch_hh2012 = left_join(ch_hh2012, females2012, by = c("hh_id", "weight", "mother_id", "p_code", 
                                                     "d_code", "s_code", "v_code", "t_code", "final_urban"))

# clean ch-hh vars --------------------------------------------------------------
# Defining illness shocks as being: 
ill_shk_cats = 11:14 # serious illness or death in family

# TLU coefs
camelVal 	= 0.70
cattleVal 	= 0.50
pigVal 		= 0.20
sheepVal 	= 0.10
horsesVal 	= 0.50
mulesVal 	= 0.60
assesVal 	= 0.30
chxVal 		= 0.01
duckVal = 0.03
rabbitVal = 0.02


ch_hh2012 = ch_hh2012 %>%  
  mutate(
    # -- fix weirdness / create new var --
    
    # -- wealth --
    log_pcexp = log10(pc_exp_year/12),
    
    # -- ages --
    head_age_sq = head_age^2,
    mother_age_sq = mother_age^2,
    
    # -- WASH --
    impr_unshared_toilet = case_when(ch_hh2012$impr_toilet == 0 ~ 0,
                                     (ch_hh2012$impr_toilet == 1 & ch_hh2012$share_toilet == 0) ~ 1, # improved + unshared
                                     (ch_hh2012$impr_toilet == 1 & ch_hh2012$share_toilet == 1) ~ 0, # improved + shared
                                     TRUE ~ NA_real_),
    
    impr_water_30min = case_when(ch_hh2012$impr_water == 0 ~ 0, # unimproved
                                   (ch_hh2012$impr_water == 1 & ch_hh2012$Q215_1 <= 30) ~ 1, # improved + < 30 min. away
                                   (ch_hh2012$impr_water == 1 & ch_hh2012$Q215_1 > 30) ~ 0, # improved + > 30 min. away
                                   TRUE ~ NA_real_),
    
    # -- shocks --
    shock_illness = case_when(ch_hh2012$Q1105_1_1 %in% ill_shk_cats ~ 1, # primary shock
                              ch_hh2012$Q1105_2_1 %in% ill_shk_cats ~ 1, # secondary shock
                              (!(ch_hh2012$Q1105_1_1 %in% ill_shk_cats) &
                                 !(ch_hh2012$Q1105_2_1 %in% ill_shk_cats)) ~ 0, # not shocked.
                              TRUE ~ NA_real_
    ), # no NAs in data
    
    shock_drought = case_when(ch_hh2012$Q1105_1_1 == 1 ~ 1, # primary shock
                              ch_hh2012$Q1105_2_1 == 1 ~ 1, # secondary shock
                              (ch_hh2012$Q1105_1_1 != 1 &
                                 ch_hh2012$Q1105_2_1 != 1) ~ 0, # not shocked.
                              TRUE ~ NA_real_
    ), # no NAs in data
    
    # -- TLUs --
    # Create TLU (based on values from http://www.lrrd.org/lrrd18/8/chil18117.htm)
    # Notes: Sheep includes sheep and goats
    # Horse includes all draught animals (donkey, horse, bullock)
    # chxTLU includes all small animals (chicken, fowl, etc).  Assuming rabbits fall into this category.
    
    total_chicken = ifelse(QA412_1 == -1, 0, QA412_1),
    total_duck = ifelse(QB412_1 == -1, 0, QB412_1),
    total_goat = ifelse(QC412_1 == -1, 0, QC412_1),
    total_sheep = ifelse(QD412_1 == -1, 0, QD412_1),
    total_pig = ifelse(QE412_1 == -1, 0, QE412_1),
    total_cow = ifelse(QF412_1 == -1, 0, QF412_1),
    total_rabbit = ifelse(QG412_1 == -1, 0, QG412_1),
    
    tlucattle = (total_cow) * cattleVal,	  
    tlusheep 	= (total_sheep + total_goat) * sheepVal,
    # tluhorses = (total_donkey + total_mule) * horsesVal, # no horses in data
    tlupig 	= (total_pig) * pigVal,
    tluchx 	= (total_chicken) * chxVal + total_duck * duckVal + total_rabbit * rabbitVal,
    
    TLU = tlucattle + tlusheep + tlupig + tluchx, # + tluhorses
    
    health_less_60min = case_when(ch_hh2012$Q_hospital == -1 ~ 0, # Assuming -1 means no hospital access? Only 25 obs.
                                  ch_hh2012$Q_hospital <= 60 ~ 1,
                                  ch_hh2012$Q_hospital > 60 ~ 0,
                                  TRUE ~ NA_real_),
    low_birthwt = case_when(ch_hh2012$Q202_13 %in% c(1, 2, 3) ~ 0, # very large, larger than normal, normal
                            ch_hh2012$Q202_13 %in% c(4, 5) ~ 1, # very large, larger than normal, normal
                            TRUE ~ NA_real_),
    
    # -- regroup --
    numWomen_18plus = numWomen_18_59 + numWomen_60plus,
    
    # reclassifying CSI to match 2015 breaks.
    CSI_cat = case_when(ch_hh2012$CSI == 0 ~ 0, # no food insecurity
                        (ch_hh2012$CSI > 0 & ch_hh2012$CSI < 10) ~ 1, # 'low' coping: few changes to diet
                        (ch_hh2012$CSI > 9 & ch_hh2012$CSI < 18) ~ 2, # 'medium' coping
                        (ch_hh2012$CSI > 17) ~ 3, # 'high' coping: most changes to diet
                        TRUE ~ NA_real_),
    CSI_cat = 
      factor(CSI_cat,
             levels = 0:3,
             labels = c('No coping strategies',
                        'Low coping',
                        'Medium coping',
                        'High coping'
             )),
    
    # reclassifying land to match 2015.
    land_size_cat = case_when(ch_hh2012$Land_cat == 0 ~ 0, # land size reported as 'none'
                              ch_hh2012$Land_cat == 1 ~ 1, # land size = 0.00 - 0.10 ha
                              ch_hh2012$Land_cat == 2 ~ 1, # land size = 0.10 - 0.19 ha
                              ch_hh2012$Land_cat == 3 ~ 2, # land size = 0.20 - 0.49 ha
                              ch_hh2012$Land_cat == 4 ~ 2, # land size = 0.50 - 0.99 ha
                              ch_hh2012$Land_cat == 5 ~ 3, # land size > 1 ha
                              TRUE ~ NA_real_),
    land_size_cat = 
      factor(land_size_cat,
             levels = 0:3,
             labels = c('no land', 
                        '0.00 - 0.19 ha',
                        '0.20 - 0.99 ha',
                        'more than 1.00 ha')),
    
    # guessing on regrouping hh occupations to align w/ 2015 cats.  unskilled/skilled difficult part; based partly on avg. stunting rate (after grouping by category)
    hh_occup_cat = case_when(ch_hh2012$livelihood == 1 ~ 1, # low income ag
                             ch_hh2012$livelihood == 2 ~ 4, # agro-pastoralists
                             ch_hh2012$livelihood == 3 ~ 3, # day laborer
                             ch_hh2012$livelihood == 4 ~ 3, # ag workers = group with day laborers? 
                             ch_hh2012$livelihood == 5 ~ 5, # informal sales = unskilled?
                             ch_hh2012$livelihood == 6 ~ 6, # business = skilled
                             ch_hh2012$livelihood == 7 ~ 6, # sellers  = trade (?) = skilled
                             ch_hh2012$livelihood == 8 ~ 6, # artisans = skilled
                             ch_hh2012$livelihood == 9 ~ 5, # marginal livelihood = unskilled
                             ch_hh2012$livelihood == 10 ~ 2, # high income ag
                             ch_hh2012$livelihood == 11 ~ 1, # assuming the no income reported are low income agriculturalists
                             TRUE ~ NA_real_),
    hh_occup_cat = 
      factor(hh_occup_cat,
             levels = 1:6,
             labels = c('Low-income agriculturalists',
                        'Medium/high income agriculturalists',
                        'Agricultural daily labour',
                        'Agro-pastoralists',
                        'Unskilled', 
                        'Skilled'
             ))
  ) %>% 
  # -- location --
  factorize(hh2012_raw, 'Urban_NEW', 'rural_cat') %>% 
  factorize(hh2012_raw, 'p_code', 'admin1') %>% 
  factorize(hh2012_raw, 'd_code', 'admin2') %>% 
  factorize(hh2012_raw, 's_code', 'admin3') %>% 
  factorize(hh2012_raw, 'v_code', 'admin4') %>% 
  factorize(hh2012_raw, 'fews_code', 'livelihood_zone') %>% 
  # -- demographics --
  factorize(ch2012_raw, 'Q202_09', 'sex') %>% 
  factorize(ch2012_raw, 'Q102', 'fem_head') %>% 
  # -- education --
  factorize(ch2012_raw, 'Q105', 'head_education_cat') %>% 
  factorize(females2012_raw, 'Q102_04', 'mother_education') %>% 
  # -- Admins --
  # Fix Kigali City (sigh)
  mutate(livelihood_zone = ifelse(livelihood_zone %like% 'Kigali', 'Kigali city',
                                  as.character(livelihood_zone)),
         admin1 = stringr::str_to_title(admin1)
  )

# -- deciled wealth --
ch_hh2012$month_pcexpend_decile = ntile(ch_hh2012$pc_exp_year, 10)
ch_hh2012$month_pcincome_decile = ntile(ch_hh2012$pc_income_year, 10)


# -- education --
ch_hh2012$head_education_cat = forcats::fct_collapse(ch_hh2012$head_education_cat,
                                                     no_school = 'No School',
                                                     some_prim = 'Some Primary',
                                                     prim_vocational = c('Vocational School', 'Completed Primary'),
                                                     sec_plus = c('Some secondary', 'Completed Secondary', 'Some / Completed University or College')
)

ch_hh2012$mother_education = forcats::fct_collapse(ch_hh2012$mother_education,
                                                   no_school = 'No School',
                                                   some_prim = 'Some Primary',
                                                   prim_vocational = c('Vocational School', 'Completed Primary'),
                                                   sec_plus = c('Some secondary', 'Completed Secondary', 'Some / Completed University or College')
)





# rebase ------------------------------------------------------------------
# -- livelihood zones -- rebasing to Lake Kivu
ch_hh2012$livelihood_zone = factor(ch_hh2012$livelihood_zone,
                                   levels = c("Lake Kivu Coffee Zone",
                                              "Central Plateau Cassava and Coffee Zone",                         
                                              "East Congo-Nile Highland Subsistence Farming Zone",               
                                              "Southeastern Plateau Banana Zone",                                
                                              "Northwest Volcanic Irish Potato Zone",                            
                                              "Kigali city",                                                     
                                              "West Congo-Nile Crest Tea Zone",                                  
                                              "Central-Northern Highland Irish Potato, Beans and Vegetable Zone",
                                              "Eastern Plateau Mixed Agriculture Zone",                          
                                              "Eastern Agropastoral Zone",                                       
                                              "Bugesera Cassava Zone",                                           
                                              "Northern Highland Beans and Wheat Zone",                          
                                              "Eastern Semi-Arid Agropastoral Zone"))

# -- land: rebasing to small land plots 0-0.1ha --
ch_hh2012$land_size_cat = fct_relevel(ch_hh2012$land_size_cat, "0.00 - 0.19 ha")

# -- education: rebasing to no ed --
ch_hh2012$head_education_cat = fct_relevel(ch_hh2012$head_education_cat, "no_school")
ch_hh2012$mother_education = fct_relevel(ch_hh2012$mother_education, "no_school")


# - -----------------------------------------------------------------------
# - -----------------------------------------------------------------------




# Admin3 seems confused.
codebk = data.frame(code = attr(hh2012_raw[['s_code']], "labels"),
                    names = names(attr(hh2012_raw[['s_code']], "labels")))

hh2012$admin3 = fct_infreq(factor(hh2012$s_code, levels = codebk$code,
                                  labels = codebk$names))

# Admin4 seems confused.
codebk = data.frame(code = attr(hh2012_raw[['v_code']], "labels"),
                    names = names(attr(hh2012_raw[['v_code']], "labels")))

hh2012$admin4 = fct_infreq(factor(hh2012$v_code, levels = codebk$code,
                                  labels = codebk$names))

# where the WB was working ------------------------------------------------

wb = hh2012 %>% 
  select(admin2, admin3, wb_site) %>% 
  distinct() %>% 
  mutate(admin3 = stringr::str_to_title(admin3))

# test
wb_map = full_join(RWA_admin3$centroids, wb, by = c('label' = 'admin3'))

wb_map %>% filter(is.na(wb_site))

wb_map %>% filter(is.na(lat))

# Only missing Shyrongi, and I'm too lazy to fix since it isn't a site anyway.
# Note: village names do not merge w/ Cell names.

wb_map = full_join(RWA_admin3$df, wb, by = c('Sector' = 'admin3'))

ggplot(wb_map, aes(fill = factor(wb_site), x = long, y = lat,
                   group = group, order = order)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_manual(values = c('0' = grey15K, '1' = brewer.pal(9, 'Spectral')[1])) +
  theme_blank()
