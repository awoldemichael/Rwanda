# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_03_importHH.R: import household-level data
#
# Script to pull stunting data and associated household- or
# child-level data for Rwanda from the CFSVA dataset
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

# Import in the raw data --------------------------------------------------
# Raw data is contained in three files:
# children's data: 'cfsva-2015-child-DB- annex.sav'
# household-level data:  'cfsva-2015-master-DB- annex.sav'
# women's data: 'cfsva-2015-mother-DB- annex.sav'

# -- household data --
hh_raw = read_sav(paste0(baseDir, 'RW_2015_CFSVA/cfsva-2015-master-DB- annex.sav'))
hh2012 = read_sav(paste0(baseDir, 'RW_2012_CFSVA/cfsvans-2012- household-v01.sav'))


# pull relevant vars ------------------------------------------------------
hh = removeAttributes(hh_raw)

hh = hh %>% 
  select(
    # -- IDs / merging vars --
    KEY, # sadly, not actually a unique id. Well, not a *consistent* unique id
    S2_13, # # liters of water used / day, what use; just in case is useful for merging
    
    # -- survey vars --
    # Survey conducted in April/May, in midst of minor lean season http://www.fews.net/file/113529
    int_date = S0_B_DATE, # interview date
    month,
    weight,
    
    # -- geo --
    S0_C_Prov, # note: redundant w/ S0_C_Prov_lyr. Not sure why 2x...
    S0_D_Dist,
    S0_E_Sect,
    Urban,
    v_S2_02, # village category -- rural, urban organized, urban slum
    livezone,
    
    # -- demographics --
    hh_size = S1_01, # hh size
    S1_01_3, # female headed
    pct_under7 = S1_01_7_HC88_S, # percent < 7 yrs old
    # S1_01_11_C, # polygamous -- too few hh
    
    # -- education --
    S1_01_7, # literate head of household
    S1_01_8, # education of head
    pct_illiterate = S1_01_7_HC0_S, # percent can't read or write (excl. under 7)
    pct_literate = S1_01_7_HC1_S, # percent of house that can read + write (incl. under 7)
    pct_lowEd = S1_01_8_S2, # percent of house with no education or primary (excl. under 7)
    pct_highEd = S1_01_8_S3, # secondary, tertiary, vocational
    missed_school = S1_01_13, # child missed > 1 week since Jan 2015 (~ 3-4 months)
    missed_school_sick = S1_01_14_1, # child missed > 1 week of school b/c of illness
    # School attendance has very low numbers: ~ 418 for M above 7 (out of 3070)
    # School enrollment much better: 2776 / 3070 for M above 7
    
    
    # -- wealth/assets --
    WI_cat, # categorical classification of wealth; redundant with WI_cat_lyr: sum(hh_raw$WI_cat == hh_raw$WI_cat_lyr)
    monthly_expend = P_CAP_EXP, # monthly per capita expenditures
    food_expend = FIE,
    sh_food_expend = S_FIE,
    S12_01, # Old Ubudehe category (external assistance classification; poverty status for aid)
    S12_02, # New Ubudehe category (poverty status for aid)
    
    # -- occupations --
    num_jobs = S3_01, # number of livelihood activities for HOUSEHOLD
    # Keeping shares of livelihoods > 5% on average
    # View(t(hh_raw  %>% select(contains('sh_')) %>% summarise_each(funs(mean(., na.rm = TRUE)))))
    sh_agricultural_production, 
    sh_labour_ag_work, 
    sh_unskilled_labour,
    livelihood_group_2, # categorization of how earn living
    
    # -- village aid profile --
    v_S2_03_1, # VUP (schemes applied in the village) Vision 2020 Umurenge Program-- Integrated Local Development Program to Accelerate Poverty Eradication, Rural Growth, and Social Protection
    v_S2_03_2, # Land consolidation (schemes applied in the village)
    v_S2_03_3, # IDP model village (schemes applied in the village)
    v_S2_03_4, # Structured umudugudu (schemes applied in the village)
    # ignoring `v_S2_03_5`: other schemes applied to village; ~ 10% of the villages.
    
    # -- village connectivity --
    # only ~ 5% villages have market within them; ignoring v_S4_01
    # v_S3_01: # hh in village with electricity
    village_school = v_S3_02,
    v_S3_02_2, # time (minutes) to nearest school
    # village_healthfac = v_S3_03, # only 276/4058 w/ health facility in village - 6-7%
    health_facility_distance,
    health_less_60min, 
    market_distance,
    market_less_60min,
    road_distance,
    
    
    # -- nutrition --
    FCS, # food consumption score
    FCG, # classified food consumption score
    FS_final, # Final CARI food security index
    CSI, # reduced coping strategies index
    
    # -- WASH -- 
    impr_toilet = improved_toilet, # !! Note: does not include whether share toilet
    share_toilet = S2_07_3, # no NAs
    impr_water = improved_water, # !! Note: does not filter by < 30 min.
    water_source_treatment
  )




hh = hh_raw %>% 
  mutate(

                        


                        
                        # -- assets (infrastructure) --
                        S2_04, # whether house in umudugudu (new recommended settlement)
                        S2_05, # own or rent house
                        bedrooms = S2_06,
                        crowding, # # people/room
                        impr_roof = improved_roof2, # improved roof. Note: only 42/7500 WITHOUT good roof.
                        impr_floor = improved_floor,
                        impr_wall = improved_wall,
                        share_toilet = S2_07_3,
                        impr_light = improved_light,
                        S2_09, # source of cooking fuel
                        mostly_selling,
                        mostly_consuming,
                        
                        # -- farming assets --
                        own_livestock,
                        own_cattle,
                        manage_livestock,
                        TLU,
                        own_land = S4_01,
                        S4_01_2, # land size. Note classified, and in ha
                        hh_garden = S4_01_8, # own garden
                        growing_beans, # whether hh grows crop
                        growing_maize,
                        growing_s_potato,
                        growing_cassava,       
                        growing_i_potato,
                        growing_sorghum,
                        growing_banana_cooking,
                        growing_banana_wine,
                        
                        # -- WASH -- 
                        impr_toilet = improved_toilet, # !! Note: does not include whether share toilet
                        impr_water = improved_water, # !! Note: does not filter by < 30 min.
                        time_water_source,
                        S2_12, # treat water before drinking
                        water_source_treatment, # how treat water before drinking
                        
                        # -- health facility --
                        hlth_fac_village = v_S3_03, # health facility in village
                        health_less_60min, # health facility less than 60 min. from house or in village
                        
                        # -- food security -- 
                        cari_idx = FS_final_lyr, # CARI food security index
                        stock_durationA, # household stock in growing season A -- 2015.  B and C are from 2014 and get into too many NAs (> 6000)
                        # Ignoring infrequent food sources (< 5%)
                        # hh_raw  %>% select(contains('shr_')) %>% summarise_each(funs(mean(., na.rm = TRUE)))
                        sh_food_purchased = shr_pur, # share of purchased food
                        sh_food_grown = shr_own, # share of own food grown.  
                        child_meal_freq = S9_02_cat, # Note: categorical (0, 1, 2, 3+)
                        S9_04, # staple
                        num_starch = Starch, # number of days consumed
                        num_pulses = Pulses,
                        num_meat = Meat,
                        num_veg = Vegetables,
                        num_oil = Oil,
                        num_fruit = Fruit,
                        num_milk = Milk,
                        num_sugar = Sugar,
                        VitA_groups, # classified
                        protein_groups, # classified
                        HIron_groups, # classified
                        FCS, 
                        FCG, # classified food consumption score
                        DDS, # dietary diversity score. Range = 0 - 7 
                        GDDS, # classified diet. diversity (from 24 h recall; hh module?)
                        HDDS_24h, # 24 h dietary diversity recall (from hh module?).  Range = 0 - 12 
                        CSI,
                        
                        # Stunting
                        hh_stunted = Stunted_YN
  ) 


# Clean & recode vars -----------------------------------------------------
# `factorize` is a function within custom-built llamar pkg.  See below for copy of code.

hh = hh %>% 
  mutate(
    
    # -- fix weirdness / create new var --
    impr_unshared_toilet = case_when((hh$impr_toilet == 1 & hh$share_toilet == 0) ~ 1, # improved + unshared
                                     (hh$impr_toilet == 1 & hh$share_toilet == 1) ~ 0, # improved + shared
                                     hh$impr_toilet == 0 ~ 0,
                                     TRUE ~ NA_real_),
    
    
    # -- create binaries --
    fem_headed = case_when(hh$S1_01_3 == 1 ~ 0, # male-headed
                           hh$S1_01_3 == 2 ~ 1, # female-headed
                           TRUE ~ NA_real_),
    
    village_VUP = case_when(hh$v_S2_03_1 == 0 ~ 0,
                            hh$v_S2_03_1 == 1 ~ 1,
                            TRUE ~ NA_real_), 
    village_landConsolid = case_when(hh$v_S2_03_2 == 0 ~ 0,
                                     hh$v_S2_03_2 == 2 ~ 1,
                                     TRUE ~ NA_real_), 
    village_IDPmodel = case_when(hh$v_S2_03_3 == 0 ~ 0,
                                 hh$v_S2_03_3 == 3 ~ 1,
                                 TRUE ~ NA_real_), 
    village_structUmudugudu = case_when(hh$v_S2_03_4 == 0 ~ 0,
                                        hh$v_S2_03_4 == 4 ~ 1,
                                        TRUE ~ NA_real_),
    
    # -- regroup --
    head_literate = case_when(hh$S1_01_7 == 0 ~ 0, # illiterate
                                hh$S1_01_7 == 1 ~ 1, # can read & write
                                hh$S1_01_7 == 2 ~ 1, # can read but not write
                                TRUE ~ NA_real_) 
    
    # -- Replace NAs --
  ) %>% 
  # -- create factors based on the labels in original dataset -
  # -- location --
  factorize(hh_raw, 'Urban', 'rural_cat') %>% 
  factorize(hh_raw, 'S0_C_Prov', 'admin1') %>% 
  factorize(hh_raw, 'S0_D_Dist', 'admin2') %>% 
  factorize(hh_raw, 'S0_E_Sect', 'admin3') %>% 
  factorize(hh_raw, 'livezone', 'livelihood_zone') %>% 
  # -- demographics --
  factorize(hh_raw, 'month', 'month') %>% 
  factorize(hh_raw, 'WI_cat', 'wealth_idx_cat') %>% 
  factorize(hh_raw, 'livelihood_group_2', 'hh_occup_cat') %>% 
  factorize(hh_raw, 'S12_01', 'old_ubudehe') %>%
  factorize(hh_raw, 'S12_02', 'new_ubudehe') %>%
  factorize(hh_raw, 'v_S2_02', 'village_cat') %>%
  # -- village connectivity --
  factorize(hh_raw, 'health_facility_distance', 'health_dist_cat') %>% 
  factorize(hh_raw, 'health_less_60min', 'health_less_60min') %>% 
  factorize(hh_raw, 'market_distance', 'market_dist_cat') %>%
  factorize(hh_raw, 'market_less_60min', 'market_less_60min') %>% 
  factorize(hh_raw, 'road_distance', 'road_dist_cat') %>% 
  # -- nutrition --
  factorize(hh_raw, 'FCG', 'FCS_cat') %>% 
  factorize(hh_raw, 'FS_final', 'CARI_cat') %>% 
  # -- WASH --
  factorize(hh_raw, 'water_source_treatment', 'drinkingH2O_cat') %>%  # whether improved source water + treatment
# -- education --
factorize(hh_raw, 'S1_01_8', 'head_education_cat') 

# hh$int_month = plyr::mapvalues(hh$livezone_lyr, from = livelihood_zones$codes, to = livelihood_zones$lz)
int_month
admin1-3
hh division m/f
umudugudu 
!! check food expend == per capita
asset idx
impr_toilet

hh = hh %>% 
  mutate(
    
    urban = case_when(hh$Urban == 1 ~ 1,
                      hh$Urban == 2 ~ 0,
                      TRUE ~ NA_real_),
    fem_head = case_when(hh$S1_01_3 == 1 ~ 0, # male
                         hh$S1_01_3 == 2 ~ 1,
                         TRUE ~ NA_real_)
  )


# merge into kids data ----------------------------------------------------

ch_hh = left_join(ch, hh, by = c("weight",                   
                                 "S2_13",     # liters of water used              
                                 "WI_cat",
                                 "S12_01", # Ubudehe profile (old) 
                                 "S12_02", # Ubudehe profile (new)
                                 "FCS",   
                                 "FS_final")) # food security CARI index
