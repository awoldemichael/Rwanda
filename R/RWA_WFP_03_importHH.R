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


# NOTES -------------------------------------------------------------------

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
    monthly_pc_expend = P_CAP_EXP, # monthly per capita expenditures
    food_expend = FIE, # monthly
    sh_food_expend = S_FIE,
    S12_01, # Old Ubudehe category (external assistance classification; poverty status for aid)
    S12_02, # New Ubudehe category (poverty status for aid)
    mostly_selling, # HH selling more than 50% of the produce
    mostly_consuming, # HH consuming more than 50% of the produce
    
    S7_01, # loan in last 12 mo.; anyone in hh
    S7_01_2, # whether received loan
    S7_02, # how loan was used
    S7_03, # source of loan
    S7_04, # value of loan
    
    # -- assets (infrastructure) --
    S2_05, # own or rent house
    bedrooms = S2_06,
    crowding, # # people/room
    impr_roof = improved_roof2, # improved roof. Note: only 42/7500 WITHOUT good roof.
    impr_floor = improved_floor,
    impr_wall = improved_wall,
    impr_light = improved_light,
    S2_09, # source of cooking fuel
    
    # -- farming assets --
    # blissfully, no NA codes.
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
    
    # -- occupations --
    num_jobs = S3_01, # number of livelihood activities for HOUSEHOLD
    # Keeping shares of livelihoods > 5% on average
    # View(t(hh_raw  %>% select(contains('sh_')) %>% summarise_each(funs(mean(., na.rm = TRUE)))))
    sh_agricultural_production, 
    sh_labour_ag_work, 
    sh_unskilled_labour,
    livelihood_group_2, # categorization of how earn living
    
    # -- village & hh aid profile --
    S2_04, # whether house in umudugudu (new recommended settlement)
    v_S2_03_1, # VUP (schemes applied in the village) Vision 2020 Umurenge Program-- Integrated Local Development Program to Accelerate Poverty Eradication, Rural Growth, and Social Protection
    v_S2_03_2, # Land consolidation (schemes applied in the village)
    v_S2_03_3, # IDP model village (schemes applied in the village)
    v_S2_03_4, # Structured umudugudu (schemes applied in the village)
    # ignoring `v_S2_03_5`: other schemes applied to village; ~ 10% of the villages.
    
    # looking at most frequent sources of hh aid:
    # hh_raw  %>% select(contains('assistance')) %>% summarise_each(funs(mean(., na.rm = TRUE)))
    # hh_raw  %>% select(contains('S12_04_2')) %>% summarise_each(funs(mean(., na.rm = TRUE)))
    # t(hh_raw  %>% select(contains('S12_07_2')) %>% summarise_each(funs(mean(., na.rm = TRUE))))
    any_food_assistance, # categorical
    any_non_food_assistance, # categorical
    financial_assistance,
    ag_assistance = agriculture_assisstance,
    # S12_04_2... variables give the type of food assistance.  most free food distribution but not a whole lot of observations
    # S12_07_2... variables give the type of other assistance. very few values.
    
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
    DDS, # dietary diversity score. Range = 0 - 7. Presuming based on the 7 day recall of food grps used to calc FCS.
    GDDS, # classified diet. diversity (from 24 h recall; hh module?)
    HDDS_24h, # 24 h dietary diversity recall (from hh module?).  Range = 0 - 12 
    
    # -- food security -- 
    stock_durationA, # household stock in growing season A -- 2015.  B and C are from 2014 and get into too many NAs (> 6000)
    # Ignoring infrequent food sources (< 5%); most households purchase or grow food
    # hh_raw  %>% select(contains('shr_')) %>% summarise_each(funs(mean(., na.rm = TRUE)))
    sh_food_purchased = shr_pur, # share of purchased food
    sh_food_grown = shr_own, # share of own food grown.  
    S9_01_cat, # adult meal frequency; categorical
    S9_02_cat, # Note: child meal frequency; categorical (0, 1, 2, 3+)
    S9_04, # preferred staple food
    
    # -- food consumption --
    staples_days = Starch, # number of days consumed
    pulse_days = Pulses,
    meat_days = Meat,
    veg_days = Vegetables,
    oil_days = Oil,
    fruit_days = Fruit,
    milk_days = Milk,
    sugar_days = Sugar,
    
    VitA_groups, # classified; sadly there's no RAW data on what consumed aside from what they've already lumped together.
    protein_groups, # classified
    HIron_groups, # classified
    
    
    # -- food security --
    FS_final, # Final CARI food security index; redundant with FS_final_lyr
    CSI, # reduced coping strategies index
    CSI_terciles, # categorical coping index
    food_access_prob = S10_01, # hh had food access problems in last week
    FoodAccess, # food access problems in past year, categorized
    Months_FA, # months w/ food access issues.
    
    # -- shocks --
    # hh_raw  %>% select(contains('shock')) %>% summarise_each(funs(mean(., na.rm = TRUE)))
    # ~ 10% had shock from drought or illness. ignoring losing job, b/c only 3% of full sample
    # S11_02 -- whether the hh exp. a shock -- contains a surprising number of NAs, given that there are none in drought/illness shock vars.
    shock_drought,
    drought_coping,
    shock_illness,
    illness_coping,
    shock_ongoing = household_still_recovering, # still recovering from shock in last year (?). something is weird.  209 hh didn't experience shock but they're still recovering (?)
    
    # -- WASH -- 
    impr_toilet = improved_toilet, # !! Note: does not include whether share toilet
    share_toilet = S2_07_3, # no NAs
    impr_water = improved_water, # !! Note: does not filter by < 30 min.
    time_water_source,
    S2_12, # treat water before drinking
    water_source_treatment,
    
    # -- Stunting --
    hh_stunted = Stunted_YN
  )





# Clean & recode vars -----------------------------------------------------
# `factorize` is a function within custom-built llamar pkg.  See below for copy of code.

hh = hh %>% 
  mutate(
    # -- Replace NAs --
    asked_loan = na_if(S7_01, 88),
    
    # -- fix weirdness / create new var --
    impr_unshared_toilet = case_when(hh$impr_toilet == 0 ~ 0,
                                     (hh$impr_toilet == 1 & hh$share_toilet == 0) ~ 1, # improved + unshared
                                     (hh$impr_toilet == 1 & hh$share_toilet == 1) ~ 0, # improved + shared
                                     TRUE ~ NA_real_),
    
    impr_water_30min = case_when(hh$impr_water == 0 ~ 0, # unimproved
                                 (hh$impr_water == 1 & hh$time_water_source == 1) ~ 1, # improved + < 30 min. away
                                 (hh$impr_water == 1 & hh$time_water_source > 1) ~ 0, # improved + > 30 min. away
                                 TRUE ~ NA_real_),
    
    months_food_access = ifelse(FoodAccess == 0, 0, Months_FA), # # months have food access issues; setting NAs to 0 if no food access issues
    
    loan_value = ifelse((asked_loan == 0 | S7_01_2 == 0), 0, S7_04), # value of loan; setting to 0 if didn't get a loan
    
    # -- convert shares to ratios --
    sh_food_grown = sh_food_grown / 100,
    sh_labour_ag_work = sh_labour_ag_work / 100,
    sh_agricultural_production = sh_agricultural_production / 100,
    sh_unskilled_labour = sh_unskilled_labour / 100,
    sh_food_purchased = sh_food_purchased / 100,
    
    
    # -- create binaries --
    fem_head = case_when(hh$S1_01_3 == 1 ~ 0, # male-headed
                         hh$S1_01_3 == 2 ~ 1, # female-headed
                         TRUE ~ NA_real_),
    
    treat_water = case_when(hh$S2_12 == 6 ~ 0, # no water treatment
                            hh$S2_12 %in% 1:5 ~ 1, # boil, train, filter, bleach, or sediment water
                            TRUE ~ NA_real_
    ),
    
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
    got_loan = case_when(hh$S7_01 == 0 ~ 0, # didn't ask for one, so couldn't get one!
                         hh$S7_01_2 == 0 ~ 0, # asked but didn't receive (note: only 19 hh)
                         hh$S7_01_2 == 1 ~ 1, # asked & received
                         TRUE ~ NA_real_),
    
    # -- regroup --
    head_literate = case_when(hh$S1_01_7 == 0 ~ 0, # illiterate
                              hh$S1_01_7 == 1 ~ 1, # can read & write
                              hh$S1_01_7 == 2 ~ 1, # can read but not write
                              TRUE ~ NA_real_), 
    
    land_size = case_when(hh$own_land == 0 ~ 0, # no land
                          hh$S4_01_2 == 0 ~ 0, # land size reported as 'none'
                          hh$S4_01_2 == 1 ~ 1, # land size = 0.00 - 0.10 ha
                          hh$S4_01_2 == 2 ~ 2, # land size = 0.10 - 0.19 ha
                          hh$S4_01_2 == 3 ~ 3, # land size = 0.20 - 0.49 ha
                          hh$S4_01_2 == 4 ~ 4, # land size = 0.50 - 0.99 ha
                          hh$S4_01_2 == 5 ~ 5, # land size = 1.00 - 1.99 ha
                          hh$S4_01_2 >= 6 ~ 6, # land size > 2.00  ha - lumping previous category "> 5 ha" together, since only 9 hh
                          TRUE ~ NA_real_),
    land_size = forcats::fct_infreq( # sort by frequency
      factor(land_size,
             levels = 0:6,
             labels = c('no land', 
                        '0.00 - 0.10 ha',
                        '0.10 - 0.19 ha',
                        '0.20 - 0.49 ha',
                        '0.50 - 0.99 ha',
                        '1.00 - 1.99 ha',
                        'more than 2.00 ha')))
  ) %>% 
  # -- create factors based on the labels in original dataset -
  # -- location --
  factorize(hh_raw, 'Urban', 'rural_cat') %>% 
  factorize(hh_raw, 'S2_04', 'umudugudu_cat') %>% # Note: "town" isn't 100% urba
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
  # -- assets --
  factorize(hh_raw, 'S2_05', 'own_house_cat') %>%
  factorize(hh_raw, 'S2_09', 'cookingfuel_cat') %>%
  mutate(cookingfuel_cat = forcats::fct_lump(cookingfuel_cat, prop = 0.02)) %>% # grouping all "good" fuels into an other category. all infrequent anyway
  factorize(hh_raw, 'S7_02', 'loan_purpose') %>% # note: should condense
  factorize(hh_raw, 'S7_03', 'loan_source') %>% # note: should condense
  factorize(hh_raw, 'any_food_assistance', 'food_assistance') %>% # note: should condense
  factorize(hh_raw, 'any_non_food_assistance', 'nonfood_assistance') %>% # note: should condense
  # -- village connectivity --
  factorize(hh_raw, 'health_facility_distance', 'health_dist_cat') %>% 
  factorize(hh_raw, 'health_less_60min', 'health_less_60min') %>% 
  factorize(hh_raw, 'market_distance', 'market_dist_cat') %>%
  factorize(hh_raw, 'market_less_60min', 'market_less_60min') %>% 
  factorize(hh_raw, 'road_distance', 'road_dist_cat') %>% 
  # -- nutrition --
  factorize(hh_raw, 'FCG', 'FCS_cat') %>% 
  factorize(hh_raw, 'FS_final', 'CARI_cat') %>% # CARI food security index
  factorize(hh_raw, 'CSI_terciles', 'CSI_cat') %>% # CSI food security coping categories
  factorize(hh_raw, 'S9_01_cat', 'adult_meal_freq') %>% 
  factorize(hh_raw, 'S9_02_cat', 'child_meal_freq') %>% 
  factorize(hh_raw, 'S9_04', 'pref_staple') %>% # other than beans, soup 
  factorize(hh_raw, 'VitA_groups', 'vitAfruitveg_days') %>% 
  factorize(hh_raw, 'protein_groups', 'protein_days') %>% 
  factorize(hh_raw, 'HIron_groups', 'ironrich_days') %>% 
  factorize(hh_raw, 'FoodAccess', 'food_access_year_cat') %>% # food access issues over past year 
  # -- shock coping --
  factorize(hh_raw, 'drought_coping', 'drought_coping') %>%  # whether improved source water + treatment
  factorize(hh_raw, 'illness_coping', 'illness_coping') %>%  
  # -- WASH --
  factorize(hh_raw, 'S2_12', 'H2Otreatment_cat') %>%  
  factorize(hh_raw, 'water_source_treatment', 'drinkingH2O_cat') %>%  # whether improved source water + treatment
  factorize(hh_raw, 'time_water_source', 'time_drinkingH2O_cat') %>%  
  # -- education --
  factorize(hh_raw, 'S1_01_8', 'head_education_cat') 

# -- Creating shorter names for livelihood zones --
hh = hh %>% 
  mutate(lz_name = case_when(hh$livelihood_zone %like% 'Tea' ~ 'W. Congo-Nile Crest Tea',
                    hh$livelihood_zone %like% 'Wheat'            ~ 'N. Highland Beans/Wheat',                          
                    hh$livelihood_zone %like% 'Congo-Nile'    ~ 'E. Congo-Nile Highland Subsistence',
                    hh$livelihood_zone %like% 'Volcanic'         ~ 'N.W. Volcanic Irish Potato',                            
                    hh$livelihood_zone %like% 'Mixed'            ~ 'E. Plateau Mixed Agriculture',
                    hh$livelihood_zone %like% 'Eastern Ag'       ~ 'E. Agropastoral',                                       
                    hh$livelihood_zone %like% 'Central-Northern' ~ 'C.-N. Highland Irish Potato/Beans/Veg.',
                    hh$livelihood_zone %like% 'Kivu'             ~ 'Lake Kivu Coffee',
                    hh$livelihood_zone %like% 'Banana'           ~ 'S.E. Plateau Banana',
                    hh$livelihood_zone %like% 'Bugesera'         ~ 'Bugesera Cassava',                                           
                    hh$livelihood_zone %like% 'Central Plateau'  ~ 'C. Plateau Cassava/Coffee',
                    hh$livelihood_zone %like% 'Semi-Arid'        ~ 'E. Semi-Arid Agropastoral',                             
                    hh$livelihood_zone %like% 'Kigali'            ~ 'Kigali City',
                    TRUE ~ NA_character_))

# -- refactorize to be common w/ levels b/w ch and hh (to avoid losing levels in merge) --
# -- admin1 -- rebasing to Northern; then sorting by ch incidence
hh$admin1 = forcats::fct_relevel(hh$admin1, 'Northern', 'Western', 'Southern', 'Eastern', 'Kigali city')

# -- admin2 -- rebasing to Musanze
hh$admin2 = forcats::fct_relevel(hh$admin2, 'Musanze')

# -- livelihood zones -- rebasing to Lake Kivu
hh$livelihood_zone = forcats::fct_relevel(hh$livelihood_zone,
                                          "Lake Kivu Coffee Zone",
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
                                          "Eastern Semi-Arid Agropastoral Zone")  
# -- health_less_60min --
hh$health_less_60min = forcats::fct_relevel(hh$health_less_60min, levels(ch$health_less_60min))

# -- wealth index --
hh$wealth_idx_cat = forcats::fct_relevel(hh$wealth_idx_cat, levels(ch$wealth_idx_cat))



# double check there are no NA values in any of the vars ------------------
# Assuming NA values are 88
# Note: will ignore all the factor levels.
cutoff = 70

hh_test = as.data.frame(hh > cutoff)

hh_test = hh_test %>% summarise_each(funs(sum(., na.rm = TRUE)))

hh_test = t(hh_test)


# merge into kids data ----------------------------------------------------

ch_hh = left_join(ch, hh, by = c("weight",                   
                                 "S2_13",     # liters of water used              
                                 "WI_cat_lyr_lyr" = "WI_cat",
                                 "S12_01", # Ubudehe profile (old) 
                                 "S12_02", # Ubudehe profile (new)
                                 "FCS",   
                                 "FS_final", # food security CARI index
                                 # common variables along for the ride
                                 "rural_cat",
                                 "S0_C_Prov", 
                                 "S0_D_Dist", 
                                 "S0_E_Sect", 
                                 "Urban",  
                                 "village_school",
                                 "health_facility_distance",
                                 "health_less_60min",
                                 "market_distance",
                                 "market_less_60min",
                                 "road_distance",
                                 "FCG",
                                 "impr_toilet",
                                 "share_toilet",
                                 "impr_water",
                                 "water_source_treatment",
                                 "impr_unshared_toilet",
                                 "village_VUP",
                                 "village_landConsolid", "village_IDPmodel", "village_structUmudugudu",
                                 "admin1",
                                 "admin2", 
                                 "admin3",
                                 "livelihood_zone",
                                 "wealth_idx_cat",
                                 "old_ubudehe", "new_ubudehe",
                                 "village_cat",
                                 "health_dist_cat",
                                 "market_dist_cat", "road_dist_cat",
                                 "FCS_cat", "CARI_cat", "drinkingH2O_cat"
)) 

# Checking merge
print(ch_hh %>% group_by(is.na(DDS), is.na(village)) %>% summarise(n()))
nrow(ch_hh) == 4058

# is.na(DDS) is.na(village)   n()
# <lgl>          <lgl> <int>
#   1      FALSE          FALSE  4027
# 2       TRUE          FALSE    31




# Create some simple indices ----------------------------------------------
ch_hh = ch_hh %>% 
  mutate(
    infrastruct_idx = impr_roof + impr_floor + impr_wall + impr_light,
    wash_idx = impr_unshared_toilet + impr_water_30min + as.numeric(wash_knowl > 0) + treat_water)