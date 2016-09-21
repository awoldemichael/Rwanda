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

hh = hh_raw %>% 
  mutate(
    # -- household ids / info --
    KEY, # sadly, not actually a unique id.
    
    # Survey conducted in April/May, in midst of minor lean season http://www.fews.net/file/113529
    int_date = S0_B_DATE, # interview date
    month,
    weight,
    normalized_weight,
    
    # -- geography --
    Urban,
    S0_C_Prov, # province
    S0_D_Dist, # district
    S0_E_Sect, # sector
    livezone, # livelihood zone
    
    # -- demographics
    hh_size = S1_01, # hh size
    S1_01_3, # female headed
    pct_under7 = S1_01_7_HC88_S, # percent < 7
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
    school_village = v_S3_02, # school in village
    # School attendance has very low numbers: ~ 418 for M above 7 (out of 3070)
    # School enrollment much better: 2776 / 3070 for M above 7
    
    # -- wealth status --
    wealth_idx = WI_cat_lyr, # wealth index; redundant with WI_cat: sum(hh_raw$WI_cat == hh_raw$WI_cat_lyr)
    monthly_expend = P_CAP_EXP, # monthly per capita expenditures
    food_expend = FIE,
    sh_food_expend = S_FIE,
    
    S12_01, # external assistance classification (old)
    S12_02, # external assistance classification (new)
    v_S2_02, # urban slums
    v_S2_03_1, # VUP: Vision 2020 Umurenge Program-- Integrated Local Development Program to Accelerate Poverty Eradication, Rural Growth, and Social Protection
    
    # -- economics --
    mkt_village = v_S4_01, # market in village
    mkt_accessible = v_S4_02_4, # can access market yearround w/o walking
    market_less_60min, # market w/i 60 min.
    road_distance,
    num_jobs = S3_01, # number of livelihood activities
    # Keeping shares of livelihoods > 5% on average
    # View(t(hh_raw  %>% select(contains('sh_')) %>% summarise_each(funs(mean(., na.rm = TRUE)))))
    sh_agricultural_production, 
    sh_labour_ag_work, 
    sh_unskilled_labour,
    livelihood_group_2, # categorization of how earn living
    
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
    GDDS, # classified diet. diversity
    HDDS_24h, # 24 h dietary diversity recall.  Range = 0 - 12 
    CSI,
    
    # Stunting
    hh_stunted = Stunted_YN
  ) 


# Clean & recode vars -----------------------------------------------------
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


