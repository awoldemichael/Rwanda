# Rwanda stunting analysis -----------------------------------------
#
# RWA_WFP_01_importKids.R: import children's data
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

children_raw = read_sav(paste0(baseDir, 'RW_2015_CFSVA/cfsva-2015-child-DB- annex.sav'))



# Checking how weights should be applied ----------------------------------

# According to CFSVA final report, they used a two-stage survey design, with the districts as the primary cut and villages as the secondary:
# (from CFSVA 2015 detailed survey methodolgy)
# To facilitate comparison with existing studies, the CFSVA 2015 was designed to provide statistically representative
# and precise information at the district level. In addition, it was decided to include both urban and rural households
# and not to exclude the capital province Kigali. The sampling frame was organized according to the 30 districts.
# Subsequently, a two-stage cluster sample procedure was applied.
# In the first stage, 25 villages per district were randomly selected with probability to be selected proportional to the
# population size. In the second stage, ten households in each of the 25 villages in the 30 provinces were selected for
# participation in the survey. A systematic random sampling technique was chosen for this stage. The team leader,
# together with the village head, listed all households in the village. Based on this list, a systematic random sample
# was utilized to pick ten households to be interviewed and three reserve households should any of the first ten
# households be missing at the time of the interview or not agree to participate. Households were eligible for
# participation in the assessment if living in one of the selected villages at the time of the interviews.
# Thus, ten households, from 25 villages, from 30 provinces were chosen to participate in the survey, amounting up
# to 7,500 households.

# And from the NISR explanation of the data:
# "Taking into consideration the two-stage cluster sampling methodology described above, adjustment weights were
# computed to provide results representative at country level. The household probability of being selected in the
# sample is equal to the product of a household’s probability of being selected in a village by the probability of the
# village of being sampled. The inverse of this probability is the design weight. The design weight was adjusted for
# the expected and actual number of households in the surveyed villages and was used in the complex sample
# calculations. The design weight was divided by the product of the total number of households in the population
# divided by the number of sampled households. The resulting weight was used in all non-complex sample analyses."

# Based on this info, it seems like the primary strata = 30 districts (S0_D_Dist) and the enumeration areas are the villages (S0_G_Vill)
# Unfortunately, the village data are only located in the kids dataset, not the dataset at large.

# Note: straight averages don't work:
x = children_raw %>% mutate(st = Stunted_global * normalized_weight_CHILD)

# Similar but not right
x  %>% group_by(Urban) %>% summarise(tot = sum(Stunted_global), n = n()) %>% mutate(pct = tot/n)


# Seems right on target w/ the strata being the 30 districts (first sampling division)
# Comparing final numbers from the CFSVA to the ones I calculated
# Point of difference: report says there are 4058 children measured, but the smaple only contains 3810.
# Guessing (?) difference is that there were 4058 eligible, but only 3810 measured / valid. 
# Raw children's file very clearly has 280 NAs, and numbers check out.
cfsva = svydesign(id = ~S0_G_Vill, strata = ~S0_D_Dist, weights = ~weight, data = children_raw)

svymean(~Stunted_global, design = cfsva, na.rm = TRUE)
# mean   SE
# Stunted_global 0.36708 0.01

svyby(~Stunted_global, design = cfsva, by = ~Urban, svymean, na.rm = TRUE)
# Urban Stunted_global         se
# 1     1      0.2709561 0.02541909
# 2     2      0.3961389 0.01039587

svyby(~Stunted_global, design = cfsva, by = ~S0_C_Prov, svymean, na.rm = TRUE)
# S0_C_Prov Stunted_global         se
# 1         1      0.2477522 0.02854823
# 2         2      0.3415020 0.01877670
# 3         3      0.4588179 0.01703526
# 4         4      0.3888858 0.03046762
# 5         5      0.3506154 0.02088620

svyby(~Stunted_global, design = cfsva, by = ~livezone, svymean, na.rm = TRUE)
# livezone Stunted_global         se
# 0         0      0.2301671 0.03235057
# 1         1      0.3688482 0.02093194
# 2         2      0.5335319 0.03735974
# 3         3      0.4485097 0.04902240
# 4         4      0.4869630 0.02606977
# 5         5      0.2838102 0.01948119
# 6         6      0.5135553 0.07400019
# 7         7      0.3799819 0.03584594
# 8         8      0.2901785 0.03686537
# 9         9      0.3981396 0.03688260
# 10       10      0.3147220 0.03375017
# 11       11      0.3910140 0.04925721
# 12       12      0.2366302 0.05193559

svyby(~Stunted_global, design = cfsva, by = ~S0_D_Dist, svymean, na.rm = TRUE)


# Notes on data -----------------------------------------------------------

# -- stunting -- 
# stunting is based on 2006 WHO children's growth standards
# stunting was calculated by WFP in SPSS.
# Key stunting variables for 2015 data is *Stunted_global* (binary stunted) 
# and *HAZWHO* (height-for-age z-score based on the WHO distribution)

# -- livelihood zones --
# livelihood zones aren't entirely consistent w/ FEWS NET codes. 
# 0 == Kigali City, NOT urban areas (as is in the FEWS NET spatial file, and therefore used in DHS spatial join)

# select variables --------------------------------------------------------
# Remove attributes so they can be added back in in a logical manner.
ch = removeAttributes(children_raw)

# Was intending to pull majority of household explanatory variables from the household-level data.
# However, there's no unique id, so I have to jerry rig one.  
# As a result, pulling all the household-level variables I can from the children's dataset.

ch = ch %>% 
  select(
    # -- IDs / survey --
    CHN_KEY, # Despite the name, this isn't a unique id! Is merely a (unique) link to the database on their end.
    # PARENT_KEY,
    MHN_KEY,
    weight,
    # normalized_weight_CHILD, # redundant with actual weight; linearly related
    S0_B_DATE, # date of interview, in obnoxious SPSS form.
    
    # -- geo --
    S0_C_Prov, # note: redundant w/ S0_C_Prov_lyr. Not sure why 2x...
    S0_D_Dist,
    S0_E_Sect,
    village = S0_G_Vill, # village (746 villages)
    Urban,
    v_S2_02, # village category -- rural, urban organized, urban slum
    livezone_lyr,
    
    # -- demographics --
    wealth_idx = WI, # numeric wealth index
    WI_cat_lyr_lyr, # categorical classification of wealth; same as WI_cat but w/ lables
    S12_01, # Old Ubudehe category (poverty status for aid)
    S12_02, # New Ubudehe category (poverty status for aid)
    S14_02_2, # primary caregiver
    age_months = S14_02_7, # age
    S14_02_8, # sex
    S14_01, # # kids < 5 y in household
    
    # -- village aid profile --
    v_S2_03_1, # VUP (schemes applied in the village)
    v_S2_03_2, # Land consolidation (schemes applied in the village)
    v_S2_03_3, # IDP model village (schemes applied in the village)
    v_S2_03_4, # Structured umudugudu (schemes applied in the village)
    v_S2_03_88, # None (schemes applied in the village)
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
    
    # -- mother attributes -- 
    mother_age = S13_02_2,
    S13_02_3, # mother read/write
    S13_02_4, # mother's education; relying on their classification in education_groups: children_raw %>% group_by(ed = S13_02_4, education_groups) %>% summarise(n()
    education_groups, # classified mother's education
    # stunted/underweight mother is very rare (69 or 161 mothers in dataset)
    mother_BMI = BMI, # mother's BMI
    underwt_mother = underweight_women, # mother is underweight
    stunted_mother = stunted_women, # mother is stunted
    
    # -- nutrition --
    FCS, # food consumption score
    FCG, # classified food consumption score
    FS_final, # Final CARI food security index
    CSI, # reduced coping strategies index
    S14_03, # ever breastfed
    S14_03_2, # hours after birth breastfed
    S14_03_4, # given food/drink other than breastmilk in first 6 mo.
    still_breastfed = S14_03_5, # still breastfed (no NAs)
    # Children food consumption only for those who are currently breastfed (consumption, minimal acceptable diet, in feeding programs)
    
    # -- WASH -- 
    impr_toilet = improved_toilet, # !! Note: does not include whether share toilet
    share_toilet = S2_07_3, # no NAs
    impr_water = improved_water, # !! Note: does not filter by < 30 min.
    water_source_treatment,
    S2_07_2, # if no toilet, what use; just in case is useful for merging
    S2_13, # # liters of water used / day, what use; just in case is useful for merging
    
    # -- supplements --
    # most kids (3963) got vit A drops
    
    # -- birth weight --
    birthweight_cat,
    birthwt = S14_03_6, # birth weight in kg
    
    # -- health --
    S14_05, # ill in the past 2 weeks 
    S14_05_2, # fever
    S14_05_3, # cough
    S14_05_4, # diarrhea
    S14_05_6, # given deworming tablets in past 6 mo.
    
    # -- WASH --
    # S14_06_3 & S14_06_4 on child handwashing before eating has too low coverage (3186 NAs)
    wash_beforecook = AS13_05,
    wash_kidtoilet = BS13_05,
    wash_beforeeat = CS13_05,
    wash_ifdirty = DS13_05,
    wash_aftertoilet = ES13_05,
    
    
    # -- stunting calcs --
    isWasted = Wasted_global, isStunted = Stunted_global, isUnderwt = Underweight_global, # binaries (yes or no)
    wasted_cat = Wasted, stunted_cat = Stunted, underwt_cat = Underweight, # normal, moderate, severe
    stunting_invalid = WHO_Flag, # whether child has height/weight measured
    stuntingZ_NHCS = HAZNCHS, # stunting score based on NCHS standards
    stuntingZ = HAZWHO # stunting z-score based on WHO standards
    
  )


# clean vars --------------------------------------------------------------
# `factorize` is a function within custom-built llamar pkg.  See below for copy of code.

ch = ch %>% 
  mutate(
    
    # -- fix weirdness / create new var --
    interview_date = as.Date(S0_B_DATE + ISOdate(1582,10,14)), # Convert SPSS date to a normal time; based on http://r.789695.n4.nabble.com/How-to-convert-SPSS-date-data-to-dates-td793972.html
    month = lubridate::month(interview_date),
    
    impr_unshared_toilet = case_when(ch$impr_toilet == 0 ~ 0, # unimproved
                                     (ch$impr_toilet == 1 & ch$share_toilet == 0) ~ 1, # improved + unshared
                                     (ch$impr_toilet == 1 & ch$share_toilet == 1) ~ 0, # improved + shared
                                     TRUE ~ NA_real_),
    
    school_dist_cat = case_when(ch$village_school == 1 ~ 1, # school in village
                                ch$v_S3_02_2 <= 30 ~ 2, # school w/i 30 min. of village
                                (ch$v_S3_02_2 <= 60 & ch$v_S3_02_2 > 30) ~ 3, # school 30-60 min. of village
                                ch$v_S3_02_2  > 60 ~ 4, # school > 60 min. of village
                                TRUE ~ NA_real_),
    school_dist_cat = forcats::fct_infreq( # sort by frequency
      factor(school_dist_cat,
             levels = 1:4,
             labels = c('school in village', 
                        'school within 30 min. of village (outside village)',
                        'school 30 - 60 min. from village',
                        'school > 60 min. from village'))),
    
    # -- create binaries --
    low_birthwt = case_when(ch$birthweight_cat == 1 ~ 1,
                            ch$birthweight_cat == 2 ~ 0,
                            TRUE ~ NA_real_),
    
    village_VUP = case_when(ch$v_S2_03_1 == 0 ~ 0,
                            ch$v_S2_03_1 == 1 ~ 1,
                            TRUE ~ NA_real_), 
    village_landConsolid = case_when(ch$v_S2_03_2 == 0 ~ 0,
                                     ch$v_S2_03_2 == 2 ~ 1,
                                     TRUE ~ NA_real_), 
    village_IDPmodel = case_when(ch$v_S2_03_3 == 0 ~ 0,
                                 ch$v_S2_03_3 == 3 ~ 1,
                                 TRUE ~ NA_real_), 
    village_structUmudugudu = case_when(ch$v_S2_03_4 == 0 ~ 0,
                                        ch$v_S2_03_4 == 4 ~ 1,
                                        TRUE ~ NA_real_), 
    village_noSchemes = case_when(ch$v_S2_03_88 == 88 ~ 1,
                                  ch$v_S2_03_4 == 0 ~ 0,
                                  TRUE ~ NA_real_), 
    
    # -- regroup --
    kids_under5 = case_when(ch$S14_01 == 1 ~ 1, # one child < 5 years old in hh
                            ch$S14_01 == 2 ~ 2, # 2 children < 5 years old in hh
                            ch$S14_01 > 2 ~ 3, # 3+ children < 5 years old in hh
                            TRUE ~ NA_real_),
    kids_under5 = forcats::fct_infreq( # sort by frequency
      factor(kids_under5,
             levels = 1:3,
             labels = c('1 child < 5 years old in hh', 
                        '2 children < 5 years old in hh',
                        '3+ children < 5 years old in hh'))),
    
    mother_literate = case_when(ch$S13_02_3 == 0 ~ 0, # illiterate
                                ch$S13_02_3 == 1 ~ 1, # can read & write
                                ch$S13_02_3 == 2 ~ 1, # can read but not write
                                TRUE ~ NA_real_), 
    
    breastfed_afterbirth = case_when(ch$S14_03 == 0 ~ 0, # never breastfed
                                     ch$S14_03_2 <= 1 ~ 1, # breastfed within first hr of birth
                                     ch$S14_03_2 > 1 ~ 2, # breastfed more than 1 hr after birth
                                     TRUE ~ NA_real_),
    breastfed_afterbirth = forcats::fct_infreq( # sort by frequency
      factor(breastfed_afterbirth,
             levels = c(0, 1, 2),
             labels = c('never breastfed', 
                        'breastfed within first hour of birth',
                        'breastfed > 1 hr after birth'))),
    
    # -- Replace NAs --
    ever_breastfed = na_if(S14_03, 88),
    fed_nonbreastmilk = na_if(S14_03_4, 88),
    
    ill_fortnight = na_if(S14_05, 88),
    fever = na_if(S14_05_2, 88),
    cough = na_if(S14_05_3, 88),
    diarrhea = na_if(S14_05_4, 88),
    dewormed = na_if(S14_05_6, 88)) %>% 
  # -- create factors based on the labels in original dataset --
  # -- location --
  factorize(children_raw, 'Urban', 'rural_cat') %>% 
  factorize(children_raw, 'S0_C_Prov', 'admin1') %>% 
  factorize(children_raw, 'S0_D_Dist', 'admin2') %>% 
  factorize(children_raw, 'S0_E_Sect', 'admin3') %>% 
  factorize(children_raw, 'livezone_lyr', 'livelihood_zone') %>% 
  # -- demographics --
  factorize(children_raw, 'WI_cat_lyr_lyr', 'wealth_idx_cat') %>% 
  factorize(children_raw, 'S12_01', 'old_ubudehe') %>%
  factorize(children_raw, 'S12_02', 'new_ubudehe') %>%
  factorize(children_raw, 'v_S2_02', 'village_cat') %>%
  factorize(children_raw, 'S14_02_2', 'prim_caregiver') %>% 
  factorize(children_raw, 'S14_02_8', 'sex') %>% 
  # -- village connectivity --
  factorize(children_raw, 'health_facility_distance', 'health_dist_cat') %>% 
  factorize(children_raw, 'health_less_60min', 'health_less_60min') %>% 
  factorize(children_raw, 'market_distance', 'market_dist_cat') %>%
  factorize(children_raw, 'market_less_60min', 'market_less_60min') %>% 
  factorize(children_raw, 'road_distance', 'road_dist_cat') %>% 
  # -- nutrition --
  factorize(children_raw, 'FCG', 'FCS_cat') %>% 
  factorize(children_raw, 'FS_final', 'CARI_cat') %>% 
  # -- WASH --
  factorize(children_raw, 'water_source_treatment', 'drinkingH2O_cat') %>% # whether improved source water + treatment
  # -- education --
  factorize(children_raw, 'education_groups', 'mother_education') %>% 
  
  # -- Create indices --
  rowwise() %>% 
  mutate(
    # Creating crude cleanliness index.  Assuming NAs aren't important-- but the NAs are consistent for all washing questions anyway.
    wash_knowl = sum(wash_beforecook, wash_kidtoilet, wash_aftertoilet, wash_beforeeat, wash_ifdirty, na.rm = TRUE)
  )





# double check there are no NA values in any of the vars ------------------
# Assuming NA values are 88
# Note: will ignore all the factor levels.
cutoff = 70

ch_test = as.data.frame(ch > cutoff)

ch_test = ch_test %>% summarise_each(funs(sum(., na.rm = TRUE)))

ch_test = t(ch_test)



