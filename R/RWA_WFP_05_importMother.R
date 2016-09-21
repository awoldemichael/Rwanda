# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_05_importMother.R: import data from the mother module
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

women_raw = read_sav(paste0(baseDir, 'RW_2015_CFSVA/cfsva-2015-mother-DB- annex.sav'))


# Can women be merged w/ kids? --------------------------------------------

x = left_join(ch, women_raw, by = c("parent_id" = "PARENT_KEY"))

# Result: some mergal, but also extra ch values (presumably from non-unqiueness of argument)

# PARENT_KEY isn't unique...
nrow(women_raw %>% select(PARENT_KEY) %>% distinct())

# .., but KEY is (mother's id?)
nrow(women_raw %>% select(KEY) %>% distinct())

# Unfortunately, mother's key in children's dataset is missing 415 values.
x = left_join(children_raw, women_raw, by = c('MHN_KEY' = 'KEY'))

# Simplest thing should be to do parent id + age of mother.
nrow(women_raw %>% select(PARENT_KEY, S13_02_2) %>% distinct())
# Okay.  25 mismatches.

# Throwing in BMI:
nrow(women_raw %>% select(PARENT_KEY, S13_02_2, BMI) %>% distinct())

# Yippee!
x = left_join(ch, women_raw, by = c("parent_id" = "PARENT_KEY", "mother_age" = "S13_02_2", "mother_BMI" = "BMI")) # unique

# Only doesn't merge properly, b/c imprecise precision of BMI.
x  %>% group_by(WDDS) %>% summarise(n()) # WDDS is only in mother's module, and contains no NAs

ch = ch %>% mutate(rounded_BMI = round(mother_BMI, 2))
women_raw = women_raw %>% mutate(rounded_BMI = round(BMI, 2))

x = left_join(ch, women_raw, by = c("parent_id" = "PARENT_KEY", "mother_age" = "S13_02_2", "rounded_BMI" = "rounded_BMI")) # unique

# Only doesn't merge properly, b/c imprecise precision of BMI.
x  %>% group_by(WDDS) %>% summarise(n()) # WDDS is only in mother's module, and contains no NAs

# Still unique
nrow(women_raw %>% mutate(bmi = round(BMI, 2)) %>% select(PARENT_KEY, S13_02_2, bmi) %>% distinct())
# down to 414.  (sigh)  NA for BMIs?  Gonna have to do this stepwise


# investigating whether there are interesting unique variables in the women's  --------
# received Vit A when pregnant: too many NAs
women_raw %>% group_by(S13_03_3) %>% summarise(n())

# antenatal care: S13_03_4-8
women_raw %>% group_by(S13_03_4) %>% summarise(n())
women_raw %>% group_by(S13_03_5) %>% summarise(n())
women_raw %>% group_by(S13_03_6) %>% summarise(n())
women_raw %>% group_by(S13_03_7) %>% summarise(n())
women_raw %>% group_by(S13_03_8) %>% summarise(n())
women_raw %>% group_by(S13_03_9) %>% summarise(n())


# malaria; no freq bednet (most people sleep for 7 days/week)
women_raw %>% group_by(S13_04) %>% summarise(n())
women_raw %>% group_by(S13_04_2) %>% summarise(n())

# ill past 2 weeks
women_raw %>% group_by(S13_04_3) %>% summarise(n())

# dietary diversity calc: 24 h recall from the woman.
# in household data, looks like HDDS_24h should = WDDS
women_raw %>% group_by(AS13_07) %>% summarise(n())
# looks like no NAs


# pull relevant vars ------------------------------------------------------
# remove attributes
women = removeAttributes(women_raw)

women = women %>% 
  select(
    # -- unique ids for merging --
    MHNKEY = KEY, 
    
    # -- antenatal care --
    # Note: for most recent baby, so *NOT NECESSARILY* the child in question
    antenatal_care = S13_03_4,
    S13_03_6, # when received antenatal care
    S13_03_7, # how often rec'd antenatal care
    Fe_supplements  = S13_03_8,
    S13_03_9, # how long (in weeks) took iron supplements
    
    # -- health --
    mother_mosquito_net = S13_04,
    moher_ill_2weeks = S13_04_3,
    
    # -- nutrition --
    dietDiv_W24h = WDDS, # dietary diversity from women's module; 24 h recall
    starch_W24h = AS13_07,
    beans_W24h = BS13_07,
    nuts_W24h = CS13_07,
    milk_W24h = DS13_07,
    protein_W24h = ES13_07,
    eggs_W24h = FS13_07,
    greenVeg_W24h = GS13_07,
    orangeFruits_W24h = HS13_07,
    otherVeg_W24h = IS13_07,
    otherFruit_W24h = JS13_07,
    superCereal_W24h = KS13_07
  )


# Clean women's mod -------------------------------------------------------
women  = women %>% 
  mutate(
    # -- Fix weirdness --
    # some unrealistically large numbers.  converting to NAs. 
    weeks_tookFe = ifelse(S13_03_9 > 39, NA_real_, S13_03_9), 
    
    # -- regroup --
    when_antenatal = case_when(women$S13_03_6 <= 3 ~ 1, # first trimester
                               (women$S13_03_6 > 3 & women$S13_03_6 <= 6 ) ~ 2, # second trimester
                               women$S13_03_6 > 6 ~ 3, # third trimester
                               TRUE ~ NA_real_),
    when_antenatal = forcats::fct_infreq( # sort by frequency
      factor(when_antenatal,
             levels = 1:3,
             labels = c('first trimester', 
                        'second trimester',
                        'third trimester'))),
    
    # Grouping antenatal visits together.  Assuming 50 & 77 visits are "do not know" or something similar
    # Note: technically, this should be treated as a factor.  Treating as pseudo-continuous, with 5 vistis being 5 or more.
    num_antenatal_visits = case_when(women$antenatal_care == 0 ~ 0, # no antenatal care
                                     women$S13_03_7 == 0 ~ 0, # weird b/c claim had antenatal care, but no visits. Only 13 obs. so keeping as 0
                                     women$S13_03_7 == 1 ~ 1,
                                     women$S13_03_7 == 2 ~ 2,
                                     women$S13_03_7 == 3 ~ 3,
                                     women$S13_03_7 == 4 ~ 4,
                                     women$S13_03_7 == 5 ~ 5,
                                     women$S13_03_7 > 20 ~ NA_real_, # unrealistic and unknown
                                     (women$S13_03_7 < 20 & women$S13_03_7 > 5) ~ 5,
                                     TRUE ~ NA_real_)
  ) 
   


# Merge kids + women's ----------------------------------------------------


