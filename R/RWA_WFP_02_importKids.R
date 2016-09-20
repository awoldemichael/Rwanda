# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_02_importKids.R: import children's data
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

children_raw = read_sav(paste0(baseDir, 'cfsva-2015-child-DB- annex.sav'))




# Notes on data -----------------------------------------------------------

# stunting is based on 2006 WHO children's growth standards
# stunting was calculated by WFP in SPSS.
# Key stunting variables for 2015 data is Stunted_global (binary stunted) and HAZWHO (height-for-age z-score based on the WHO distribution)

# select variables --------------------------------------------------------
# Majority of household explanatory variables will be pulled from the household-level data.
ch = children_raw %>% 
  select(
    # -- IDs --
    child_id = CHN_KEY,
    parent_id = PARENT_KEY,
    village = S0_G_Vill, # village (746 villages)
    weight,
    normalized_weight_CHILD,
    livezone_lyr,
    
    # -- demographics --
    WI, # numeric wealth index
    S14_02_2, # primary caregiver
    age_months = S14_02_7, # age
    S14_02_8, # sex
    mother_age = S13_02_2,
    S13_02_3, # mother read/write
    # S13_02_4, # mother's education
    mother_education = education_groups, # classified mother's education
    # stunted/underweight mother is very rare (69 or 161)
    BMI, # mother's BMI
    underweight_women, # mother is underweight
    stunted_women, # mother is stunted
    
    # -- nutrition --
    S14_03, # ever breastfed
    S14_03_2, # hours after birth breastfed
    S14_03_4, # given food/drink other than breastmilk in first 6 mo.
    S14_03_5, # still breastfed
    # Children food consumption only for those who are currently breastfed (consumption, minimal acceptable diet, in feeding programs)
    
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
    Wasted_global, Stunted_global, Underweight_global, # binaries (yes or no)
    Wasted, Stunted, Underweight, # normal, moderate, severe
    WHO_Flag, # whether child has height/weight measured
    HAZNCHS, # stunting score based on NCHS standards
    HAZWHO
    
  )


# clean vars --------------------------------------------------------------

ch = ch %>% 
  mutate(
    
    # -- Replace NAs --
    ill_fortnight = na_if(S14_05, 88),
    fever = na_if(S14_05_2, 88),
    cough = na_if(S14_05_3, 88),
    diarrhea = na_if(S14_05_4, 88),
    dewormed = na_if(S14_05_6, 88)
  )

# old stuff ---------------------------------------------------------------

# 2012 data
stuntingDist12 = ch2012 %>% filter(!is.na(G_Stunted)) %>% group_by(dist) %>% summarise(avg = mean(G_Stunted), 
                                                                                            std = sd(G_Stunted),
                                                                                            num = n(),
                                                                                            se = std / (sqrt(num)),
                                                                                            lb = avg - ciFactor * se,
                                                                                            ub = avg + ciFactor * se) %>% 
  arrange(desc(avg))

# 2009 data
stuntingDist09 = ch2009 %>% filter(!is.na(G_Stunted)) %>% group_by(fews_code) %>% summarise(avg = mean(G_Stunted), 
                                                                                            std = sd(G_Stunted),
                                                                                            num = n(),
                                                                                            se = std / (sqrt(num)),
                                                                                            lb = avg - ciFactor * se,
                                                                                            ub = avg + ciFactor * se) %>% 
  arrange(desc(avg)) %>% 
  mutate(dist = fews_code)

# clean children’s data ---------------------------------------------------

# metadata = lapply(children_raw, function(x) attr(x, 'label'))

dists = data.frame(codes = attr(children_raw$S0_D_Dist_lyr, 'labels')) 
dists = dists %>% 
  mutate(dist = row.names(dists))


stuntingDist$dist = plyr::mapvalues(stuntingDist$dist, from = dists$codes, to = dists$dist)

livelihood_zones = data.frame(codes = attr(children_raw$livezone_lyr, 'labels')) 
livelihood_zones = livelihood_zones %>% 
  mutate(lz = row.names(livelihood_zones))

ch$dist = plyr::mapvalues(ch$livezone_lyr, from = livelihood_zones$codes, to = livelihood_zones$lz)
ch2012$dist = plyr::mapvalues(ch2012$fews_code, from = livelihood_zones$codes, to = livelihood_zones$lz)

# clean mother_raw --------------------------------------------------------


# children's dataset includes lots of duplicated variables.
write.csv()





# remove children without stunting scores ---------------------------------

ch = children_raw %>% filter(! is.na(Stunted_global))

ciFactor = 1.96

stuntingDist = ch %>% filter(! is.na(Stunted_global)) %>% 
  group_by(dist = livezone_lyr) %>% summarise(avg = mean(Stunted_global), 
                                                                  std = sd(Stunted_global),
                                                                  num = n(),
                                                                  se = std / (sqrt(num)),
                                                                  lb = avg - ciFactor * se,
                                                                  ub = avg + ciFactor * se) %>% 
  arrange(desc(avg))


stuntingDist = removeAttributes(stuntingDist)

stuntingDist$dist = factor(stuntingDist$dist, levels = rev(stuntingDist$dist))

ggplot(stuntingDist) +
  geom_pointrange(aes(x = dist, y = avg, ymin = lb, ymax = ub)) +
  coord_flip() +
  ylim(c(0.15, 0.72))

s = left_join(stuntingDist, stuntingDist12, by = c('dist' = 'dist'))
ggplot(s %>% filter(dist %in% c()), aes(x = `avg.y`, xend = `avg.x`, y = 2012, yend = 2015,
                                             colour = as.character(dist))) +
  geom_segment() + coord_flip() + 
  geom_text(aes(label = as.character(dist), x = `avg.y`, y = 2015), nudge_x = 0.1, size = 5)

ggplot(s, aes(x = 2012, xend = 2015, y = avg.y, yend = avg.x, colour = factor(dist))) +
  geom_segment()

# determine what should be base -------------------------------------------
hh_raw %>% group_by(livezone) %>% summarise(num = n()) %>% arrange(desc(num))
# livelihood zone #5 is most frequent therefore will be used as base.
# zone 5 == Central Plateau Cassava and Coffee Zone

# older datasets ----------------------------------------------------------
ch2012 = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2012_CFSVA/cfsvans-2012- children-v01.sav')

# 2009 data are unprocessed.  Also doesn't include Kigali
ch2009 = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2009_CFSVA/Section 13 enfants.sav')


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

# Note: straight averages don't work:
x = ch %>% mutate(st = Stunted_global * normalized_weight_CHILD)

# Similar but not right
x  %>% group_by(Urban) %>% summarise(tot = sum(Stunted_global), n = n()) %>% mutate(pct = tot/n)

library(survey)

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
