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

# ch2012 = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2012_CFSVA/cfsvans-2012- children-v01.sav')
# ch2009 = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2009_CFSVA/Section 13 enfants.sav')


# Notes on data -----------------------------------------------------------

# stunting is based on 2006 WHO children's growth standards
# stunting was calculated by WFP in SPSS.

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
    
    # -- demographics --
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
stuntingDist12 = ch2012 %>% filter(!is.na(G_Stunted)) %>% group_by(fews_code) %>% summarise(avg = mean(G_Stunted), 
                                                                                            std = sd(G_Stunted),
                                                                                            num = n(),
                                                                                            se = std / (sqrt(num)),
                                                                                            lb = avg - ciFactor * se,
                                                                                            ub = avg + ciFactor * se) %>% 
  arrange(desc(avg)) %>% 
  mutate(dist = fews_code)

# clean children’s data ---------------------------------------------------

# metadata = lapply(children_raw, function(x) attr(x, 'label'))

dists = data.frame(codes = attr(children_raw$S0_D_Dist_lyr, 'labels')) %>% 
  mutate(dist = row.names(dists))


stuntingDist$dist = plyr::mapvalues(stuntingDist$dist, from = dists$codes, to = dists$dist)

livelihood_zones = data.frame(codes = attr(children_raw$livezone_lyr, 'labels')) 
livelihood_zones = livelihood_zones %>% 
  mutate(lz = row.names(livelihood_zones))

ch$livezone_lyr = plyr::mapvalues(ch$livezone_lyr, from = livelihood_zones$codes, to = livelihood_zones$lz)

# clean mother_raw --------------------------------------------------------


# children's dataset includes lots of duplicated variables.
write.csv()





# remove children without stunting scores ---------------------------------

ch = children_raw %>% filter(! is.na(Stunted_global))

ciFactor = 1.96

stuntingDist = ch %>% group_by(dist = livezone_lyr) %>% summarise(avg = mean(Stunted_global), 
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

s = full_join(stuntingDist, stuntingDist12, by = 'dist')
ggplot(s %>% filter(dist %in% c(12, 2)), aes(x = `avg.y`, xend = `avg.x`, y = 2012, yend = 2015,
                                             colour = as.character(dist))) +
  geom_segment() + coord_flip() + 
  geom_text(aes(label = as.character(dist), x = `avg.y`, y = 2015), nudge_x = 0.1, size = 5)

# determine what should be base -------------------------------------------
hh_raw %>% group_by(livezone) %>% summarise(num = n()) %>% arrange(desc(num))
# livelihood zone #5 is most frequent therefore will be used as base.
# zone 5 == Central Plateau Cassava and Coffee Zone 

