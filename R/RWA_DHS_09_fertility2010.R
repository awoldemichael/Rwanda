# Rwanda stunting analysis -----------------------------------------
#
# RWA_DHS_09_fertility2010.R: calculate stunting averages for DHS 2010
#
# Script to pull fertility data for Rwanda from the DHS dataset
# 
# Laura Hughes, lhughes@usaid.gov, 30 November 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------


# setup functions/libs ----------------------------------------------------

setwd('~/GitHub/Rwanda/R/')
source('RWA_WFP_00_setup.R')

# Import women’s modules --------------------------------------------------

women10_raw = read_dta(paste0(baseDir, 'RW_2010_DHS/rwir61dt/RWIR61FL.DTA'))

# livelihood zone data from Dr. Essam-- spatial join b/w DHS clusters and FEWS NET 
lz_raw = read_dta('~/Documents/USAID/Rwanda/processeddata/RWA_DHS2010_Livelihoods.dta')

#  hh data, to pull whether own land.
hh10_raw = read_dta(paste0(baseDir, 'RW_2010_DHS/rwhr61dt/RWHR61FL.DTA'))

# pull the useful vars ----------------------------------------------------
w10 = removeAttributes(women10_raw)

w10 = w10 %>% 
  select(
    # -- sampling --
    v005,  #weight
    strata = v022, 
    v001,# dhs cluster
    v002, # hhid
    caseid,
    partner_id = v034,
    psu = v021,
    
    # -- geo --
    v024,
    sdistr,
    altitude = v040, 
    v025, # urban/rural
    
    # -- demographics --
    age = v012,
    age_partner = v730,
    v013,  #ageGroup = 
    v717, #occupGroup = 
    v705,
    v130, #religion = 
    v504, # living w/ partner
    numChildUnd5 = v137,
    v502,# g byte curUnion = (v502 ==1)
    v501, # clonevar maritialStatus = 
    v505,
    
    # -- assets --
    wealthGroup = v190,
    v191,
    
    # -- education --
    educ = v106,
    educDetail  =v107,
    educYears = v133,
     v701, # educPartner
    
    
    # -- family planning --
    v602, #moreKidsWanted = 
    v605, # clonevar moreChild = # Same as v602, but breaks down for when want more kids.
    v621, # clonevar moreChildHus = 
    v621, # husband pref
    idealNum = v613, 
    idealBoys = v627,
    idealGirls = v628,
    v225, # whether current child was wanted
    v367, # whether last child was wanted
    v614, # categorical ideal # kids
    # /* Parity parity is defined as the number of times that she has given 
    # birth to a fetus with a gestational age of 24 weeks or more, 
    # regardless of whether the child was born alive or was stillborn. */
    totChild = v201,
    totLiving = v219,
    sons = v202,
    daughters = v203,
    v313, # v313== 3 modern contraception
    intentionContra = v364,
    sexActivity = v536,
    v531, # age_firstSex = imputed from if responded "when married"
    v624, # unmet need
    contains('v3a08'), # reason not using
    fp_radio = v384a,
    fp_tv = v384b,
    fp_news = v384c,
    
    # -- health --
    bedNetUse = v461,
    pregnant = v213,
    v623, #fecund = 
    
    went_doctor = v394, # went to health facility last 12 mo.
    FPatHealth = v395, # received family planning advice at health facility
    m14_1, # # antenatal visits during pregnancy
    checkup_postPregn = m50_1,
    
    v467b, # permission to go to health clinic difficult
    v467c, # $$ to get to health clinic difficult
    v467d, # access to heatlh clinic difficult
    v467f, # don't go to clinic b/c don't want to
    
    # women's empowerment
    contains('v743'), # is allowed to go places?
    contains('v744'), # when is okay to beat
    contains('v745') # own land/house
  ) %>% 
  mutate(weight = v005 / 1e6,
         wealth = v191 / 1e5,
         
         age_sq = age^2,
         
         # -- create binaries --
         polygamous = case_when(w10$v505 > 0 ~ 1,
                                w10$v505 == 0 ~ 0,
                                TRUE ~ NA_real_),
         
         childDied = (totChild > totLiving),
         
         own_house = case_when(w10$v745a == 0 ~ 0,
                                w10$v745a %in% 1:3 ~ 1,
                                TRUE ~ NA_real_),
         own_land = case_when(w10$v745b == 0 ~ 0,
                             w10$v745b %in% 1:3 ~ 1,
                             TRUE ~ NA_real_),
         
         modernContra = case_when(w10$v313 == 3 ~ 1,
                                  w10$v313 != 3 ~ 0,
                                  TRUE ~ NA_real_),
         
         hasSon = ifelse(sons > 0, 1,
                         ifelse(sons == 0, 0,
                                NA_real_)),
         
         hasDaughter = ifelse(daughters > 0, 1,
                              ifelse(daughters == 0, 0,
                                     NA_real_)),

         goHealth_alone = case_when(w10$v473a == 1 ~ 1,
                                      w10$v473a != 1 ~ 0,
                                      TRUE ~ NA_real_),
         
         health_nopermiss = case_when(w10$v467b == 1 ~ 1,
                                      w10$v467b == 2 ~ 0,
                                      TRUE ~ NA_real_),
         health_money = case_when(w10$v467c == 1 ~ 1,
                                  w10$v467c == 2 ~ 0,
                                  TRUE ~ NA_real_),
         health_dist = case_when(w10$v467d == 1 ~ 1,
                                 w10$v467d == 2 ~ 0,
                                 TRUE ~ NA_real_),
         health_nowanna = case_when(w10$v467f == 1 ~ 1,
                                    w10$v467f == 2 ~ 0,
                                    TRUE ~ NA_real_),
         
         curUnion = case_when(w10$v502 == 1 ~ 1,
                              w10$v502 != 1 ~ 0,
                              TRUE ~ NA_real_),
         
         unmet_wantsBabies = case_when(w10$v624 == 7 ~ 1,
                                       w10$v624 != 7 ~ 0,
                                       TRUE ~ NA_real_),
         unmet_wantsContra = case_when(w10$v624 %in% c(1,2) ~ 1,
                                       !w10$v624 %in% c(1,2) ~ 0,
                                       TRUE ~ NA_real_),
         moreChild_binary = case_when(w10$v602 == 1 ~ 1, # wants another baby
                                      w10$v602 == 3 ~ 0, # wants no more babies
                                      # everything else: undecided, infertile, sterilized, etc. treated as NA
                                      TRUE ~ NA_real_),
         # -- remove NAs --
         beatIf_leaveHouse = na_if(v744a, 8),
         beatIf_neglChild = na_if(v744b, 8),
         beatIf_argues = na_if(v744c, 8),
         beatIf_noSex = na_if(v744d, 8),
         beatIf_burnsFood = na_if(v744e, 8),
         
         beatIf_leaveHouse = na_if(beatIf_leaveHouse, 9),
         beatIf_neglChild = na_if(beatIf_neglChild, 9),
         beatIf_argues = na_if(beatIf_argues, 9),
         beatIf_noSex = na_if(beatIf_noSex, 9),
         beatIf_burnsFood = na_if(beatIf_burnsFood, 9),
         
         fp_radio = na_if(fp_radio, 9),
         fp_tv = na_if(fp_tv, 9),
         fp_news = na_if(fp_news, 9),
         
         educPartner = na_if(v701, 8),
         educPartner = na_if(educPartner, 9),
         
         age_partner = na_if(age_partner, 97),
         age_partner = na_if(age_partner, 98),
         
         age_firstSex = na_if(v531, 97),
         age_firstSex = na_if(age_firstSex, 98),
         age_firstSex = na_if(age_firstSex, 99),
         # -- gaps --
         ideal_pctM = idealBoys/idealNum, # Note: lots of seemingly missing values; 0 B, 0 G reported, but ideal family size > 0. Do not recommend using.
         ageGap = age - age_partner,
         
         moreChild_agree = ifelse(is.na(v621) | v621 == 8, NA,
                                     ifelse(v621 == 1, 1, 0))
         
  ) %>% 
  rowwise() %>% 
  # -- empowerment indices --
  mutate(beating_idx = sum(beatIf_leaveHouse, beatIf_neglChild, 
                           beatIf_argues, beatIf_noSex, beatIf_burnsFood, na.rm = TRUE)) %>% 

  ungroup() %>% 
  factorize(women10_raw, 'v024', 'province') %>% 
  factorize(women10_raw, 'sdistr', 'district') %>% 
  factorize(women10_raw, 'v025', 'rural') %>% 
  factorize(women10_raw, 'v130', 'religion') %>% 
  factorize(women10_raw, 'v013', 'ageGroup') %>% 
  factorize(women10_raw, 'v717', 'occupGroup') %>%  
  factorize(women10_raw, 'v705', 'occupHusGroup') %>% 
  factorize(women10_raw, 'v623', 'fecund') %>% 
  factorize(women14_raw, 'v367', 'wantedChild') %>% 
  factorize(women10_raw, 'v602', 'moreChild') %>% 
  factorize(women10_raw, 'v621', 'moreChildHus') %>%  
  factorize(women10_raw, 'v624', 'unmetNeed') %>%  
  
  arrange()


# -- manual interaction --
w10 = w10 %>% mutate(age_rural = paste(rural, ageGroup, sep = '_'))

w10$age_rural = factor(w10$age_rural, levels = c('rural_35-39', unique(w10$age_rural)))

# refactor ----------------------------------------------------------------
# age cats
w10$ageGroup = fct_relevel(w10$ageGroup, "35-39")

# lump together infrequent religions
w10$religion = fct_lump(w10$religion, n = 4)
w10$religion = fct_relevel(w10$religion, 'catholic')
# g educGap = (educ - educPartner) if !missing(educPartner)
# g educGapDetail = v133 - v715 if !missing(v715) & v715!= 98

w10$occup_cat = fct_collapse(w10$occupGroup, 
                             prof = c('professional/technical/managerial', 'household and domestic', 'services', 'clerical'))


# clean/merge hh data -----------------------------------------------------
hh10 = hh10_raw %>% 
  dplyr::select(v001 = hv001,
         v002 = hv002, 
         hh_ownland = hv244)

w10 = left_join(w10, hh10)

sum(is.na(hh10_raw$hv244))
sum(is.na(w10$hh_ownland))

# merge in livelihood zone names ------------------------------------------
sum(is.na(lz_raw$lvdzone)) # no lvdzone missing.

lz = removeAttributes(lz_raw) %>% select(-district, -province, -rural)
lz = factorize(lz, lz_raw, 'lvdzone', 'lvdzone')

# merge by DHS cluster and hh id.
w10 = left_join(w10, lz, by = c('v001', 'v002', 'strata', 'altitude', 'psu', 'numChildUnd5', 'wealthGroup', 'wealth'))
sum(is.na(w10$lvdzone)) 


# Filter women ------------------------------------------------------------
# currently in union.
w10 = w10 %>% 
  filter(curUnion == 1)


# check I got all the NA values -------------------------------------------
# DHS uses 9, 96, 97, 98, 99, 999, etc. to encode NAs. Checking I didn't miss any:
na_check = data.frame(tot = t(w10 %>%
                                select(
                                  # -- demographics --
                                  age, rural ,
                                  ageGap ,
                                  religion ,
                                  age_firstSex ,
                                  
                                  # numChildUnd5 ,
                                  totLiving, hasSon ,
                                  
                                  # hasSon , 
                                  # hasDaughter ,
                                  
                                  # -- education --
                                  educ ,
                                  educPartner ,
                                  # occup_cat ,
                                  # occupHusGroup ,
                                  
                                  # -- wealth --
                                  wealth , 
                                  
                                  # -- geo / connectivity --
                                  altitude ,
                                  # rural ,
                                  
                                  # -- health -- 
                                  bedNetUse , 
                                  went_doctor , 
                                  # FPatHealth , # too many unobs? ~ 1/2 didn't go to doc.
                                  health_dist ,
                                  health_money ,
                                  # goHealth_alone , # too many unobs.
                                  fp_radio ,
                                  fp_tv ,
                                  fp_news ,
                                  
                                  # -- empowerment --
                                  own_land ,
                                  own_house ,
                                  beating_idx) %>% 
                                summarise_all(funs(sum(. > 50 | . %in% c(8,9), na.rm = TRUE))))) 
na_check %>% mutate(var = row.names(na_check)) %>% filter(tot>0)



