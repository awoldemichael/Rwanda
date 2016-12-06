# Rwanda stunting analysis -----------------------------------------
#
# RWA_DHS_08_fertility.R: calculate stunting averages for DHS 2010 and 2014/2015 data
#
# Script to pull fertility data for Rwanda from the DHS dataset
# 
# Laura Hughes, lhughes@usaid.gov, 30 November 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------


# Notes: ------------------------------------------------------------------
# * Unmet need is classified into 10 categories:
  # * never had sex (v624 == 0)
  # * unmet need for spacing (v624 == 1): v225 == 2
  # * unmet need for limiting (v624 == 2): v225 == 3
  # *   using for spacing (v624 == 3): v312!=0 + using contracption
  # * using for limiting (v624 == 4): v312!=0 & (v605>=5 & v605<=7) + using contraception
  # * spacing failure (v624 == 5)
  # * limiting failure (v624 == 6)
  # * no unmet need (v624 == 7): wanted last pregnancy, wants baby w/i 2 y (v605 == 1)
  # * never had sex (v624 == 8): not married, not sexually active w/i 30 days
  # * infecund, menopausal (v624 == 9)

# setup functions/libs ----------------------------------------------------

setwd('~/GitHub/Rwanda/R/')
source('RWA_WFP_00_setup.R')

# Import women’s modules --------------------------------------------------

women14_raw = read_dta(paste0(baseDir, 'RW_2014-15_DHS/rwir70dt/RWIR70FL.DTA'))

# livelihood zone data from Dr. Essam-- spatial join b/w DHS clusters and FEWS NET 
lz_raw = read_dta('~/Documents/USAID/Rwanda/processeddata/RWA_DHS_Livelihoods.dta')

# hh module-- to pull whether hh owns land
hh14_raw = read_dta(paste0(baseDir, 'RW_2014-15_DHS/rwhr70dt/RWHR70FL.DTA'))

# Men's modules / couples modules investigated, but discarded, since too little overlap w/ women's modules
# men14_raw = read_dta(paste0(baseDir, 'RW_2014-15_DHS/rwmr70dt/RWMR70FL.DTA'))
# men10_raw = read_dta(paste0(baseDir, 'RW_2010_DHS/rwmr70dt/RWMR70FL.DTA'))
# 
# couples14 = read_dta('~/Documents/USAID/Rwanda/rawdata/RW_2014-15_DHS/rwcr70dt/RWCR70FL.DTA')



# pull the useful vars ----------------------------------------------------
w14 = removeAttributes(women14_raw)

w14 = w14 %>% 
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
    sdistrict,
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
    v225, # whether current child was wanted
    v367, # whether last child was wanted
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
         polygamous = case_when(w14$v505 > 0 ~ 1,
                                w14$v505 == 0 ~ 0,
                                TRUE ~ NA_real_),
         
         childDied = (totChild > totLiving),
         
         wanted_last = case_when(w14$v225 == 1 ~ 1,
                                w14$v225 > 1 ~ 0,
                                TRUE ~ NA_real_),
         
         own_house = case_when(w14$v745a == 0 ~ 0,
                               w14$v745a %in% 1:3 ~ 1,
                               TRUE ~ NA_real_),
         own_land = case_when(w14$v745b == 0 ~ 0,
                              w14$v745b %in% 1:3 ~ 1,
                              TRUE ~ NA_real_),
         
         modernContra = case_when(w14$v313 == 3 ~ 1,
                                  w14$v313 != 3 ~ 0,
                                  TRUE ~ NA_real_),
         
         hasSon = ifelse(sons > 0, 1,
                         ifelse(sons == 0, 0,
                                NA_real_)),
         
         hasDaughter = ifelse(daughters > 0, 1,
                              ifelse(daughters == 0, 0,
                                     NA_real_)),
         
         goHealth_alone = case_when(w14$v473a == 1 ~ 1,
                                    w14$v473a != 1 ~ 0,
                                    TRUE ~ NA_real_),
         
         health_nopermiss = case_when(w14$v467b == 1 ~ 1,
                                      w14$v467b == 2 ~ 0,
                                      TRUE ~ NA_real_),
         health_money = case_when(w14$v467c == 1 ~ 1,
                                  w14$v467c == 2 ~ 0,
                                  TRUE ~ NA_real_),
         health_dist = case_when(w14$v467d == 1 ~ 1,
                                 w14$v467d == 2 ~ 0,
                                 TRUE ~ NA_real_),
         health_nowanna = case_when(w14$v467f == 1 ~ 1,
                                    w14$v467f == 2 ~ 0,
                                    TRUE ~ NA_real_),
         
         curUnion = case_when(w14$v502 == 1 ~ 1,
                              w14$v502 != 1 ~ 0,
                              TRUE ~ NA_real_),
         
         unmet_wantsBabies = case_when(w14$v624 == 7 ~ 1,
                                       w14$v624 != 7 ~ 0,
                                       TRUE ~ NA_real_),
         unmet_wantsContra = case_when(w14$v624 %in% c(1,2) ~ 1,
                                       !w14$v624 %in% c(1,2) ~ 0,
                                       TRUE ~ NA_real_),
         moreChild_binary = case_when(w14$v602 == 1 ~ 1, # wants another baby
                                      w14$v602 == 3 ~ 0, # wants no more babies
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
         
         educPartner = na_if(v701, 8),
         educPartner = na_if(educPartner, 9),
         
         fp_radio = na_if(fp_radio, 9),
         fp_tv = na_if(fp_tv, 9),
         fp_news = na_if(fp_news, 9),
         
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
  
  factorize(women14_raw, 'v024', 'province') %>% 
  factorize(women14_raw, 'sdistrict', 'district') %>% 
  factorize(women14_raw, 'v025', 'rural') %>% 
  factorize(women14_raw, 'v130', 'religion') %>% 
  factorize(women14_raw, 'v013', 'ageGroup') %>% 
  factorize(women14_raw, 'v717', 'occupGroup') %>%  
  factorize(women14_raw, 'v705', 'occupHusGroup') %>% 
  factorize(women14_raw, 'v623', 'fecund') %>% 
  factorize(women14_raw, 'v602', 'moreChild') %>% 
  factorize(women14_raw, 'v367', 'wantedChild') %>% 
  factorize(women14_raw, 'v621', 'moreChildHus') %>%  
  factorize(women14_raw, 'v624', 'unmetNeed') %>%  
  
  arrange()


# -- manual interaction --
w14 = w14 %>% mutate(age_rural = paste(rural, ageGroup, sep = '_'))

w14$age_rural = factor(w14$age_rural, levels = c('rural_35-39', unique(w14$age_rural)))

# refactor ----------------------------------------------------------------
# age cats
w14$ageGroup = fct_relevel(w14$ageGroup, "35-39")

# lump together infrequent religions
w14$religion = fct_lump(w14$religion, n = 4)
w14$religion = fct_relevel(w14$religion, 'catholic')
# g educGap = (educ - educPartner) if !missing(educPartner)
# g educGapDetail = v133 - v715 if !missing(v715) & v715!= 98

w14$occup_cat = fct_collapse(w14$occupGroup, 
                             prof = c('professional/technical/managerial', 'household and domestic', 'services', 'clerical'))

# clean/merge hh data -----------------------------------------------------
hh14 = hh14_raw %>% 
  dplyr::select(v001 = hv001,
                v002 = hv002, 
                hh_ownland = hv244)

w14 = left_join(w14, hh14)

sum(is.na(hh14_raw$hv244))
sum(is.na(w14$hh_ownland))

# merge in livelihood zone names ------------------------------------------
sum(is.na(lz_raw$lvdzone)) # no lvdzone missing.

lz = removeAttributes(lz_raw) %>% select(-district, -province, -rural)
lz = factorize(lz, lz_raw, 'lvdzone', 'lvdzone')

# merge by DHS cluster and hh id.
w14 = left_join(w14, lz, by = c('v001', 'v002', 'strata', 'altitude', 'psu', 'numChildUnd5', 'wealthGroup', 'wealth'))
sum(is.na(w14$lvdzone)) 

# men’s mod ---------------------------------------------------------------
# m14 = removeAttributes(men14_raw)
# 
# m14 = m14 %>% 
#   select(
#     # -- sampling --
#     mv005,  # weight
#     strata = mv022, 
#     dhsclust = mv001,
#     mcaseid,
#     # partner_id = mv034,
#     hhid = mv002,
#     m_linenum = mv003,
#     psu = mv021,
#     
#     # -- contraception --
#     # contraception knowledge (mv301) high
#     mv313, # current method using
#     mv384a, # radio abt FP
#     mv384b, # tv
#     mv384c, # newspaper
#     mv395, # talked about FP w/ health worker
#     mv3b25a, # male attitudes about whose responsibility contra is are roughly egalitarian.
#     mv3b25b, # similar for promiscuity / contr use -- men (mostly) okay w/ them
#     
#     # -- family planning ---
#     idealNum_hus = mv613, 
# idealNum_husGrp = mv614
# )

# Filter women ------------------------------------------------------------
# currently in union.
w14 = w14 %>% 
  filter(curUnion == 1)


# check I got all the NA values -------------------------------------------
# DHS uses 9, 96, 97, 98, 99, 999, etc. to encode NAs. Checking I didn't miss any:
# na_check = data.frame(tot = t(w14 %>% summarise_all(funs(sum(. > 50, na.rm = TRUE))))) 
# na_check %>% mutate(var = row.names(na_check)) %>% filter(tot>0)

# na_check = data.frame(tot = t(w14 %>% summarise_all(funs(sum(. %in% c(8, 9), na.rm = TRUE))))) 
# na_check %>% mutate(var = row.names(na_check)) %>% filter(tot>0)

# merge men/women ---------------------------------------------------------
# Intending to pull the men's module data to merge with women's, to see if there's any relationship b/w
# the limited data the dhs collects about men's perceptions of contraception and family planning.
# HOWEVER-- it looks as though only a small number of women (~2904) had their partners interviewed as well.
# Verified by the couples' module. Since that throws out ~ 1/2 of the sample, ignoring.
# comb = left_join(w14, m14, by = c('dhsclust', 'hhid', 'partner_id' = 'm_linenum', 'strata', 'psu'))
# 
# # canary checks
# sum(is.na(m14$mv313)) # 0 missing.
# sum(is.na(comb$mv313))
# 
# comb = comb %>% 
#   mutate(diff_idealNum = idealNum - idealNum_hus)
# 
# ggplot(removeAttributes(comb), aes(x = diff_idealNum, fill = factor(diff_idealNum > 0))) +
#   geom_histogram(binwidth = 1) +
#   xlim(c(-10, 10)) +
#   theme_ygrid()
#   
# ggplot(removeAttributes(comb), aes(x = idealNum_hus, y = idealNum)) +
#   geom_abline(slope = 1, intercept = 0, color = 'red', size = 0.5) +
#   geom_point(size = 5, alpha = 0.03) +
#   coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
#   theme_xygrid()


# For those w/ unmet need— why not using condoms? -------------------------
why_unmet = data.frame(tot = t( w14 %>% 
                                  filter(unmet_wantsContra == 1) %>% 
                                  select(contains('v3a08')) %>% 
                                  summarise_each(funs(sum(., na.rm = TRUE))))) 

why_unmet%>% 
  mutate(var = row.names(why_unmet)) %>% 
  arrange(desc(tot))


