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


# setup functions/libs ----------------------------------------------------

setwd('~/GitHub/Rwanda/R/')
source('RWA_WFP_00_setup.R')

# Import women’s modules --------------------------------------------------

women14_raw = read_dta(paste0(baseDir, 'RW_2014-15_DHS/rwir70dt/RWIR70FL.DTA'))
women10_raw = read_dta(paste0(baseDir, 'RW_2010_DHS/rwir61dt/RWIR61FL.DTA'))

# livelihood zone data from Dr. Essam-- spatial join b/w DHS clusters and FEWS NET 
lz = read_dta('~/Documents/USAID/Rwanda/processeddata/RWA_DHS_Livelihoods.dta')

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
    dhsclust = v001,
    caseid,
    partner_id = v034,
    hhid = v002,
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
    v614, # categorical ideal # kids
    # /* Parity parity is defined as the number of times that she has given 
    # birth to a fetus with a gestational age of 24 weeks or more, 
    # regardless of whether the child was born alive or was stillborn. */
    totChild = v201,
    v313, # v313== 3 modern contraception
    intentionContra = v364,
    sexActivity = v536,
    v624, # unmet need
    contains('v3a08'), # reason not using
    
    # -- health --
    bedNetUse = v461,
    pregnant = v213,
    fecund = v623,
    v467d # access to heatlh clinic difficult
  ) %>% 
  mutate(weight = v005 / 1e6,
         wealth = v191 / 1e5,
         
         # -- create binaries --
         modernContra = case_when(w14$v313 == 3 ~ 1,
                                  w14$v313 != 3 ~ 0,
                                  TRUE ~ NA_real_),
         
         curUnion = case_when(w14$v502 == 1 ~ 1,
                              w14$v502 != 1 ~ 0,
                              TRUE ~ NA_real_),
         
         unmet_wantsBabies = case_when(w14$v624 == 7 ~ 1,
                                       w14$v624 != 7 ~ 0,
                                       TRUE ~ NA_real_),
         unmet_wantsContra = case_when(w14$v624 %in% c(1,2) ~ 1,
                                       !w14$v624 %in% c(1,2) ~ 0,
                                       TRUE ~ NA_real_)
  ) %>% 
  factorize(women14_raw, 'v024', 'province') %>% 
  factorize(women14_raw, 'sdistrict', 'district') %>% 
  factorize(women14_raw, 'v025', 'rural') %>% 
  factorize(women14_raw, 'v130', 'religion') %>% 
  factorize(women14_raw, 'v013', 'ageGroup') %>% 
  factorize(women14_raw, 'v717', 'occupGroup') %>%  
  factorize(women14_raw, 'v705', 'occupHusGroup') %>% 
  factorize(women14_raw, 'v602', 'moreChild') %>% 
  factorize(women14_raw, 'v621', 'moreChildHus') %>%  
  factorize(women14_raw, 'v624', 'unmetNeed') %>%  
  
  arrange()
# g byte educSame = (educPartner == educ) if !missing(educPartner)
# g educGap = (educ - educPartner) if !missing(educPartner)
# g educGapDetail = v133 - v715 if !missing(v715) & v715!= 98
# g ageGap = v012 - v730


# men’s mod ---------------------------------------------------------------
m14 = removeAttributes(men14_raw)

m14 = m14 %>% 
  select(
    # -- sampling --
    mv005,  # weight
    strata = mv022, 
    dhsclust = mv001,
    mcaseid,
    # partner_id = mv034,
    hhid = mv002,
    m_linenum = mv003,
    psu = mv021,
    
    # -- contraception --
    # contraception knowledge (mv301) high
    mv313, # current method using
    mv384a, # radio abt FP
    mv384b, # tv
    mv384c, # newspaper
    mv395, # talked about FP w/ health worker
    mv3b25a, # male attitudes about whose responsibility contra is are roughly egalitarian.
    mv3b25b, # similar for promiscuity / contr use -- men (mostly) okay w/ them
    
    # -- family planning ---
    idealNum_hus = mv613, 
    idealNum_husGrp = mv614
    )

# Filter women ------------------------------------------------------------
# currently in union.
w14 = w14 %>% 
  filter(curUnion == 1)


# check I got all the NA values -------------------------------------------
# DHS uses 9, 96, 97, 98, 99, 999, etc. to encode NAs. Checking I didn't miss any:
na_check = data.frame(tot = t(w14 %>% summarise_all(funs(sum(. > 50, na.rm = TRUE))))) 
na_check %>% mutate(var = row.names(na_check)) %>% filter(tot>0)

na_check = data.frame(tot = t(w14 %>% summarise_all(funs(sum(. == 9, na.rm = TRUE))))) 
na_check %>% mutate(var = row.names(na_check)) %>% filter(tot>0)

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


