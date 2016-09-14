# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_01_importHH.R: import household-level data
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

hh_raw = read_sav(paste0(baseDir, 'cfsva-2015-master-DB- annex.sav'))
# hh2012 = read_sav('~/Documents/USAID/Rwanda/rawdata/RW_2012_CFSVA/cfsvans-2012- household-v01.sav')


# pull relevant vars ------------------------------------------------------

hh = hh_raw %>% 
  mutate(
    # -- household ids / info --
    hhid = KEY,
    int_date = S0_B_DATE,
    month,
    hh_wt = weight,
    hh_wt_norm = normalized_weight,
    
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
    # S1_01_11_C, # polygamous
    
    # -- education --
    S1_01_7, # literate head of household
    S1_01_8, # education of head
    pct_illiterate = S1_01_7_HC0_S, # percent can't read or write (excl. under 7)
    pct_literate = S1_01_7_HC1_S, # percent of house that can read + write (incl. under 7)
    pct_lowEd = S1_01_8_S2, # percent of house with no education or primary (excl. under 7)
    pct_highEd = S1_01_8_S3, # secondary, tertiary, vocational
    
    # -- assets --
    wealth_idx = WI_cat_lyr, # wealth index
    
    # -- food security -- 
    cari_idx = FS_final_lyr # CARI food security index
  ) 


# Clean & recode vars -----------------------------------------------------
# hh$int_month = plyr::mapvalues(hh$livezone_lyr, from = livelihood_zones$codes, to = livelihood_zones$lz)
int_month
admin1-3
hh division m/f

hh = hh %>% 
  mutate(
    
    urban = case_when(hh$Urban == 1 ~ 1,
                      hh$Urban == 2 ~ 0,
                      TRUE ~ NA_integer_),
    fem_head = case_when(hh$S1_01_3 == 1 ~ 0, # male
                      hh$S1_01_3 == 2 ~ 1,
                      TRUE ~ NA_integer_)
  )

# determine what should be base -------------------------------------------
hh_raw %>% group_by(livezone) %>% summarise(num = n()) %>% arrange(desc(num))
# livelihood zone #5 is most frequent therefore will be used as base.
# zone 5 == Central Plateau Cassava and Coffee Zone 

