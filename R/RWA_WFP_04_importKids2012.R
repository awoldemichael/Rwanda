# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_04_importKids2012.R: import child-level data from 2012 dataset
#
# Script to pull stunting data and associated household- or
# child-level data for Rwanda from the CFSVA dataset
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 14 November 2016
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


ch2012_raw = read_sav(paste0(baseDir, 'RW_2012_CFSVA/cfsvans-2012- children-v01.sav'))


# 2009 data are unprocessed.  Also doesn't include Kigali / urban areas.
# ch2009 = read_sav(paste0(baseDir, 'RW_2009_CFSVA/Section 13 enfants.sav'))



# Checking how weights should be applied ----------------------------------



# Notes on data -----------------------------------------------------------

# stunting is based on 2006 WHO children's growth standards
# stunting was calculated by WFP in SPSS.
# Key stunting variables for 2015 data is G_stunted (binary stunted) and HAZWHO (height-for-age z-score based on the WHO distribution)

# select variables --------------------------------------------------------
# Majority of household explanatory variables will be pulled from the household-level data.
ch2012 = ch2012_raw %>% 
  select(
    # -- survey data --
    childID, 
    hh_id,
    weight = FINAL_norm_weight,
    strata_ID,
    interview_date = Q001_2,
    mother_id = Q202_03, # mother number from maternal section
    
    # -- geo --
    p_code, #admin1
    d_code, #admin2
    s_code, #admin3
    v_code, # village
    t_code, # team (enumerator?)
    final_urban,
    # fews_code, # fews code seems wrong for 19 kids; labeled as being Kigali, but in fews_hh are listed as being Agropastoral. Looking into it-- the 19 hh are in the Eastern province, which doesn't make sense.  So it must be wrong.
    
    # -- child demographics --
    age_months = MONTHS_NEW,
    Q202_09, # sex
    Q202_13, # birth weight (sorta)
    
    # -- hh demographics --
    hh_size = hh_size_computed, # same as HH_size
    livelihood, # head occupation
    kids_under5 = under5,
    Q102, # femheaded
    head_age  = Q103, # head age
    numWomen_18_59 = QG110_2, # fem 18-59 y old
    numWomen_60plus = QH110_2, # fem 60+
    rooms_hh = Q204, # sleeping rooms in hh
    
    # -- education --
    Q105, # head ed
    
    # -- WASH --
    Q206, # share toilet
    Q215_1, # time to water, in min.
    impr_toilet = improved_toilet, 
    impr_water = improved_water,
    
    # -- health --
    # Q202_18, # ill w/ diarrhea past 2 weeks.  Not sure why is tagged as -1, 0, 1. Using Diarrhoea.  What seems to be the case: -1 == no illness in the past 2 weeks.  0 is 0, 1 is 1.  obv.
    diarrhea = Diarrhoea,
    # -- ag --
    # Q401_2, # land size Substitutes NA for no land; using land size from hh module
    own_livestock = Q411,
    # TLU: QA412-QH412
    contains('412_1'), # All animals owned

    # -- food --
    # Q1002-- food security q's
    FCS,
    CSI = CSI_reduced, # numeric CSI index
    
    # -- nutrition --
    isStunted = G_Stunted,
    stuntingZ = HAZWHO,
    
    # -- shocks -- (ignoring 3rd and 4th)
    Q1105_1_1, # first shock
    Q1105_2_1 # second shock
  )




# Applying sampling weights ---------------------------------------------------------------
# !!! PROBLEM: strata_ID has missing values and doesn't seem to match the 2015 sampling frame.
# 16 strata, not 30 (?)
# Missing strata are all Kigali.
# All strata grouped by districts, but some strata have multiple districts
# Suggests 2012 isn't representative at the district level.

# cfsva2012 = svydesign(id = ~v_code, strata = ~d_code, weights = ~weight, data = ch_hh2012)
# stunting_lz_2012 = svyby(~isStunted, design = cfsva2012, by = ~livelihood_zone, svymean, na.rm = TRUE)
# svyby(~G_Stunted, design = cfsva2012, by = ~d_code, svymean, na.rm = TRUE)
# 
# # strata_ID has 495 missing values (?)
# stunting_lz_2012 = calcPtEst(df = ch2012, var = 'G_Stunted', by_var = 'fews_code', 
#                              psu_var = 'v_code', 
#           strata_var = 'd_code', weight_var = 'FINAL_norm_weight')
# 
# stunting_lz_2012 = factorize(stunting_lz_2012, ch2012_raw, 'fews_code', 'livelihood_zone')
# stunting_lz_2012 = stunting_lz_2012 %>% 
#   mutate(livelihood_zone = ifelse(livelihood_zone %like% 'Kigali', 'Kigali city',
#                                   as.character(livelihood_zone)))

# merge w/ 2015 -----------------------------------------------------------
  
ggplot(stunting_comb) +
  geom_segment(aes(x = `2012`, xend  = y2, 
                   y = livelihood_zone, yend = livelihood_zone),
               size = 0.5, 
               arrow = arrow(length = unit(0.03, "npc")),
               colour = grey60K, 
               data = stunting_untidy) +
  geom_point(aes(x = stunting, y = livelihood_zone,
                 color = year, shape = year, fill = year),
             size = 8, colour = grey90K) +
  geom_point(aes(x = b, y = livelihood_zone),
             # width = 0,
             size = 8, colour = grey90K,
             fill = brewer.pal(9, 'Spectral')[9],
             shape = 23,
             data = stunting_lz_dhs) +
  # geom_text(aes(x = stunting, y = livelihood_zone,
  #                color = year, shape = year, fill = year,
  #                label = percent(stunting, 0)),
  #           colour = grey75K,
  #           size = 3) +
  theme_xgrid() +
  scale_shape_manual(values = c(21, 23)) +
  scale_x_continuous(labels = percent, limits = c(0.2, 0.62),
                     breaks = seq(0.2, 0.6, by = 0.2)) +
  scale_fill_manual(values = c('2012' = 'white', '2015' = brewer.pal(9, 'Spectral')[1])) +
  theme(axis.text.y = element_text(size = 10),
        axis.title.x = element_blank())



# Vars not available in 2012 ----------------------------------------------


# ! NOTE: no antenatal care

# + elevation
