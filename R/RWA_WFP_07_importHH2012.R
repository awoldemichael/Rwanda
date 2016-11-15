# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_07_importHH2012.R: import household-level data from 2012 dataset
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


# Previous dependencies ---------------------------------------------------
# source('RWA_WFP_00_setup.R')
# source('RWA_WFP_04_importKids2012.R')

hh2012_raw = read_sav(paste0(baseDir, 'RW_2012_CFSVA/cfsvans-2012- household-v01.sav'))


# 2009 data are unprocessed.  Also doesn't include Kigali / urban areas.
# ch2009 = read_sav(paste0(baseDir, 'RW_2009_CFSVA/Section 13 enfants.sav'))







# select variables --------------------------------------------------------

hh2012 =  hh2012_raw %>% 
  select(
    # -- basics --
    hh_id,
    int_date = inter_date,
    month = Interview_Cat,
    weight = FINAL_norm_weight,
    strata_ID,
    
    
    wb_site = LWH, # World Bank Intevention Sites
    
    # -- demographics --
    hh_size = HH_size, # same as hh_size_computed
    crowding = crowding_index,
    Q102, # female headed
    head_age  = Q103,
    
    # -- education --
    Q104, # literate head
    Q105, # education head
    
    # -- WASH --
    #Q205_1, # type of toilet
    improved_water,
    improved_toilet,
    Q206, # shared toilet
    # Q209_1, # water source
    Q215_1, # distance from water
    Q216, # water treatment
    
    # -- geography --
    p_code, d_code, s_code, v_code, # province, district
    Urban, Urban_NEW, final_urban, int_urban,
    agro_climat, 
    fews_code,
    Q_altitude, # village height in m
    Q_sfi, # soil fertility index
    Q_se, # soil erosion
    suitability, # slope suitability
    
    # -- food --
    CSI_cat = CSI_categories, 
    months_food_access = Months_FA,
    
    FCS, FCS_category,
    
    starch_days = Starches, 
    meat_days = Protein, 
    milk_days = Milk,
    oil_days = Oil,
    veg_days = Vegs,
    fruit_days = Fruits,
    pulse_days = Pulses,
    sugar_days = Sugar,
    
    # -- wealth --
    WI_cat = NFAC1_1, GWI,
    
    # -- livelihoods --
    livelihood_NEW,
    livelihood,
    num_jobs = Q301, #! check same def
    
    # -- ag --
    land_size = Land_cat,
    Q401_1, # farm land
    Q401_2, # size land
    hh_garden = Q410,
    TLU_cat,
    
    growing_beans = beans_YN,
    growing_maize = maize_YN,
    growing_s_potato = sw_potato_YN,
    growing_cassava = cassava_YN,
    growing_i_potato = ir_potato_YN,
    growing_sorghum = sorghum_YN,
    
    # -- finances --
    # Q1202, Q1204, QA1206+#food_assistance + financial_assistance + ag_assistance
    food_assitance,
    financial_assitance,
    Ag_assitance,
    
    
    # -- food --
    
    
    
    # -- connectivity --
    health_less_60min  = Q_hospital,
    road_dist_cat = Q_roads_km,
    market_dist_cat = Q_market
  )

# -- ag --
own_livestock + TLU +  + hh_garden +
  
  mostly_selling + # CARI contains FCS.  
  
  hh_occup_cat + num_jobs,
sh_agricultural_production + sh_labour_ag_work + 
  sh_unskilled_labour 
!village_cat,
# -- ed --
ed_cat = ~ head_education_cat + mother_education,
ed_lit = ~ head_literate + mother_literate,
ed_all = ~ pct_lowEd, #pct_literate

)
# clean vars --------------------------------------------------------------
hh2012 = hh2012 %>% 
  # -- location --
  factorize(hh2012_raw, 'Urban', 'rural_cat') %>% 
  factorize(hh2012_raw, 'p_code', 'admin1') %>% 
  factorize(hh2012_raw, 'd_code', 'admin2') %>% 
  factorize(hh2012_raw, 's_code', 'admin3') %>% 
  factorize(hh2012_raw, 'v_code', 'admin4') %>% 
  factorize(hh2012_raw, 'fews_code', 'livelihood_zone')

# Admin3 seems confused.
codebk = data.frame(code = attr(hh2012_raw[['s_code']], "labels"),
                    names = names(attr(hh2012_raw[['s_code']], "labels")))

hh2012$admin3 = fct_infreq(factor(hh2012$s_code, levels = codebk$code,
                                  labels = codebk$names))

# Admin4 seems confused.
codebk = data.frame(code = attr(hh2012_raw[['v_code']], "labels"),
                    names = names(attr(hh2012_raw[['v_code']], "labels")))

hh2012$admin4 = fct_infreq(factor(hh2012$v_code, levels = codebk$code,
                                  labels = codebk$names))

# where the WB was working ------------------------------------------------

wb = hh2012 %>% 
  select(admin2, admin3, wb_site) %>% 
  distinct() %>% 
  mutate(admin3 = stringr::str_to_title(admin3))

# test
wb_map = full_join(RWA_admin3$centroids, wb, by = c('label' = 'admin3'))

wb_map %>% filter(is.na(wb_site))

wb_map %>% filter(is.na(lat))

# Only missing Shyrongi, and I'm too lazy to fix since it isn't a site anyway.


wb_map = full_join(RWA_admin3$df, wb, by = c('Sector' = 'admin3'))

ggplot(wb_map, aes(fill = factor(wb_site), x = long, y = lat,
                   group = group, order = order)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_manual(values = c('0' = grey15K, '1' = brewer.pal(9, 'Spectral')[1])) +
  theme_blank()
