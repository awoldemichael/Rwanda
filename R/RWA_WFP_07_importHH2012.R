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
    impr_toilet = improved_toilet,
    share_toilet = Q206, # shared toilet
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



# merge everything together -----------------------------------------------
# checking everything is unqiue
length(unique(hh2012$hh_id)) == nrow(hh2012)
length(unique(females2012$hh_id)) == nrow(females2012) # duplicative, but maybe okay.

# canary column for children: sex of child sum(is.na(ch2012$Q202_09))
# canary column for women: sum(is.na(females2012$moth_ID))
# canary column for men: sum(is.na(hh2012$Q_altitude))

# test merge:
k = left_join(ch2012, females2012, 
              by = c("hh_id", "weight", "mother_id", "p_code", 
                     "d_code", "s_code", "v_code", "t_code", "final_urban", "fews_code"))

if(nrow(k) != nrow(ch2012)) {
  print('ch2012 / females2012 merge fail. Redundant/nondescript ids')
} else if (sum(is.na(k$moth_ID)) > 0){
  print('ch2012 / females2012 merge fail: mother NAs')
} else if (sum(is.na(k$Q202_09)) > 0){
  print('ch2012 / females2012 merge fail: mother NAs')
}

# Lots of mothers that don't merge.  Porque?  Motherless children, or something else?
# 875 problems initially.
# So there are 282 children, all in Kigali, with hh_id = 0, and no 
# 349 children have missing info from their mom (code 88), or mother is dead (code 99) :(. Assuming code 77 is also a no-go.
# strata_id is missing in hh and children's, but set to 0 in females.
# rest seem to be code mismatches.  Not sure I can fix.


k = left_join(ch2012, hh2012, 
              by = c("hh_id", "weight", "strata_ID", "p_code", "d_code", "s_code", 
                     "v_code", "final_urban", "hh_size", 
                     # "fews_code",
                     "livelihood", "Q102", "Q105", "Q215_1", "impr_toilet", "FCS"))
# 308 total.
# 19 fews code mis-match.
# 282 children with hhid==0 (wtf?)
# 7 children w/ hh_id 1 but different sampling weights and interview dates, neither of which matches the one in hh.
# Not sure there's anything to be done...

# nrow(k) == nrow(ch2012)
# sum(is.na(k$Q202_09)) # kids remain intact.
# sum(is.na(k$Q_altitude))
# glimpse(k %>% filter(hh_id !=0, is.na(Q_altitude)))
# # 
# t(hh2012 %>% filter(hh_id == 7899) %>% select(one_of(c("hh_id", "weight", "strata_ID", "p_code", "d_code", "s_code", 
#   "v_code", "final_urban", "fews_code", "hh_size", 
#   "livelihood", "Q102", "Q105", "Q215_1", "impr_toilet", "FCS"))))

# merge for real. ---------------------------------------------------------
ch_hh2012 = left_join(ch2012, hh2012, 
          by = c("hh_id", "weight", "strata_ID", "p_code", "d_code", "s_code", 
                 "v_code", "final_urban", "hh_size", 
                 "livelihood", "Q102", "Q105", "Q215_1", "impr_toilet", "FCS"))


ch_hh2012 = left_join(ch_hh2012, females2012, by = c("hh_id", "weight", "mother_id", "p_code", 
                     "d_code", "s_code", "v_code", "t_code", "final_urban"))

# clean vars --------------------------------------------------------------

hh2012 = hh2012 %>% 
  mutate(
    # -- fix weirdness / create new var --
    impr_unshared_toilet = case_when(hh2012$impr_toilet == 0 ~ 0,
                                     (hh2012$impr_toilet == 1 & hh2012$share_toilet == 0) ~ 1, # improved + unshared
                                     (hh2012$impr_toilet == 1 & hh2012$share_toilet == 1) ~ 0, # improved + shared
                                     TRUE ~ NA_real_)
  )

hh2012 = hh2012 %>% 
  # -- location --
  factorize(hh2012_raw, 'Urban', 'rural_cat') %>% 
  factorize(hh2012_raw, 'p_code', 'admin1') %>% 
  factorize(hh2012_raw, 'd_code', 'admin2') %>% 
  factorize(hh2012_raw, 's_code', 'admin3') %>% 
  factorize(hh2012_raw, 'v_code', 'admin4') %>% 
  factorize(hh2012_raw, 'fews_code', 'livelihood_zone')

# Fix Kigali City (sigh)
hh2012 =  hh2012  %>% 
  mutate(livelihood_zone = ifelse(livelihood_zone %like% 'Kigali', 'Kigali city',
                                  as.character(livelihood_zone)))

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
# Note: village names do not merge w/ Cell names.

wb_map = full_join(RWA_admin3$df, wb, by = c('Sector' = 'admin3'))

ggplot(wb_map, aes(fill = factor(wb_site), x = long, y = lat,
                   group = group, order = order)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_manual(values = c('0' = grey15K, '1' = brewer.pal(9, 'Spectral')[1])) +
  theme_blank()
