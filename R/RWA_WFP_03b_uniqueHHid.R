# -- household data --
hh_raw = read_sav(paste0(baseDir, 'RW_2015_CFSVA/cfsva-2015-master-DB- annex.sav'))
hh2012 = read_sav(paste0(baseDir, 'RW_2012_CFSVA/cfsvans-2012- household-v01.sav'))

# -- mother / children data (to create unique id) --
women_raw = read_sav(paste0(baseDir, 'RW_2015_CFSVA/cfsva-2015-mother-DB- annex.sav'))
children_raw = read_sav(paste0(baseDir, 'RW_2015_CFSVA/fsva-2015-child-DB- annex.sav'))


# Create unique ID --------------------------------------------------------
# SO... even though the 2012 data contains a household id, the 2015 data strips that out.
# Which is annoying not to have a unique idea.  Creating one based on geography and 
# indicators that are present in the household, children's, and women's modules.

# Find intersection b/w vars
hh_cols = colnames(hh_raw)
w_cols = colnames(women_raw)
ch_cols = colnames(children_raw)

isect1 = base::intersect(hh_cols, ch_cols)
isect2 = base::intersect(isect1, w_cols)

# Dates are a bit weird (some imported as time objects; others as strings)
# Let's see if we can create a unique set!
# Should have 7500 rows when remove duplicates

# 368
subset1 = hh_raw %>% select(S0_E_Sect, Urban) %>% distinct()

# 748 (--> almost # villages; should be 750)
subset2 = hh_raw %>% select(S0_E_Sect, Urban, weight) %>% distinct()

# 7098
subset3 = hh_raw %>% select(S0_E_Sect, Urban, weight, FCS) %>% distinct()

# 7322 
subset4 = hh_raw %>% select(S0_E_Sect, Urban, weight, FCS, CSI) %>% distinct()

# 7386
subset5 = hh_raw %>% select(S0_E_Sect, Urban, weight, FCS, CSI, FS_final) %>% distinct()

# SO... impossible to merge all of them together.  Trying using wealth index categories (which have diff names in vars):
subset6 = hh_raw %>% select(S0_E_Sect, Urban, weight, FCS, CSI, FS_final, WI_cat_lyr) %>% distinct()

# Looking at merging kids + hh:
# road/mkt distances seem to be community-level data
# 7467
subset7 = hh_raw %>% select(S0_E_Sect, Urban, weight, FCS, CSI, FS_final, S2_13) %>% distinct()

# Remove liters of water and adding old Ubudehe profile
# So: sector, urban/rural, weighting function, food consumption score, coping strategy index, food security (CARI) index), and Ubudehe profile
subset8 = hh_raw %>% select(S0_E_Sect, Urban, weight, FCS, CSI, FS_final, S12_01) %>% distinct()
# 7459!

subset9 = hh_raw %>% select(S0_E_Sect, Urban, weight, FCS, CSI, FS_final, S2_13, S12_01, S12_02) %>% distinct()
subset9 = hh_raw %>% select(S0_E_Sect, Urban, weight, FCS, CSI, FS_final, S2_13, S12_01, S12_02) %>% distinct()
# Seems to almost work?  Gives 7497!

# Testing it out:
subset8 = subset8 %>% mutate(hh = 'new data!')

x = left_join(children_raw, subset8, by = c('S0_E_Sect', 'Urban', 'weight', 'FCS', 'CSI', 'FS_final', 'S12_01'))
# That's a big fail.  2277/4058 not merge.
# CSI leads to craziness: NA in hh and 0 in children
x = left_join(ch, subset8, by = c('S0_E_Sect', 'Urban', 'weight', 'FCS', 'FS_final', 'S12_01'))
# Doesn't work b/c isn't unique in hh-level
x %>% group_by(is.na(hh)) %>% summarise(n())

x = left_join(children_raw, hh_raw, by = c('S0_E_Sect', 'Urban', 'weight', 'FCS', 'FS_final', 'S2_13', 'S12_01', 'S12_02'))


# Looking at a tiny subsample, looks like these might be unique:
# Note: though it'd make *so* much sense and make life easier, unfortunately date isn't consistent.
# Presumably women's modules for some households were completed at a later date.
# Could do a fuzzy match with flexible date (+ 1-3 days?)
# S0_E_Sect, weight (proxy for village), FCS, (Stunted_YN), FCS, FCG, CSI, FS_final


# STARTING OVER -----------------------------------------------------------

# Let's look at EVERY variable in common.

hh = removeAttributes(hh_raw) %>% 
  mutate(month = ifelse(month == 1, 4,
                        ifelse(month == 2, 5, NA)),
         CSI = ifelse(is.na(CSI), 0, CSI))

x = left_join(children_raw, hh, by = c("weight",                   
                                       "S2_13",     # liters of water used              
                                       "WI_cat",
                                       "S12_01",
                                       "S12_02",
                                       "FCS",   
                                       # "impr_water" = "improved_water", 
                                       # "water_source_treatment",
                                       # "impr_toilet" = "improved_toilet",         
                                       # "share_toilet"  = "S2_07_3",   
                                       # "improved_water", 
                                       # "improved_toilet",         
                                       # "S2_07_3",
                                       # "S2_07_2", 
                                       # "livezone_lyr" = "livezone",  
                                       # "S0_B_DATE",   
                                       # "CSI",
                                       # "Urban",                    
                                       # "S0_C_Prov",               
                                       # "S0_D_Dist",
                                       # "v_S2_01","v_S3_01",                   "v_S2_03_1",                "v_S2_03_2",
                                       # "v_S2_03_3",                "v_S2_03_4",                                 "v_S3_02",
                                       # "v_S3_02_2",                "v_S3_03",                  "v_S3_03_2",                "v_S4_01",
                                       # "v_S4_02_2",                "v_S4_02_3",                "v_S4_02_4",        "health_less_60min",
                                       # "market_less_60min",
                                       # "v_S2_02",
                                       # "health_facility_distance",
                                       # "market_distance",
                                       # "road_distance",
                                       # "FCG",                      
                                       "FS_final"))

# DDS is only in hh; village (S0_G_Vill) is only in children
x %>% group_by(is.na(DDS), is.na(S0_G_Vill)) %>% summarise(n())
nrow(x) == 4058

# Not bad. only 31 missing differences.

# -- Redundant --
# "livezone_lyr" = "livezone"
# "Urban",                    
# "S0_C_Prov",               
# "S0_D_Dist",
# "S0_E_Sect",
# "CSI" (when edited)
# "health_facility_distance",
# "market_distance",          
# "road_distance",    
# "FCG"
# "v_S2_02",
# "S2_07_2", 
# "v_S2_03_3",                "v_S2_03_4",                                 "v_S3_02",                 
# "v_S3_02_2",                "v_S3_03",                  "v_S3_03_2",                "v_S4_01",                 
# "v_S4_02_2",                "v_S4_02_3",                "v_S4_02_4",        "health_less_60min",        
# "market_less_60min",    
# "v_S2_01","v_S3_01",                   "v_S2_03_1",                "v_S2_03_2",  
# "impr_water" = "improved_water", 
# "impr_toilet" = "improved_toilet",
# "water_source_treatment",
# "share_toilet"  = "S2_07_3",  

# -- Different --
# "S0_B_DATE",   "CSI"
# -- Not in ch dataset --




