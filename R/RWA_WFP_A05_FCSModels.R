#
# RW_WFP_A04_FCSModels.R: script to run regressions on FCS from the CFSVA data
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



# Outline of the models to run --------------------------------------------


# -- Protocol --
# * start w/ basic model and build up
# * cluster errors at village level to take into account any non-independent behavior due to sample design (using package `multiwayvcov`)
# * evaluate models using adjusted R^2
# * look at stability of coefficients using coefplot
# * look at residuals by summarizing model
# * for better models, standardize errors


# import data -------------------------------------------------------------

source('~/GitHub/Rwanda/R/RWA_WFP_runAll.R')


# Remove NAs from FCS ------------------------------------------------
# NOTE: CSI_category lumps together those w/ a coping score of 0 with those w/o food security situations.
# CSI_cat and food_access_problem (last week) should be highly correlated. 
# Removing CSI from fit reduces the fit and makes fem_head pop up.  everything else is pretty consistent.

fcs = hh %>% 
  filter(!is.na(FCS)) %>% 
  mutate(head_age_sq = head_age^2,
         head_edu_num = as.numeric(head_education_cat))

# standardize variables for regression. binaries, categoricals ignored; continuous scaled.
fcs = fcs %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'FCS'))

# -- For children's data --
fcs_ch = ch_hh %>% 
  filter(!is.na(FCS)) %>% 
  mutate(WI_cat = WI_cat_lyr_lyr,
         month = month.x,
         head_age_sq = head_age^2,
         head_edu_num = as.numeric(head_education_cat),
         mother_edu_num = as.numeric(mother_education)
  )

# standardize variables for regression. binaries, categoricals ignored; continuous scaled.
fcs_ch = fcs_ch %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'FCS', 'village'))



# FCS models ------------------------------------------------------------
# Running head education as a number doesn't seem to add much value; fits not better.
# Age^2 added to take into account behavior of young + old is usually not good.
# Robust to pct_dep or dep_ratio

fcs_models = formulas(~FCS, # lhs
                      # -- child demographics --
                      basic = ~ month +
                        
                        # -- geography --
                        livelihood_zone + 
                        village_cat +
                        
                        # -- wealth --
                        # splines::bs(monthly_pc_expend, degree = 2),
                        WI_cat +
                        
                        # -- hh demographics -- 
                        hh_size + crowding + fem_head +  head_age + head_age_sq + 
                        
                        # -- food --
                        CSI_cat + # CARI contains FCS.
                        # months_food_access,
                        
                        # -- connectivity --
                        health_less_60min + road_dist_cat + market_dist_cat,
                      
                      
                      # -- ag --
                      ag = ~ TLU + land_size + hh_garden,
                      
                      # -- ed --
                      ed = ~ pct_illiterate + pct_lowEd,
                      # mother_literate + head_literate +pct_lowEd + pct_highEd head_education_cat + 
                      
                      shk = ~ shock_drought + shock_illness,
                      
                      wealth2 = ~ asked_loan + food_assistance + 
                        financial_assistance + ag_assistance,
                      # monthly_pc_expend + new_ubudehe + got_loan + old_ubudehe + cookingfuel_cat+ infrastruct_idx + impr_roof + impr_floor + impr_wall + own_house_cat +
                      
                      food2 = ~ 
                        months_food_access + mostly_selling,
                      # + sh_food_grown,
                      # + sh_food_purchased + sh_food_expend,
                      # food_access_prob + food_access_year_cat + 
                      
                      livelihood_occup = ~ growing_beans + growing_maize + growing_s_potato +
                        growing_cassava + growing_i_potato + growing_sorghum +
                        hh_occup_cat + num_jobs,
                      
                      livelihood_sh = ~ growing_beans + growing_maize + growing_s_potato +
                        growing_cassava + growing_i_potato + growing_sorghum +
                        sh_agricultural_production + sh_labour_ag_work + 
                        sh_unskilled_labour + num_jobs,
                      # + mostly_consuming,
                      
                      village = ~village_VUP + village_IDPmodel + village_landConsolid + village_structUmudugudu,
                      
                      # -- REMOVING EVERYTHING THAT SEEMS IRRELEVANT --
                      minimal = 
                        ~ month +
                        
                        # -- geography --
                        livelihood_zone + 
                        village_cat +
                        
                        # -- wealth --
                        WI_cat +
                        
                        # -- hh demographics -- 
                        crowding + dep_ratio + fem_head +  head_age + head_age_sq +
                        
                        # -- food --
                        months_food_access + sh_food_grown + # CARI contains FCS.  
                        #mostly_selling has large # NAs, --> poorer fit.
                        
                        # -- connectivity --
                        health_less_60min + road_dist_cat + market_dist_cat +
                        
                        # -- ag --
                        TLU + land_size + hh_garden +
                        
                        
                        # -- finances --
                        food_assistance + financial_assistance + ag_assistance,
                      
                      # -- ed --
                      ed_cat  = ~ head_education_cat, 
                      ed_lit = ~ head_literate,
                      ed_all = ~ pct_lowEd, #pct_literate
                      
                      # -- coping strategies to food shortages --
                      csi = ~CSI_cat,
                      
                      # -- combined models --
                      min_edu = add_predictors(minimal, ed_cat, livelihood_occup, csi),
                      min_lit = add_predictors(minimal, ed_lit, livelihood_occup, csi),
                      
                      all = add_predictors(basic,  ag, ed, shk, wealth2, food2, livelihood_occup, csi),
                      occup = add_predictors(minimal, ed_all, livelihood_occup, csi),
                      noCSI = add_predictors(minimal, ed_all, livelihood_occup),
                      sh = add_predictors(minimal, ed_all, livelihood_sh, csi)
)

fcs_fits = fcs %>% fit_with(lm, fcs_models)

# lapply(ch_fits, function(x) summary(x))

# Plot model comparison
compare_models(list('occup' = fcs_fits$occup, 'sh' = fcs_fits$sh, 
                    # 'lowEd' = fcs_fits$min_all,
                    # 'literate' = fcs_fits$min_lit,
                    'education' = fcs_fits$min_edu)) 

# Plot and evaluate variations
coefplot(fcs_fits$occup, cluster_col = NA)
coefplot(fcs_fits$noCSI, cluster_col = NA)
coefplot(fcs_fits$sh, cluster_col = NA)
coefplot(fcs_fits$min_lit, cluster_col = NA)
coefplot(fcs_fits$min_edu, cluster_col = NA)
coefplot(fcs_fits$all, cluster_col = NA)


# testing rebasing LZ -----------------------------------------------------
fcs$lz_centralPlateau = fct_relevel(fcs$livelihood_zone, "Central Plateau Cassava and Coffee Zone")

fcs_rebase = lm(FCS ~   month +
                  # -- geography --
                  lz_centralPlateau + 
                  village_cat +
                  
                  # -- wealth --
                  WI_cat +
                  
                  # -- hh demographics -- 
                  crowding + dep_ratio + fem_head +  head_age + head_age_sq +
                  
                  # -- food --
                  months_food_access + sh_food_grown + # CARI contains FCS.  
                  #mostly_selling has large # NAs, --> poorer fit.
                  
                  # -- connectivity --
                  health_less_60min + road_dist_cat + market_dist_cat +
                  
                  # -- ag --
                  TLU + land_size + hh_garden +
                  
                  
                  # -- finances --
                  food_assistance + financial_assistance + ag_assistance +
                  
                  # -- ed --
                  pct_lowEd +
                  
                  # -- coping strategies to food shortages --
                  CSI_cat +
                  growing_beans + growing_maize + growing_s_potato +
                  growing_cassava + growing_i_potato + growing_sorghum +
                  hh_occup_cat + num_jobs, 
                data = fcs)

coefplot(fcs_rebase)

# model evaluation -------------------------------------------------------
# http://www.statmethods.net/stats/rdiagnostics.html

# VIF: look at values > 2; remove VIF > 5-10 (too much co-linearity)
library(car)
vif(ch_fits$final)
sqrt(vif(stunting_fits$total)) > 2 

# children (to look at mother’s education) --------------------------------

fcs_models = formulas(~FCS, # lhs
                      # -- REMOVING EVERYTHING THAT SEEMS IRRELEVANT --
                      minimal = 
                        ~ month +
                        
                        # -- geography --
                        livelihood_zone + 
                        village_cat +
                        
                        # -- wealth --
                        WI_cat +
                        
                        # -- hh demographics -- 
                        crowding + dep_ratio + fem_head +  head_age + head_age_sq +
                        
                        # -- food --
                        months_food_access + CSI_cat + sh_food_grown + # CARI contains FCS.  
                        
                        # -- connectivity --
                        health_less_60min + road_dist_cat + market_dist_cat +
                        
                        # -- ag --
                        TLU + land_size + hh_garden +
                        
                        # -- finances --
                        food_assistance + financial_assistance + ag_assistance,
                      
                      # -- livelihoods --
                      livelihood_occup = 
                        ~ growing_beans + growing_maize + growing_s_potato +
                        growing_cassava + growing_i_potato + growing_sorghum +
                        hh_occup_cat + num_jobs,
                      
                      livelihood_sh = ~ growing_beans + growing_maize + growing_s_potato +
                        growing_cassava + growing_i_potato + growing_sorghum +
                        sh_agricultural_production + sh_labour_ag_work + 
                        sh_unskilled_labour + num_jobs,
                      
                      # -- ed --
                      ed_cat = ~ head_education_cat + mother_education,
                      ed_lit = ~ head_literate + mother_literate,
                      ed_all = ~ pct_lowEd, #pct_literate
                      
                      # -- combined models --
                      min_edu = add_predictors(minimal, ed_cat, livelihood_occup),
                      min_lit = add_predictors(minimal, ed_lit, livelihood_occup),
                      
                      occup = add_predictors(minimal, ed_all, livelihood_occup),
                      sh = add_predictors(minimal, ed_all, livelihood_sh)
)

fcs_ch_fits = fcs_ch %>% fit_with(lm, fcs_models)

# lapply(ch_fits, function(x) summary(x))

# Plot model comparison
compare_models(list('occup' = fcs_ch_fits$occup, 
                    'sh' = fcs_ch_fits$sh,
                    # 'lowEd' = fcs_ch_fits$min_all,
                    'literate' = fcs_ch_fits$min_lit,
                    'education' = fcs_ch_fits$min_edu),
               cluster_col = fcs_ch$village) 

# Plot and evaluate variations
coefplot(fcs_ch_fits$occup, cluster_col = fcs_ch$village)
coefplot(fcs_ch_fits$sh, cluster_col = fcs_ch$village)
coefplot(fcs_ch_fits$min_edu, cluster_col = fcs_ch$village)

vif(fcs_ch_fits$minimal)


# compare kids/all --------------------------------------------------------

compare_models(list('(all hh)  share_work ' = fcs_fits$sh,
                    '(all hh) occup/lowEd' = fcs_fits$occup,
                    '(all hh) no CSI' = fcs_fits$noCSI,
                    '(kids) occup/lowEd ' = fcs_ch_fits$occup,
                    '(kids) occup/ed' = fcs_ch_fits$min_edu,
                    '(kids) occup/lit' = fcs_ch_fits$min_lit),
               filter_insignificant = FALSE,
               sort_by_est  = FALSE)  +
  theme_ygrid() + theme(axis.text.x = element_text(size= 11),
                        axis.text.y = element_text(size= 11))


compare_models(list('(all hh)  share_work ' = fcs_fits$sh,
                    '(all hh) occup/lowEd' = fcs_fits$occup,
                    '(all hh) no CSI' = fcs_fits$noCSI,
                    '(kids) occup/lowEd ' = fcs_ch_fits$occup,
                    '(kids) occup/ed' = fcs_ch_fits$min_edu,
                    '(kids) occup/lit' = fcs_ch_fits$min_lit),
               filter_insignificant = TRUE,
               sort_by_est  = TRUE)  +
  theme_ygrid() + theme(axis.text.x = element_text(size= 11),
                        axis.text.y = element_text(size= 11))


# Rebase is exactly the same, aside from the LZ differences.
compare_models(list(
                    '(all hh) occup/lowEd' = fcs_fits$occup,
                    'rebase' = fcs_rebase),
               filter_insignificant = TRUE,
               sort_by_est = F)  +
  theme_ygrid() + theme(axis.text.x = element_text(size= 11),
                        axis.text.y = element_text(size= 11))

