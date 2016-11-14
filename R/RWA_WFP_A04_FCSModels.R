# Rwanda stunting analysis -----------------------------------------
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

# source('~/GitHub/Rwanda/R/RWA_WFP_runAll.R')


# Remove NAs from FCS ------------------------------------------------

fcs = hh %>% filter(!is.na(FCS))

# standardize variables for regression. binaries, categoricals ignored; continuous scaled.
fcs = fcs %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'FCS'))

# -- For children's data --
fcs_ch = ch_hh %>% filter(!is.na(FCS))

# standardize variables for regression. binaries, categoricals ignored; continuous scaled.
fcs_ch = fcs_ch %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'FCS'))

# model evaluation -------------------------------------------------------
# http://www.statmethods.net/stats/rdiagnostics.html

# VIF: look at values > 2; remove VIF > 5-10 (too much co-linearity)
library(car)
vif(ch_fits$final)
sqrt(vif(stunting_fits$total)) > 2 


# FCS models ------------------------------------------------------------

fcs_models = formulas(~FCS, # lhs
                      # -- child demographics --
                      basic = ~ month +
                        
                        # -- geography --
                        livelihood_zone + 
                        village_cat +
                        
                        # -- wealth --
                        # splines::bs(monthly_pc_expend, degree = 2),
                        wealth_idx_cat +
                        
                        # -- hh demographics -- 
                        hh_size + crowding + fem_head +  head_age +  
                        
                        # -- food --
                        CSI_cat + # CARI contains FCS.
                        # months_food_access,
                        
                        # -- connectivity --
                        health_less_60min + road_dist_cat + market_dist_cat,
                      
                      
                      # -- ag --
                      ag = ~ own_livestock + TLU + land_size + hh_garden,
                      
                      # -- ed --
                      ed = ~ head_education_cat + pct_illiterate + pct_lowEd,
                      # mother_literate + head_literate +pct_lowEd + pct_highEd 
                      
                      shk = ~ shock_drought + shock_illness,
                      
                      wealth2 = ~ got_loan + asked_loan + food_assistance + 
                        financial_assistance + ag_assistance,
                      # monthly_pc_expend + new_ubudehe + old_ubudehe + cookingfuel_cat+ infrastruct_idx + impr_roof + impr_floor + impr_wall + own_house_cat +
                      
                      food2 = ~ 
                        months_food_access + sh_food_grown,
                      # + sh_food_purchased + sh_food_expend,
                      # food_access_prob + food_access_year_cat + 
                      
                      livelihood = ~ growing_beans + growing_maize + growing_s_potato +
                        growing_cassava + growing_i_potato + growing_sorghum +
                        hh_occup_cat + sh_agricultural_production + sh_labour_ag_work + 
                        sh_unskilled_labour + num_jobs + mostly_selling,
                      # + mostly_consuming,
                      
                      village = ~village_VUP + village_IDPmodel + village_landConsolid + village_structUmudugudu,
                      
                      
                      simple = add_predictors(basic),
                      educ = add_predictors(basic, ed),
                      all = add_predictors(basic,  ag, ed, shk, wealth2, food2, livelihood)
)

fcs_fits = fcs %>% fit_with(lm, fcs_models)

# lapply(ch_fits, function(x) summary(x))

# Plot model comparison
compare_models(fcs_fits) 

# Plot and evaluate variations
coefplot(fcs_fits$all, cluster_col = NA)
