# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_A02_stuntingModels.R: script to run regressions on stunting from the CFSVA data
#
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 5 October 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License



# Outline of the models to run --------------------------------------------

# Running a bunch of iterations of different models.  

# -- Dependent variable variations --
# 1. stunting z-score vs. binary stunted classification
# 2. males vs. females
# 3. children < 2 y. vs. children 2-5
# 4. 2012 stunting vs. 2015 stunting_lz
# 5. stunting by province, comparing 2 time points

# -- Independent variable variations --
# 1. Comparisons to other published  models 
# 2. Comparisons to models run on DHS data
# 3. What I think may be interesting, based on those + lit. models
# 4. models w/ and w/o hh-level variables (since merging --> loss of children)


# -- Comparisons to other models --

# * CFSVA published models
# Variables from the CFSVA analysis taken from http://www.moh.gov.rw/fileadmin/templates/Summit3/8_Regional_VAriation.pdf
# and the CVSFA report. Assuming linear fit for all variables (?); looks like in the interim pdf they were just running kids < 2.
# Confusing b/c there's no slope variable in dataset, and they also reference the 2013 CFSVA (?)

# * Nada's models from the DHS
# * Tim's models from the DHS

# -- Protocol --
# * start w/ basic model and build up
# * cluster errors at village level to take into account any non-independent behavior due to sample design (using package `multiwayvcov`)
# * evaluate models using adjusted R^2
# * look at stability of coefficients using plot_coef
# * look at residuals by summarizing model
# * for better models, standardize errors


# import data -------------------------------------------------------------

source('~/GitHub/Rwanda/R/RWA_WFP_07_importHH2012.R')


# Remove NAs from stunting ------------------------------------------------


all_hh2012 = ch_hh2012 %>% 
  filter(!is.na(isStunted))

# standardize coefficients
all_hh2012 = all_hh2012 %>% 
  stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'v_code'))


# ch-hh models ------------------------------------------------------------
# CHANGES:
# -- REMOVED: --
# • prenatal visits (not in survey)

# -- ADDED: --
# • altitude

# -- CHANGED: --
# • TLUs manually recalculated.
# • continuous wealth index not available; substituting quantiled index or deciled pc-income or deciled pc-expenditures
# • hh_occup_cats might not be comparable.
# • actual birth weight unavailable

ch_hh_models = formulas(~stuntingZ, # lhs
                        # -- child demographics --
                        basic = ~ 
                          splines::bs(age_months, degree = 3, knots = 24) +
                          # age_months +
                          sex + 
                          interview_date.x +
                          
                          # -- geography --
                          rural_cat +
                          
                          
                          # -- hh demographics -- 
                          kids_under5 + 
                          crowding + 
                          fem_head +  
                          head_age + head_age_sq +
                          numWomen_18plus + 
                          hh_occup_cat +
                          
                          # -- WASH (broken down) --
                          impr_unshared_toilet + 
                          # wash_knowl + 
                          impr_water_30min + 
                          
                          # -- health (child) --
                          diarrhea + low_birthwt +
                          
                          # -- connectivity --
                          health_less_60min + 
                          
                          # -- ag --
                          TLU + land_size_cat + hh_garden +
                          
                          # -- ed --
                          head_education_cat +
                          
                          
                          # -- food --
                          food1 = ~ FCS +
                          CSI_cat + # CARI contains FCS.
                          months_food_access,
                        food2 = ~ CSI_cat +
                          months_food_access +
                          # ironRich_binary + proteinRich_binary + vitA_binary, # converting to binaries eliminates the isolated 
                          protein_days + ironrich_days + vitAfruitveg_days,
                        
                        # -- wealth --
                        wealth1 = ~ wealth_idx_num,
                        wealth2 = ~ splines::bs(log_pcexp, degree = 3, knots = 0.4), # determined knot by plotting the scaled values and seeing approx. break pt.
                        
                        # -- mother --
                        mom1 = ~  mother_age + mother_age_sq +
                          mother_education + 
                          # -- mother health --
                          mother_mosquito_net +
                          stunted_mother,
                        # contains ~ 700 NAs --> seriously cuts down sample size
                        
                        shk = ~ shock_drought + shock_illness +
                          food_assistance + financial_assistance + ag_assistance,
                        
                        geo = ~ livelihood_zone,
                        altitude = ~ altitude,
                        
                        simple = add_predictors(basic, geo, wealth1),
                        pc_exp = add_predictors(basic, mom1, shk, wealth2, geo),
                        # mother = add_predictors(basic, mom1, geo),               
                        all = add_predictors(basic, mom1, shk, wealth1, geo),
                        alt = add_predictors(basic, mom1, shk, wealth1, geo, altitude),
                        nogeo = add_predictors(basic, mom1, shk, wealth1, altitude)
)

stunting_fits2012 = all_hh2012 %>% fit_with(lm, ch_hh_models)

# lapply(ch_fits, function(x) summary(x))

# So many NAs!  Where do they come from?  TLUs are the major culprit.  Going to have to go back and calculate TLUs properly. :(
# hh-level vars == 274 NAs
# mother-level vars == 660 NAs
# plot_relationships(stunting_fits2012$all, all_hh2012)

# Plot and evaluate variations
plot_coef(stunting_fits2012$all, cluster_col = all_hh2012$v_code)
plot_coef(stunting_fits2012$mother, cluster_col = all_hh2012$v_code)
plot_coef(stunting_fits2012$nogeo, cluster_col = all_hh2012$v_code)
plot_coef(stunting_fits2012$simple, cluster_col = all_hh2012$v_code)
plot_coef(stunting_fits2012$pc_exp, cluster_col = all_hh2012$v_code)
plot_coef(stunting_fits2012$alt, cluster_col = all_hh2012$v_code)

compare_models(list('all' = stunting_fits2012$all,
                    'no-geo' = stunting_fits2012$nogeo,
                    'pc-exp' = stunting_fits2012$pc_exp,
                    'simple' = stunting_fits2012$simple,
                    'alt' = stunting_fits2012$alt
), 
filter_insignificant = T)


# Compare w/ 2015 ---------------------------------------------------------
source('~/GitHub/Rwanda/R/RWA_WFP_A02_stuntingModels.R')

compare_models(list('2012' = stunting_fits2012$all,
                    '2015' = stunting_fits$nogeo,
                    ' alt' = stunting_fits2012$alt
), 
filter_insignificant = T)


# Export ------------------------------------------------------------------
# -- 2015 data --
all = broom::tidy(stunting_fits$all) %>% 
  select(stuntingZ_all_2015 = estimate,
         stuntingZ_all_2015p = p.value, term)

nogeo = broom::tidy(stunting_fits$nogeo) %>% 
  select(stuntingZ_nogeo_2015 = estimate,
         stuntingZ_nogeo_2015p = p.value, term)

# No data on mother added in.
fcs = broom::tidy(stunting_fits$fcs) %>% 
  select(stuntingZ_noMomData_2015 = estimate,
         stuntingZ_noMomData_2015p = p.value, term) 

protRich = broom::tidy(stunting_fits$protRich) %>% 
  select(stuntingZ_FCSgrps_2015 = estimate,
         stuntingZ_FCSgrps_2015p = p.value, term)

# -- 2012 data --
all2012 = broom::tidy(stunting_fits2012$all) %>% 
  select(stuntingZ_all_2012 = estimate,
         stuntingZ_all_2012p = p.value, term)

nogeo2012 = broom::tidy(stunting_fits2012$nogeo) %>% 
  select(stuntingZ_nogeo_2012 = estimate,
         stuntingZ_nogeo_2012p = p.value, term)

# No data on mother added in.
fcs2012 = broom::tidy(stunting_fits2012$basic) %>% 
  select(stuntingZ_noMomData_2012 = estimate,
         stuntingZ_noMomData_2012p = p.value, term) 

alt2012 = broom::tidy(stunting_fits2012$alt) %>% 
  select(stuntingZ_plusAltitude_2012 = estimate,
         stuntingZ_plusAltitude_2012p = p.value, term) 
# no protein data

# -- joins galore --

all_regr = full_join(all, all2012)

all_regr = full_join(all_regr, nogeo)
all_regr = full_join(all_regr, nogeo2012)

all_regr = full_join(all_regr, fcs)
all_regr = full_join(all_regr, fcs2012)
all_regr = full_join(all_regr, protRich)

all_regr = full_join(all_regr, alt2012)

write.csv(all_regr, '~/GitHub/Rwanda/exported_data/RWA_cfsva_regressions.csv')

