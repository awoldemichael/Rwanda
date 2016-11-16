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

# source('~/GitHub/Rwanda/R/RWA_WFP_runAll.R')


# Remove NAs from stunting ------------------------------------------------


all_hh = ch_hh2012 %>% 
  filter(!is.na(isStunted)) %>% 
  mutate(head_age_sq = head_age^2,
         mother_age_sq = mother_age^2,
         head_edu_num = as.numeric(head_education_cat))

# standardize coefficients
all_hh = all_hh %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'village'))


# ch-hh models ------------------------------------------------------------
# altitude available
# # prenatal visits not.

ch_hh_models = formulas(~stuntingZ, # lhs
                        # -- child demographics --
                        basic = ~ 
                          # age_months +
                          splines::bs(age_months, degree = 3, knots = 24) +
                          sex + 
                          
                          # -- geography --
                          rural_cat +
                          
                          # -- wealth --
                          
                          wealth_idx +
                          
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
                          impr_water_under30 + 
                          
                          # -- health (child) --
                          diarrhea + birthwt +
                          
                          # -- connectivity --
                          health_less_60min + 
                          
                          # -- ag --
                          TLU + land_size_cat + hh_garden +
                          
                          # -- ed --
                          head_education_cat +
                        
                        
                        # -- food --
                        FCS +
                          CSI_cat + # CARI contains FCS.
                          months_food_access,
                        
                        # -- mother --
                        mom1 = ~  mother_age + mother_age_sq +
                          mother_education + 
                          # -- mother health --
                          mother_mosquito_net +
                          stunted_mother,
                        # contains ~ 700 NAs --> seriously cuts down sample size
                        
                        shk = ~ shock_drought + shock_illness,
                        
                        wealth2 = ~ food_assistance + financial_assistance + ag_assistance,
                        
                        geo = ~ livelihood_zone,
                        
                        fcs = add_predictors(basic, food1, geo),
                        protRich = add_predictors(basic, food2, geo),
                        foods = add_predictors(basic, food3, geo),
                        
                        mother = add_predictors(basic, mom1, food1, geo),               
                        momBMI = add_predictors(basic, mom1, mom2, food1, geo),                        
                        all = add_predictors(basic, mom1, mom2, food1, shk, wealth2, geo),
                        nogeo = add_predictors(basic, mom1, mom2, food1, shk, wealth2)
)

stunting_fits = all_hh %>% fit_with(lm, ch_hh_models)

# lapply(ch_fits, function(x) summary(x))


# Plot and evaluate variations
plot_coef(stunting_fits$fcs, cluster_col = all_hh$village)
plot_coef(stunting_fits$protRich, cluster_col = all_hh$village)
plot_coef(stunting_fits$foods, cluster_col = all_hh$village)
plot_coef(stunting_fits$mother, cluster_col = all_hh$village)
plot_coef(stunting_fits$momBMI, cluster_col = all_hh$village)
plot_coef(stunting_fits$all, cluster_col = all_hh$village)
plot_coef(stunting_fits$nogeo, cluster_col = all_hh$village)

compare_models(list('all' = stunting_fits$all,
                    'no-geo' = stunting_fits$nogeo,
                    'fcs' = stunting_fits$fcs,
                    # 'protRich' = stunting_fits$protRich,
                    # 'mother' = stunting_fits$mother,
                    'momBMI' = stunting_fits$momBMI
), 
filter_insignificant = T)
# intermediate analysis ---------------------------------------------------
# Initially: running age as linear, not splines, since 
library(car)
vif(stunting_fits$all)
# splines of age, when_antenatal = never aliased.

# look at relationships
plot_relationships(stunting_fits$basic, all_hh)




# Plot model comparison
compare_models(stunting_fits, cluster_col = all$village) 


# stunted models ----------------------------------------------------------

stunted_models = formulas(~isStunted, # lhs
                          # -- child demographics --
                          basic = ~ 
                            # age_months +
                            splines::bs(age_months, degree = 3, knots = 24) +
                            sex + 
                            
                            # -- geography --
                            rural_cat +
                            
                            # -- wealth --
                            
                            wealth_idx +
                            
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
                            diarrhea + birthwt +
                            
                            # -- connectivity --
                            health_less_60min + 
                            
                            # -- ag --
                            TLU + land_size_cat + hh_garden +
                            
                            # -- ed --
                            head_education_cat +
                            
                            # -- food --
                            FCS +
                            CSI_cat + # CARI contains FCS.
                            months_food_access,
                          
                          # -- mother --
                          mom2 = ~  mother_age + mother_age_sq +
                            mother_education + 
                            # -- mother health --
                            num_antenatal_visits +
                            mother_mosquito_net  +
                            stunted_mother,
                          # contains ~ 700 NAs --> seriously cuts down sample size
                          
                          shk = ~ shock_drought + shock_illness,
                          
                          wealth2 = ~ food_assistance + financial_assistance + ag_assistance,
                          
                          geo = ~ livelihood_zone,
                          
                          simple = add_predictors(basic, geo),
                          mother = add_predictors(basic, mom2, geo),               
                          all = add_predictors(basic, mom2, shk, wealth2, geo),
                          nogeo = add_predictors(basic, mom2, shk, wealth2)
)

stunted_fits = all_hh %>% fit_with(lm, stunted_models)

plot_coef(stunted_fits$simple) # Simpler model; no mother data (incr. sample size); no shocks/social capital
plot_coef(stunted_fits$mother) # + Mother data (incr. sample size); no shocks/social capital
plot_coef(stunted_fits$all) # Everything
plot_coef(stunted_fits$nogeo) # Everything

# compare models ----------------------------------------------------------
models_z = list('all' = stunting_fits$all,
              ' nogeo' = stunting_fits$nogeo,
              'simple' = stunting_fits$fcs,
              'mom' = stunting_fits$mother
)

compare_models(models_z, 
               filter_insignificant = F, sort_by_est = F)

compare_models(models_z, 
               filter_insignificant = T)

models = list('all' = stunted_fits$all,
                ' nogeo' = stunted_fits$nogeo,
                'simple' = stunted_fits$simple,
                'mom' = stunted_fits$mother
)

compare_models(models, 
               filter_insignificant = F, sort_by_est = F, negative_good = T)

compare_models(models, negative_good = T, negative_ontop = F,
               filter_insignificant = T)

# models by province ------------------------------------------------------
library(data.table)
df = all_hh %>% filter(admin1 %like% 'North')
stunting_fits_north = df %>% fit_with(lm, ch_hh_models)
plot_coef(stunting_fits_north$all, cluster_col = df$village)

df = all_hh %>% filter(admin1 %like% 'Kigali')
stunting_fits_kigali = df %>% fit_with(lm, ch_hh_models)
plot_coef(stunting_fits_kigali$all, cluster_col = df$village)

df = all_hh %>% filter(admin1 %like% 'East')
stunting_fits_east = df %>% fit_with(lm, ch_hh_models)
plot_coef(stunting_fits_east$all, cluster_col = df$village)

df = all_hh %>% filter(admin1 %like% 'South')
stunting_fits_south = df %>% fit_with(lm, ch_hh_models)
plot_coef(stunting_fits_south$all, cluster_col = df$village)

df = all_hh %>% filter(admin1 %like% 'West')
stunting_fits_west = df %>% fit_with(lm, ch_hh_models)
plot_coef(stunting_fits_west$all, cluster_col = df$village)

compare_models(list('north' = stunting_fits_north$all,
                    'east' = stunting_fits_east$all,
                    'south' = stunting_fits_south$all,
                    'west' = stunting_fits_west$all,
                    'kigali' = stunting_fits_kigali$all
), 
filter_insignificant = T)
