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
# * look at stability of coefficients using coefplot
# * look at residuals by summarizing model
# * for better models, standardize errors


# import data -------------------------------------------------------------

source('~/GitHub/Rwanda/R/RWA_WFP_runAll.R')


# Remove NAs from stunting ------------------------------------------------

males = ch %>% filter(!is.na(isStunted), 
                      sex == 'Male')

females = ch %>% filter(!is.na(isStunted), 
                        sex == 'Female')

all = ch %>% filter(!is.na(isStunted))


males_hh = ch_hh %>% filter(!is.na(isStunted), 
                            sex == 'Male')

females_hh = ch_hh %>% filter(!is.na(isStunted), 
                              sex == 'Female')

all_hh = ch_hh %>% filter(!is.na(isStunted))
# standardize coefficients
all_hh = all_hh %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'village'))


# Check CFSVA lit models --------------------------------------------------

# stunting score
stunting_fit_cfsva = lm(formula = stuntingZ ~ sex + age_months + low_birthwt +
                          beans_W24h + milk_W24h + 
                          stunted_mother + mother_age + mother_education +
                          wealth_idx_cat + new_ubudehe + 
                          rural_cat + livelihood_zone +
                          CARI_cat + diarrhea +
                          road_dist_cat + school_dist_cat + market_dist_cat + health_dist_cat,
                        data = all)
summary(stunting_fit_cfsva)
coefplot(stunting_fit_cfsva, cluster_col = all$village)
# plot_relationships(stunting_fit_cfsva)

# stunting score
stunted_fit_cfsva = lm(formula = isStunted ~ sex + age_months + low_birthwt +
                         beans_W24h + milk_W24h + superCereal_W24h + nuts_W24h + eggs_W24h + starch_W24h + protein_W24h + 
                         greenVeg_W24h + otherVeg_W24h + orangeFruits_W24h + otherFruit_W24h +
                         stunted_mother + mother_age + mother_education +
                         wealth_idx_cat + new_ubudehe + 
                         rural_cat + livelihood_zone +
                         CARI_cat + diarrhea +
                         road_dist_cat + school_dist_cat+ market_dist_cat + health_dist_cat,
                       data = all)

summary(stunted_fit_cfsva)
coefplot(stunted_fit_cfsva, cluster_col = all$village, negative_good = TRUE)

# plot_relationships(stunted_fit_cfsva)

# Overall: fairly poor relationship: r^2= 0.128; 936 obs. removed

# Tim comparison ----------------------------------------------------------
tim_models = formulas(~stuntingZ, # lhs
                      basic = 
                        # maternal characteristics
                        ~ fem_head +  
                        # !not available: mother's birthweight, mother's age at first pregnancy
                        
                        # hh characterstics
                        wealth_idx + impr_toilet + impr_water + 
                        
                        # hh ag
                        TLU +  
                        
                        # demographics
                        rooms_PC + head_age + hh_size + kids_under5 + numWomen_18plus +
                        # !not available: numWomen15-25, numWomen26-65. Options: 15-17, 15-49, 18-59, 60+, 18+
                        
                        # child characteristics
                        sex + age_months + age_months^2  +  
                        # !not available: birthOrder, wantedChild
                        
                        # child health
                        # note: most kids received vitamin A drops.
                        dewormed + vitaminA + diarrhea +
                        
                        # geography
                        rural_cat + livelihood_zone +
                        # !unavailable: altitude
                        
                        # interview date (month)
                        month.y,
                      
                      # Lots more NAs, since unclear how mothers match with kids in all cases.
                      mother = ~mother_BMI + mother_education + mother_mosquito_net,
                      birthweight = ~birthwt,
                        
                      # throwing in other variables similar to what Tim was trying to run.
                      extra = ~ mother_age + land_size, 
                      
                      food = ~ FCS + months_food_access,
                      connectivity = ~ road_dist_cat + school_dist_cat + market_dist_cat + health_dist_cat,
                      health = ~ num_antenatal_visits + when_antenatal,
                      
                      tim = add_predictors(basic, mother, birthweight),
                      combined_tim = add_predictors(basic, extra, mother, birthweight),
                      combined = add_predictors(basic, extra, food, connectivity, health),
                      combined_plus = add_predictors(basic, extra, mother, birthweight, food, connectivity, health))

stunting_tim = all_hh %>% fit_with(lm, tim_models)

coefplot(stunting_tim$basic, cluster_col = all_hh$village)
# plot_relationships(stunting_tim$tim)

coefplot(stunting_tim$tim, cluster_col = all_hh$village)
coefplot(stunting_tim$combined_tim, cluster_col = all_hh$village)
coefplot(stunting_tim$combined, cluster_col = all_hh$village)
coefplot(stunting_tim$combined_plus, cluster_col = all_hh$village)

# Nada comparison ---------------------------------------------------------

# Modeling age as a third order spline with knot at 24 months (2 years) 
nada_f_model = lm(stuntingZ ~ splines::bs(age_months, degree = 3, knots = 24) + #use for male/female only
                    # birth_order+birth_interval_preceding*firstborn+ # demographics; not in CFSVA
                    stunted_mother + mother_education + # mother
                    ever_breastfed + # nutrition
                    infrastruct_idx + wash_idx + # infrastructure, WASH indices
                    # comm_ind + # communication index; asked in CFSVA but not given to me :(
                    cookingfuel_cat + # improved cooking fuel; 
                    TLU + land_size + # livestock + land
                    # bike + bankAccount + # Not in CFSVA
                    diarrhea +
                    month.y +
                    rural_cat +
                    livelihood_zone, #other
                  data = ch_hh %>% filter(sex == 'Female')) 

nada_m_model = lm(stuntingZ ~ splines::bs(age_months, degree = 3, knots = 24) + #use for male/female only
                    # birth_order+birth_interval_preceding*firstborn+ # demographics; not in CFSVA
                    stunted_mother + mother_education + # mother
                    ever_breastfed + # nutrition
                    infrastruct_idx + wash_idx + # infrastructure, WASH indices
                    # comm_ind + # communication index; asked in CFSVA but not given to me :(
                    cookingfuel_cat + # improved cooking fuel; 
                    TLU + land_size + # livestock + land
                    # bike + bankAccount + # Not in CFSVA
                    diarrhea +
                    month.y +
                    rural_cat +
                    livelihood_zone, #other
                  data = ch_hh %>% filter(sex == 'Male')) 

summary(nada_m_model)
summary(nada_f_model)

nada_mf <- glm(isStunted ~
                 #Modeling age as a third order spline with knot at 24 months (2 years) 
                 #bs(age_calc_months,degree=3,knots=24)+ #use for male/female only
                 splines::bs(age_months, degree = 3, knots = 24) * sex + #use for entire data set    
                 # birth_order+birth_interval_preceding*firstborn+ #demographics
                 stunted_mother + mother_education + # mother
                 ever_breastfed + # nutrition
                 infrastruct_idx + wash_idx +
                 # +comm_ind+ #infrastructure, WASH, communication indeces
                 cookingfuel_cat + #improved cooking fuel
                 TLU + own_land + #livestock+land
                 # bike+bankAccount+ # not in CFSVA
                 diarrhea +
                 month.y +
                 rural_cat +
                 livelihood_zone, 
               data = ch_hh, family = binomial(link = 'logit')) 

summary(nada_mf)

coefplot(nada_m_model)
coefplot(nada_f_model)



# stunting models ------------------------------------------------------------------
# define models ------------------------------------------------------------------
# Variables thrown out, + rationale:
# stock_durationA: ~1000 NAs
# ill_fortnight | diarrhea + cough + fever; choosing to run separately

# -- Check where have huge #s of NAs --
View(ch_hh %>% 
  select(sex, age_months, interview_date, month.x, 
         hh_size, crowding, kids_under5, pct_under7, numWomen_15_49, numWomen_18plus, fem_head, prim_caregiver, head_age,
         mother_literate, mother_education, head_literate, head_education_cat, pct_lowEd, pct_highEd, pct_illiterate,
         health_dist_cat, health_less_60min, market_less_60min, market_dist_cat, road_dist_cat, school_dist_cat,
         shock_drought, shock_illness,
         wealth_idx, monthly_pc_expend, new_ubudehe, old_ubudehe, got_loan, asked_loan, infrastruct_idx, impr_roof, impr_floor, impr_wall, own_house_cat,
         cookingfuel_cat, food_assistance, financial_assistance, ag_assistance,
         diarrhea, fever, cough, dewormed, vitaminA, birthwt, birthweight_cat, ever_breastfed, fed_nonbreastmilk, still_breastfed, breastfed_afterbirth,
         antenatal_care, num_antenatal_visits, when_antenatal, stunted_mother,  mother_age, mother_BMI, mother_mosquito_net, mother_ill_2weeks, Fe_supplements,
         impr_toilet, share_toilet, impr_unshared_toilet, impr_water, impr_water_30min, time_water_source, time_drinkingH2O_cat, H2Otreatment_cat, drinkingH2O_cat, wash_knowl, wash_beforecook, wash_kidtoilet, wash_beforeeat, wash_aftertoilet, wash_ifdirty,
         TLU, own_livestock, own_cattle, manage_livestock, own_land, land_size, hh_garden,
         FCS, DDS, dietDiv_W24h, dietDiv_W24h_cat, HDDS_24h, contains('day'), pref_staple, child_meal_freq, CARI_cat, food_access_prob, food_access_year_cat,
         months_food_access, CSI_cat, CSI.x, protein_days, ironrich_days, vitAfruitveg_days, sh_food_grown, sh_food_purchased, sh_food_expend, 
         contains('growing'), hh_occup_cat, sh_agricultural_production, sh_labour_ag_work, sh_unskilled_labour, num_jobs, mostly_selling, mostly_consuming,
         village_VUP, village_noSchemes, village_IDPmodel, village_landConsolid, village_structUmudugudu, 
         admin2, admin1, village_cat, rural_cat, livelihood_zone
  ) %>% 
  ungroup() %>% 
  summarise_each(funs(sum(is.na(.)))) %>% 
  gather(var, num_NA) %>% 
  filter(num_NA > 10) %>% 
  arrange(desc(num_NA)))

  

stunting_models = formulas(~stuntingZ, # lhs
                           basic = 
                             # -- child demographics --
                             ~splines::bs(age_months, degree = 3, knots = 24) +
                             # -- geography --
                             village_cat + livelihood_zone +
                             # -- wealth --
                             splines::bs(monthly_pc_expend, degree = 2),
                           
                           # -- WASH (broken down) --
                           wash = ~ impr_toilet + share_toilet + impr_water + 
                             H2Otreatment_cat + time_drinkingH2O_cat + wash_knowl,
                           
                           # -- health (child) --
                           health = ~ diarrhea + fever +
                             cough + dewormed,
                           
                           # -- mother --
                           mom = ~ stunted_mother + num_antenatal_visits + when_antenatal +
                             mother_mosquito_net + mother_ill_2weeks + Fe_supplements,
                           
                           # -- ag --
                           ag = ~ TLU + land_size + hh_garden,
                           
                           # -- food --
                           food = ~ FCS + months_food_access,
                           
                           # -- connectivity --
                           # -- hh demographics -- 
                           hh_demo = ~ hh_size + crowding + pct_under7,
                           
                           # -- ed --
                           ed = ~ mother_education + head_education_cat + pct_lowEd,
                           
                           broken_wealth = add_predictors(basic, wash, health, mom, 
                                                          ag, food, hh_demo, ed)
                           
)


splines::bs(age_months, degree = 3, knots = 24) * sex +
# VIF: look at values > 2; remove VIF > 5-10

# run models --------------------------------------------------------------

stunting_fits_m = males_hh %>% fit_with(lm, stunting_models)

stunting_fits_f = females_hh %>% fit_with(lm, stunting_models)

coefplot(stunting_fits_f$broken_wealth, cluster_col = females_hh$village)
coefplot(stunting_fits_m$broken_wealth, cluster_col = males_hh$village)

lapply(stunting_fits_f, function(x) summary(x))


# model evaluation -------------------------------------------------------
# http://www.statmethods.net/stats/rdiagnostics.html
library(car)
vif(model)
sqrt(vif(fit)) > 2 


# models by province ------------------------------------------------------
library(data.table)
df = all_hh %>% filter(admin1 %like% 'North')
stunting_fits_north = df %>% fit_with(lm, stunting_models)
coefplot(stunting_fits_north$broken_wealth, cluster_col = df$village)

df = all_hh %>% filter(admin1 %like% 'Kigali')
stunting_fits_kigali = df %>% fit_with(lm, stunting_models)
coefplot(stunting_fits_kigali$broken_wealth, cluster_col = df$village)

df = all_hh %>% filter(admin1 %like% 'East')
stunting_fits_east = df %>% fit_with(lm, stunting_models)
coefplot(stunting_fits_east$broken_wealth, cluster_col = df$village)

df = all_hh %>% filter(admin1 %like% 'South')
stunting_fits_south = df %>% fit_with(lm, stunting_models)
coefplot(stunting_fits_south$broken_wealth, cluster_col = df$village)

df = all_hh %>% filter(admin1 %like% 'West')
stunting_fits_west = df %>% fit_with(lm, stunting_models)
coefplot(stunting_fits_west$broken_wealth, cluster_col = df$village)
