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

source('~/GitHub/Rwanda/R/RWA_WFP_run2015.R')


# Remove NAs from stunting ------------------------------------------------

males = ch %>% filter(!is.na(isStunted), 
                      sex == 'Male')

females = ch %>% filter(!is.na(isStunted), 
                        sex == 'Female')

all = ch %>% filter(!is.na(isStunted))
all = all %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'village'))

males_hh = ch_hh %>% filter(!is.na(isStunted), 
                            sex == 'Male') %>% 
  mutate(head_age_sq = head_age^2,
         mother_age_sq = mother_age^2,
         head_edu_num = as.numeric(head_education_cat))

males_hh = males_hh %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'village', 'wealth_idx_cat', 'month_pcexpend_decile'))



females_hh = ch_hh %>% filter(!is.na(isStunted), 
                              sex == 'Female') %>% 
  mutate(head_age_sq = head_age^2,
         mother_age_sq = mother_age^2,
         head_edu_num = as.numeric(head_education_cat)) 

females_hh = females_hh %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'village', 'wealth_idx_cat', 'month_pcexpend_decile'))


all_hh = ch_hh %>% 
  filter(!is.na(isStunted)) %>% 
  mutate(head_age_sq = head_age^2,
         mother_age_sq = mother_age^2,
         head_edu_num = as.numeric(head_education_cat)) 

all_hh$month_pcexpend_decile = ntile(all_hh$monthly_pc_expend, 10)/10

# standardize coefficients
all_hh = all_hh %>% stdize4regr(center = TRUE, scale = TRUE, cols2ignore = c('weight', 'village', 'wealth_idx_cat', 'month_pcexpend_decile'))


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
plot_coef(stunting_fit_cfsva, cluster_col = all$village)
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
plot_coef(stunted_fit_cfsva, cluster_col = all$village, negative_good = TRUE)

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

plot_coef(stunting_tim$basic, cluster_col = all_hh$village)
# plot_relationships(stunting_tim$tim)

plot_coef(stunting_tim$tim, cluster_col = all_hh$village)
plot_coef(stunting_tim$combined_tim, cluster_col = all_hh$village)
plot_coef(stunting_tim$combined, cluster_col = all_hh$village)
plot_coef(stunting_tim$combined_plus, cluster_col = all_hh$village)

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

plot_coef(nada_m_model)
plot_coef(nada_f_model)



# stunting models: just children's data ------------------------------------------------------------------
# define models ------------------------------------------------------------------
# Variables thrown out, + rationale:
# stock_durationA: ~1000 NAs
# ill_fortnight | diarrhea + cough + fever; choosing to run separately
# re-grouping primary caregiver into +/- mother; too few obs.
# ever_breastfed: only 100 kids never were. Redundancy w/ breastfed_afterbirth
# CARI index: mixture of FCS + sh_food_exp + livelihood coping to food issues (sell assets, etc.)
# CSI index: reduced coping index-- weighted measure of whether rely

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


# Basically: anything from the mother module will have ~ 400 NAs as a result (from mothers not being interviewed.)
# Everything else throwing away < 100 observations, with the exception of baby birthweight (which is maybe impt. enough to incl. anyway.) and share_toilet


ch_models = formulas(~stuntingZ, # lhs
                     # -- child demographics --
                     child_demo1 = ~splines::bs(age_months, degree = 3, knots = 24) * sex +
                       numWomen_15_49 + caregiver_mom + interview_date,
                     child_demo2 = ~splines::bs(age_months, degree = 3, knots = 24) + sex + interview_date, 
                     
                     basic = 
                       # -- geography --
                       ~ livelihood_zone +
                       
                       # -- wealth --
                       # splines::bs(monthly_pc_expend, degree = 2),
                       wealth_idx +
                       
                       # -- hh demographics -- 
                       kids_under5,
                     
                     
                     # hh_size + crowding + pct_under7,
                     
                     # -- WASH (broken down) --
                     wash1 = ~ impr_toilet + drinkingH2O_cat + share_toilet,
                     wash2 = ~ impr_unshared_toilet + impr_water, 
                     
                     # impr_toilet + share_toilet + impr_water + 
                     # H2Otreatment_cat + time_drinkingH2O_cat + wash_knowl
                     
                     # -- rural --
                     rural1 = ~village_cat,
                     rural2 = ~rural_cat,
                     
                     # -- health (child) --
                     health1 = ~ diarrhea + fever + cough + dewormed + vitaminA + birthwt,
                     health2 = ~ diarrhea + birthwt, 
                     
                     # -- breastfeeding habits --
                     breast1 = ~ breastfed_afterbirth + fed_nonbreastmilk,
                     breast2 = ~ breastfed_afterbirth,
                     breast3 = ~ fed_nonbreastmilk,
                     
                     # -- food --
                     food1 = ~ FCS + CARI_cat + CSI, 
                     food2 = ~ FCS + CSI, # CARI contains FCS.
                     # months_food_access,
                     
                     # -- connectivity --
                     connectivity1 = ~  health_less_60min + market_less_60min + road_dist_cat + school_less_30min,
                     connectivity2 = ~  health_less_60min,
                     connectivity3 = ~  health_less_60min + road_dist_cat,
                     
                     # -- village -- 
                     village1 = ~ village_VUP + village_IDPmodel + village_structUmudugudu + village_landConsolid,
                     
                     
                     # -- mother --
                     # mom = ~ stunted_mother + num_antenatal_visits + when_antenatal +
                     # mother_mosquito_net + mother_ill_2weeks + Fe_supplements,
                     
                     # -- ag --
                     # ag = ~ TLU + land_size + hh_garden,
                     
                     
                     
                     # -- ed --
                     # ed = ~ mother_education + head_education_cat + pct_lowEd,
                     
                     simple = add_predictors(basic, child_demo2, wash2, connectivity2, health2, food2, breast2, rural2),
                     cmplx = add_predictors(basic, child_demo1, wash1, connectivity1, health1, food1, breast1, rural1),
                     breastfeeding = add_predictors(basic, child_demo2, wash2, connectivity2, health2, food2, breast3, rural2),
                     village =  add_predictors(basic, child_demo2, wash2, connectivity3, health1, food2, breast2, rural1, village1),
                     final =  add_predictors(basic, child_demo2, wash2, connectivity3, health1, food2, breast2, rural1)
)




# run models --------------------------------------------------------------

ch_fits = all %>% fit_with(lm, ch_models)

# lapply(ch_fits, function(x) summary(x))

# Plot and evaluate variations
plot_coef(ch_fits$final, cluster_col = all$village)
plot_coef(ch_fits$simple, cluster_col = all$village)
plot_coef(ch_fits$cmplx, cluster_col = all$village)
plot_coef(ch_fits$breastfeeding, cluster_col = all$village)

compare_models(ch_fits$final, ch_fits$cmplx, cluster_col = all$village) 
compare_models(ch_fits$simple, ch_fits$cmplx, cluster_col = all$village) 
compare_models(ch_fits$breastfeeding, ch_fits$simple, cluster_col = all$village) +theme_xylab()

# So: it looks like:
# * interacting sex + age doesn't seem to help.
# * village_cat is probably more useful/discerning than rural; rural+wealth index seem to compensate.
# * connectivity: health most important; keeping road distance as a proxy for isolation.
# * breastfeeding: sometimes never breastfed is important, but nothing for within 1 h of birth
# * WASH: water treatment doesn't really seem to add anything.
# * health: cough/fever, etc. don't add or subtract
# * CARI index seems redundant w/ FCS.

# model evaluation -------------------------------------------------------
# http://www.statmethods.net/stats/rdiagnostics.html

# to get rid of duplicate data:
alias(lm(stuntingZ ~ splines::bs(age_months, degree = 3, 
                                 knots = 24) * sex + village_cat + livelihood_zone + wealth_idx + 
           old_ubudehe + (impr_toilet + drinkingH2O_cat) + (diarrhea + 
                                                              fever + cough + dewormed + vitaminA + breastfed_afterbirth) + 
           (FCS + CARI_cat + CSI) + (kids_under5 + numWomen_15_49 + 
                                       caregiver_mom) + (health_less_60min + market_less_60min + 
                                                           road_dist_cat + school_less_30min), data = all))

# VIF: look at values > 2; remove VIF > 5-10 (too much co-linearity)
library(car)
vif(ch_fits$final)
sqrt(vif(ch_fits$total)) > 2 


# ch-hh models ------------------------------------------------------------
# impr_water vs. impr_water_under30 v similar
# Removed b/c seeming overlap or unimportance:
# - fever + cough + dewormed + vitaminA + 
# road_dist
# interview_date + (little variation)
# wash_knowl + 
# breastfeeding habits
#+ pct_lowEd + pct_highEd + pct_illiterate,
# mother_literate + head_literate +
# splines::bs(monthly_pc_expend, degree = 2), Running wealth index instead
# sh_food_grown

# + Fe_supplements,
# antenatal_care + redundant w/ when_antenatal
#  when_antenatal + # not as informative
# monthly_pc_expend + new_ubudehe + old_ubudehe + got_loan + asked_loan + 
# infrastruct_idx + impr_roof + impr_floor + impr_wall + own_house_cat + cookingfuel_cat +

# health_kid = ~ ,
# + birthweight_cat,

# wash2 = ~ share_toilet + time_water_source + time_drinkingH2O_cat + 
# H2Otreatment_cat + wash_knowl + wash_beforecook + wash_kidtoilet + wash_beforeeat + 
# wash_aftertoilet + wash_ifdirty,

# food2 = ~ months_food_access + protein_days + ironrich_days + vitAfruitveg_days + sh_food_grown,
# pref_staple + child_meal_freq + CARI_cat + food_access_prob + food_access_year_cat + 
# sh_food_purchased + sh_food_expend,
# DDS + dietDiv_W24h + dietDiv_W24h_cat + HDDS_24h + 
# milk_days + 

# livelihood = ~ ,
# growing_beans + hh_occup_cat + sh_agricultural_production + sh_labour_ag_work + 
# sh_unskilled_labour + num_jobs 
# + mostly_selling + mostly_consuming,

# village = ~village_VUP + village_noSchemes + village_IDPmodel + village_landConsolid + village_structUmudugudu,
# + mother_ill_2weeks,
# mother_BMI: replaced stunted w/, but didn't affect.

ch_hh_models = formulas(~stuntingZ, # lhs
                        # -- child demographics --
                        basic = ~ 
                          # age_months +
                          splines::bs(age_months, degree = 3, knots = 24) +
                          sex + 
                          
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
                          diarrhea + birthwt +
                          
                          # -- connectivity --
                          health_less_60min + 
                          
                          # -- ag --
                          TLU + land_size_cat + hh_garden +
                          
                          # -- ed --
                          head_education_cat,
                        
                        
                        # -- food --
                        food1 = ~ FCS +
                          CSI_cat + # CARI contains FCS.
                          months_food_access,
                        food2 = ~ CSI_cat +
                          months_food_access +
                          # ironRich_binary + proteinRich_binary + vitA_binary, # converting to binaries eliminates the isolated 
                          protein_days + ironrich_days + vitAfruitveg_days,
                        food3 = ~ CSI_cat + 
                          months_food_access +
                          milk_meat_days +
                          fruit_veg_days,
                        
                        # -- mother --
                        mom1 = ~  mother_age + mother_age_sq +
                          mother_education + 
                          # -- mother health --
                          num_antenatal_visits +
                          mother_mosquito_net,
                        
                        # contains ~ 400 NAs --> seriously cuts down sample size
                        
                        mom2 = ~stunted_mother,
                        # contains ~ 700 NAs --> seriously cuts down sample size
                        
                        shk = ~ shock_drought + shock_illness +
                          food_assistance + financial_assistance + ag_assistance,
                        
                        
                        
                        # -- wealth --
                        wealth1 = ~ wealth_idx_num, 
                        wealth2 = ~ splines::bs(monthly_pc_expend, degree = 3, knot = 0.4),
                        wealth3 = ~ wealth_idx,
                        
                        geo = ~ livelihood_zone,
                        
                        fcs = add_predictors(basic, food1, geo, wealth1),
                        protRich = add_predictors(basic, food2, geo, wealth1),
                        foods = add_predictors(basic, food3, geo, wealth2),
                        
                        # mother = add_predictors(basic, mom1, food1, geo),               
                        mother = add_predictors(basic, mom1, mom2, food1, wealth1, geo),                        
                        all = add_predictors(basic, mom1, mom2, food1, shk, wealth1, geo),
                        all2 = add_predictors(basic, mom1, mom2, food2, shk, wealth1, geo),
                        all_PC = add_predictors(basic, mom1, mom2, food1, shk, wealth2, geo),
                        all_WI = add_predictors(basic, mom1, mom2, food1, shk, wealth3, geo),
                        nogeo = add_predictors(basic, mom1, mom2, food1, shk, wealth1)
)

stunting_fits = all_hh %>% fit_with(lm, ch_hh_models)

# lapply(ch_fits, function(x) summary(x))


# Plot and evaluate variations
plot_coef(stunting_fits$fcs, cluster_col = all_hh$village)
plot_coef(stunting_fits$protRich, cluster_col = all_hh$village)
plot_coef(stunting_fits$foods, cluster_col = all_hh$village)
plot_coef(stunting_fits$mother, cluster_col = all_hh$village)
# plot_coef(stunting_fits$momBMI, cluster_col = all_hh$village)
plot_coef(stunting_fits$all, cluster_col = all_hh$village)
plot_coef(stunting_fits$all_WI, cluster_col = all_hh$village)
plot_coef(stunting_fits$all_WI, cluster_col = all_hh$village)
plot_coef(stunting_fits$nogeo, cluster_col = all_hh$village)

test = lm(formula = stuntingZ ~ splines::bs(age_months, degree = 3, 
                                            knots = 24) + 
            sex + 
            rural_cat + 
            kids_under5 + 
            # crowding + 
            # fem_head + head_age + head_age_sq + numWomen_18plus + hh_occup_cat + 
            impr_unshared_toilet + 
            impr_water_30min + 
            diarrhea + 
            # birthwt +
            low_birthwt +
            health_less_60min + 
            TLU + 
            land_size_cat + 
            # hh_garden + 
            head_education_cat + 
            mother_age + mother_age_sq + 
            mother_education + 
            num_antenatal_visits + 
            mother_mosquito_net + 
            stunted_mother + 
            FCS*months_food_access + 
            CSI_cat +  
            log_pcexp + 
            livelihood_zone, data = all_hh)

plot_coef(test, cluster_col = all_hh$village)

# Variation b/w the wealth options doesn't seem to matter too much.
compare_models(list('all' = stunting_fits$all,
                    'all_WI' = stunting_fits$all_WI,
                    'all_PC' = stunting_fits$all_PC
), 
filter_insignificant = T)

compare_models(list('all' = stunting_fits$all,
                    'no-geo' = stunting_fits$nogeo,
                    'fcs' = stunting_fits$fcs,
                    # 'protRich' = stunting_fits$protRich,
                    'mother' = stunting_fits$mother),
                    # 'momBMI' = stunting_fits$momBMI), 
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

compare_models(list('fcs' = stunting_fits$all,
                    'broken' = stunting_fits$all2),
                    cluster_col = all$village, sort_by_est = FALSE) 
# run M and F models ------------------------------------------------------
male_fit = lm(stuntingZ ~ splines::bs(age_months, degree = 3, 
                                      knots = 24) + rural_cat + kids_under5 + crowding + 
                fem_head + head_age + head_age_sq + numWomen_18plus + hh_occup_cat + 
                impr_unshared_toilet + impr_water_30min + diarrhea + birthwt + 
                health_less_60min + TLU + land_size_cat + hh_garden + head_education_cat + 
                (mother_age + mother_age_sq + mother_education + num_antenatal_visits + 
                   mother_mosquito_net) + stunted_mother + (FCS + CSI_cat + 
                                                              months_food_access) + (shock_drought + shock_illness + food_assistance + 
                                                                                       financial_assistance + ag_assistance) + wealth_idx_num + 
                livelihood_zone, data = males_hh)

female_fit = lm(stuntingZ ~ splines::bs(age_months, degree = 3, 
                                        knots = 24) + rural_cat + kids_under5 + crowding + 
                  fem_head + head_age + head_age_sq + numWomen_18plus + hh_occup_cat + 
                  impr_unshared_toilet + impr_water_30min + diarrhea + birthwt + 
                  health_less_60min + TLU + land_size_cat + hh_garden + head_education_cat + 
                  (mother_age + mother_age_sq + mother_education + num_antenatal_visits + 
                     mother_mosquito_net) + stunted_mother + (FCS + CSI_cat + 
                                                                months_food_access) + (shock_drought + shock_illness + food_assistance + 
                                                                                         financial_assistance + ag_assistance) + wealth_idx_num + 
                  livelihood_zone, data = females_hh)

compare_models(list('male' = male_fit,
                    'female' = female_fit,
                    'all' = stunting_fits$all), 
filter_insignificant = F,
sort_by_est = F, alpha_insignificant = 0.2)
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



