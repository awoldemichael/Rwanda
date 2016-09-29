# Variables from the CFSVA analysis taken from http://www.moh.gov.rw/fileadmin/templates/Summit3/8_Regional_VAriation.pdf
# and the CVSFA report. Assuming linear fit for all variables (?); looks like in the interim pdf they were just running kids < 2.
# Confusing b/c there's no slope variable in dataset, and they also reference the 2013 CFSVA (?)



# Remove NAs from stunting ------------------------------------------------

males = ch %>% filter(!is.na(isStunted), 
                      sex == 'Male')

females = ch %>% filter(!is.na(isStunted), 
                        sex == 'Female')

all = ch %>% filter(!is.na(isStunted))


# Check CFSVA lit models --------------------------------------------------

# stunting score
stunting_fit_cfsva = lm(formula = stuntingZ ~ sex + age_months + low_birthwt +
                          beans_W24h + milk_W24h + 
                          stunted_mother + mother_age + mother_education +
                          wealth_idx_cat + new_ubudehe + 
                          rural_cat + livelihood_zone +
                          CARI_cat + diarrhea +
                          road_distance + school_dist_cat+ market_dist_cat + health_dist_cat,
                        data = all)
summary(stunting_fit_cfsva)

# stunting score
stunted_fit_cfsva = lm(formula = isStunted ~ sex + age_months + low_birthwt +
                         beans_W24h + milk_W24h + superCereal_W24h + nuts_W24h + eggs_W24h + starch_W24h + protein_W24h + 
                         greenVeg_W24h + otherVeg_W24h + orangeFruits_W24h + otherFruit_W24h +
                         stunted_mother + mother_age + mother_education +
                         wealth_idx_cat + new_ubudehe + 
                         rural_cat + livelihood_zone +
                         CARI_cat + diarrhea +
                         road_distance + school_dist_cat+ market_dist_cat + health_dist_cat,
                       data = all)

summary(stunted_fit_cfsva)

# define models ------------------------------------------------------------------

stunting_models = formulas(~stuntingZ, # lhs
                           cfsva = ~ age_months + low_birthwt +
                             beans_W24h + milk_W24h + 
                             stunted_mother + mother_age + mother_education +
                             wealth_idx_cat + new_ubudehe + 
                             rural_cat + livelihood_zone +
                             CARI_cat + diarrhea +
                             market_distance + school_dist_cat+ market_dist_cat + health_dist_cat,
                           
                           demo_child = 
                             # -- demographic variables --
                             ~ splines::bs(age_months, degree = 3, knots = 24) +
                             interview_date,
                           
                           wealth = ~wealth_idx,
                           geo = ~livelihood_zone + rural_cat,
                           wash = ~impr_unshared_toilet + impr_water + diarrhea,
                           nutrition = ~FCS + CARI_cat + ever_breastfed,
                           health = ~fever + birthwt, 
                           ed = ~mother_literate + mother_education,
                           
                           comb = add_predictors(geo, demo_child, wealth, wash, nutrition, ed)
                           
                           
)


# run models --------------------------------------------------------------

stunting_fits_m = males %>% fit_with(lm, stunting_models)
stunting_fits_f = females %>% fit_with(lm, stunting_models)
stunting_fits_all = all %>% fit_with(lm, stunting_models)

lapply(stunting_fits_f, function(x) summary(x))

summary(stunting_fits_m$comb)
summary(stunting_fits_f$comb)


# basic fit ---------------------------------------------------------------

summary(lm(formula = stuntingZ ~ wealth_idx + interview_date + FS_final  + diarrhea + 
             splines::bs(age_months, degree = 3, knots = 24) + 
             milk_days + meat_days + impr_water + impr_toilet, 
           data = ch_hh %>% filter(!is.na(isStunted), sex == 'Male')))

summary(lm(formula = stuntingZ ~ wealth_idx + interview_date + FS_final  + diarrhea +
             age_months + milk_days + meat_days + impr_water + impr_toilet, data = ch_hh %>% filter(!is.na(isStunted), sex == 'Male')))


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
             # bike+bankAccount+
             diarrhea +
             month.y +
             rural_cat +
             livelihood_zone, 
           data = ch_hh, family = binomial(link = 'logit')) 

summary(nada_mf)

# ! need to refactor post merge


# prelim fit ---------------------------------------------------------------

summary(lm(formula = stuntingZ ~ 
             livelihood_zone + 
             rural_cat +
             # wealth_idx + 
             monthly_pc_expend + 
             interview_date + 
             diarrhea + 
             impr_unshared_toilet + impr_water_30min +
             health_less_60min + market_dist_cat +
             TLU + 
             # own_land + 
             land_size + 
             hh_occup_cat +
             # sh_agricultural_production + sh_unskilled_labour + sh_labour_ag_work +
             growing_beans + growing_maize + growing_s_potato + growing_cassava + 
             growing_i_potato + growing_sorghum + growing_banana_cooking + growing_banana_wine +
             # pref_staple + 
             FCS + child_meal_freq +
             CARI_cat + 
             # food_access_prob + 
             months_food_access + 
             stunted_mother + mother_education + head_education_cat + mother_mosquito_net +
             sex + birthwt +
             splines::bs(age_months, degree = 3, knots = 24),
           data = ch_hh %>% filter(!is.na(isStunted))))


summary(lm(formula = stuntingZ ~ 
             livelihood_zone + 
             rural_cat +
             wealth_idx +
             monthly_pc_expend + 
             interview_date + 
             diarrhea + 
             impr_unshared_toilet + impr_water_30min +
             health_less_60min + market_dist_cat +
             TLU + 
             # own_land + 
             land_size + 
             hh_occup_cat +
             # sh_agricultural_production + sh_unskilled_labour + sh_labour_ag_work +
             hh_garden + 
             growing_beans + growing_maize + growing_s_potato + growing_cassava + 
             growing_i_potato + growing_sorghum + growing_banana_cooking + growing_banana_wine +
             # pref_staple + 
             meat_days + pulse_days + veg_days + milk_days +
             child_meal_freq +
             # CARI_cat + 
             # food_access_prob + 
             months_food_access + 
             stunted_mother + mother_education + head_education_cat + mother_mosquito_net +
             num_antenatal_visits +
             birthwt +
             splines::bs(age_months, degree = 3, knots = 24) * sex,
           data = ch_hh %>% filter(!is.na(isStunted))))
