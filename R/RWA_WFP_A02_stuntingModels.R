males = ch %>% filter(!is.na(isStunted), 
                      sex == 'Male')

females = ch %>% filter(!is.na(isStunted), 
                      sex == 'Female')

stunting_models = formulas(~stuntingZ, # lhs
                           demo_child = 
                             # -- demographic variables --
                             ~ splines::bs(age_months, degree = 3, knots = 2) +
                             interview_date,
                           
                           wealth = ~wealth_idx,
                           geo = ~livelihood_zone + rural_cat,
                           wash = ~impr_unshared_toilet + impr_water + diarrhea,
                           nutrition = ~FCS + CARI_cat + ever_breastfed,
                           health = fever + birthwt, 
                           ed = mother_literate, mother_education,
                           comb = add_predictors(geo, demo_child, wealth, wash, nutrition, ed))

stunting_fits_m = males %>% fit_with(lm, stunting_models)
stunting_fits_f = females %>% fit_with(lm, stunting_models)

# lapply(stunting_fits_f, function(x) summary(x))

summary(stunting_fits_m$comb)
summary(stunting_fits_f$comb)


# basic fit ---------------------------------------------------------------

summary(lm(formula = stuntingZ ~ wealth_idx + interview_date + FS_final  + diarrhea + 
             splines::bs(age_months, degree = 3, knots = 2) + 
             milk_days + meat_days + impr_water + impr_toilet, 
           data = ch_hh %>% filter(!is.na(isStunted), sex == 'Male')))

summary(lm(formula = stuntingZ ~ wealth_idx + interview_date + FS_final  + diarrhea +
             age_months + milk_days + meat_days + impr_water + impr_toilet, data = ch_hh %>% filter(!is.na(isStunted), sex == 'Male')))
