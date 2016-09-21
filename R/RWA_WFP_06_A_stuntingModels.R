stunting_models = formulas(~stuntingZ, # lhs
                           demo_child = 
                             # -- demographic variables --
                             ~age_months +
                             sex,
                           demo_child_interacted = ~age_months * sex,
                             
                           wealth = ~wealth_idx,
                           both = add_predictors(demo_child_interacted, wealth))

stunting_fits = ch %>% fit_with(lm, stunting_models)
                           
                             # # -- geo --
                             # livelihood_zone + 
                             # 
                             # # -- WASH --
                             # impr_unshared_toilet +
                             # impr_water +
                             # wash_knowl +
                             # 
                             # # -- nutrition --
                             # ever_breastfed + 
                             # FCS)
# interview_date


# basic fit ---------------------------------------------------------------

summary(lm(formula = stuntingZ ~ wealth_idx + interview_date, data = ch))
