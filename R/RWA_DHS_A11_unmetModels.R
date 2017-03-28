
# Fertility models --------------------------------------------------------
# To complement Tim's model on who uses modern contraception, who is likely to have 
# unmet need? 
# Those who aren't using contraception are two groups:
# 1) the women who want to have more children (therefore needs are met)
# 2) those who would have liked to have contraception to limit # of children or to space them out.


# load data ----------------------------------------------------------------
source('~/GitHub/Rwanda/R/RWA_DHS_08_fertility.R')
source('~/GitHub/Rwanda/R/RWA_DHS_09_fertility2010.R')
library(mfx)
library(car)

# model notes --------------------------------------------------------------
# • Only running women currently in unions
# • Though the DHS considers met need if want baby w/i next 2 years, running a general model for women who want more babies.


# scale factors -----------------------------------------------------------
w14_scaled = w14 %>% 
  filter(totLiving > 0) %>%  # include only women who already have a child
  stdize4regr()

w10_scaled = w10 %>% 
  filter(totLiving > 0) %>%  # include only women who already have a child
  stdize4regr()

# model on who wants more babies ------------------------------------------
baby_models = formulas(~moreChild_binary,
                       basic = 
                         # -- demographics --
                         ~age_rural +
                         ageGap +
                         religion +
                         age_firstSex +
                         
                         # numChildUnd5 +
                         totLiving*hasSon +
                         
                         # hasSon + 
                         # hasDaughter +
                         
                         # -- education --
                         educ +
                         # educPartner +
                         # occup_cat +
                         # occupHusGroup +
                         
                         # -- wealth --
                         wealth + 
                         
                         # -- geo / connectivity --
                         lvdzone + 
                         altitude +
                         # rural +
                         
                         # -- health -- 
                         bedNetUse + 
                         went_doctor + 
                         # FPatHealth + # too many unobs? ~ 1/2 didn't go to doc.
                         health_dist +
                         health_money +
                         # goHealth_alone + # too many unobs.
                         fp_radio +
                         fp_tv +
                         fp_news +
                         
                         # -- empowerment --
                         own_land +
                         own_house +
                         beating_idx,
                       
                       # -- male fertility desires --
                       male_ed = add_predictors(basic, ~educPartner),
                       male_agree = add_predictors(basic, ~occup_cat, ~moreChild_agree),
                       occup = add_predictors(basic, ~occup_cat))

children_14 = w14_scaled %>% fit_with(glm, baby_models, family = binomial(link = 'logit'))

summary(children_14$basic)

plot_coef(children_14$basic)


# odds ratio --------------------------------------------------------------
children_or14 = w14_scaled %>% fit_with(logitor, baby_models)

children_or14$basic$oddsratio
# marginal effects --------------------------------------------------------
children_me14 = w14_scaled %>% fit_with(logitmfx, baby_models)


# 2010  -------------------------------------------------------------------

children_10 = w10_scaled %>% fit_with(glm, baby_models, family = binomial(link = 'logit'))

summary(children_10$basic)

children_or10 = w10_scaled %>% fit_with(logitor, baby_models)


# compare 2010/2014 -------------------------------------------------------

or10 = children_or10$basic$oddsratio
or10 = data.frame(or10) %>% 
  mutate(var =  row.names(or10)) %>% 
  dplyr::select(or10 = OddsRatio, p10 = P..z., var)

or14 = children_or14$basic$oddsratio
or14 = data.frame(or14) %>% 
  mutate(var =  row.names(or14)) %>% 
  dplyr::select(or14 = OddsRatio, p14 = P..z., var)

or = full_join(or10, or14)
write.csv(or, 'odds.csv')



# Regressing total children -----------------------------------------------
# scale factors -----------------------------------------------------------
# Only including women in unions.
w14_scaled = w14 %>% 
  stdize4regr()

w10_scaled = w10 %>% 
  stdize4regr()

# model on who wants more babies ------------------------------------------
numChild_models = formulas(~totLiving,
                       basic = 
                         # -- demographics --
                         # ~ageGroup:rural +
                         ~age_rural +
                         hh_ownland +
                         ageGap +
                         religion +
                         age_firstSex +
                         
                         # -- child demographics --
                         childDied + 
                         
                         
                         # -- education --
                         educ +
                         # educPartner +
                         # occup_cat +
                         # occupHusGroup +
                         
                         # -- wealth --
                         wealth + 
                         
                         # -- geo / connectivity --
                         lvdzone + 
                         altitude +
                         
                         # -- health -- 
                         bedNetUse + 
                         went_doctor + 
                         # FPatHealth + # too many unobs? ~ 1/2 didn't go to doc.
                         health_dist +
                         health_money +
                         # goHealth_alone + # too many unobs.
                         fp_radio +
                         fp_tv +
                         fp_news +
                         
                         # -- empowerment --
                         # own_land +
                         # own_house +
                         beating_idx,
                       
                       # -- male fertility desires --
                       male_ed = add_predictors(basic, ~educPartner),
                       prev_kids = add_predictors(basic, ~hasSon, ~hasDaughter),
                       occup = add_predictors(basic, ~occup_cat))

numChild_14 = w14_scaled %>% fit_with(lm, numChild_models)
numChild_10 = w10_scaled %>% fit_with(lm, numChild_models)

# -- 2014 models --
summary(numChild_14$male_ed)
llamar::plot_coef(numChild_14$basic, negative_good = TRUE, negative_ontop = FALSE)
llamar::plot_coef(numChild_14$prev_kids, negative_good = TRUE, negative_ontop = FALSE)
llamar::plot_coef(numChild_14$male_ed, negative_good = TRUE, negative_ontop = FALSE) + annotate(label = 'Number of living kids, 2014/2015', geom = 'text', x = 0.3, y = 37, colour = grey70K, size = 6, family = 'Lato Light')
llamar::plot_coef(numChild_14$occup, negative_good = TRUE, negative_ontop = FALSE)

# -- 2010 models --
llamar::plot_coef(numChild_10$basic, negative_good = TRUE, negative_ontop = FALSE)
llamar::plot_coef(numChild_10$male_ed, negative_good = TRUE, negative_ontop = FALSE) + annotate(label = 'Number of living kids, 2010', geom = 'text', x = 0.3, y = 37, colour = grey70K, size = 6, family = 'Lato Light')


# Including whether already has a son/daughter definitely increases fit, but worried it's just overfitting / bias towards lg. familoies
llamar::compare_models(list('basic' = numChild_14$basic,
                            'prev' = numChild_14$prev_kids), negative_good = TRUE, negative_ontop = FALSE)


# Male ed doesn't seem to matter.  Occupation matters a bit, but the classification of "Ag employee" may have changed b/w 2010 and 2015.
llamar::compare_models(list('basic' = numChild_14$basic,
                            'male_ed' = numChild_14$male_ed,
                            'occup' = numChild_14$occup), negative_good = TRUE, negative_ontop = FALSE)

car::vif(numChild_14$male_ed)
sqrt(car::vif(numChild_14$basic)) > 2


llamar::compare_models(list('2014' = numChild_14$male_ed,
                            '2010' = numChild_10$male_ed), sort_by_est = TRUE, negative_good = TRUE, negative_ontop = FALSE)



# quick #kids map ------------------------------------------------------
library(leaflet)
contPal = colorQuantile(palette = 'YlGnBu', domain = 0:12)
leaflet(data = w14) %>%
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom = 6, maxZoom  = 11, opacity = 0.8)) %>%
  addCircleMarkers(lat = ~ latnum, lng = ~longnum,stroke = FALSE,
                   radius = 5,
             color = ~contPal(totLiving))


# merge all regression results together -----------------------------------
library(broom)

# nc10_1 = broom::tidy(numChild_10$basic)
nc10_2 = broom::tidy(numChild_10$male_ed)
nc10_3 = broom::tidy(numChild_10$prev_kids)
nc10_4 = broom::tidy(numChild_10$occup)

# number of children in 2010
nc10 = full_join(nc10_2, nc10_3, by='term')
nc10 = full_join(nc10, nc10_4, by='term')

write.csv(nc10, '~/Creative Cloud Files/MAV/Projects/RWA_2017_01_06_LAM/regression results/numberChildren2010.csv')

# nc14_1 = broom::tidy(numChild_10$basic)
nc14_2 = broom::tidy(numChild_14$male_ed)
nc14_3 = broom::tidy(numChild_14$prev_kids)
nc14_4 = broom::tidy(numChild_14$occup)

# number of children in 2010
nc14 = full_join(nc14_2, nc14_3, by='term')
nc14 = full_join(nc14, nc14_4, by='term')

write.csv(nc14, '~/Creative Cloud Files/MAV/Projects/RWA_2017_01_06_LAM/regression results/numberChildren2014.csv')


# desires for having more children
# want10_1 = broom::tidy(children_10$basic)
want10_2 = broom::tidy(children_10$male_ed)
want10_3 = broom::tidy(children_10$male_agree)
want10_4 = broom::tidy(children_10$occup)


# number of children in 2010
# want10 = full_join(want10_1, want10_2, by='term')
want10 = full_join(want10_2, want10_3, by='term')
want10 = full_join(want10, want10_4, by='term')

write.csv(want10, '~/Creative Cloud Files/MAV/Projects/RWA_2017_01_06_LAM/regression results/wantChildren2010.csv')


# want14_1 = broom::tidy(children_14$basic)
want14_2 = broom::tidy(children_14$male_ed)
want14_3 = broom::tidy(children_14$male_agree)
want14_4 = broom::tidy(children_14$occup)


# number of children in 2010
# want14 = full_join(want14_1, want14_2, by='term')
want14 = full_join(want14_2, want14_3, by='term')
want14 = full_join(want14, want14_4, by='term')

write.csv(want14, '~/Creative Cloud Files/MAV/Projects/RWA_2017_01_06_LAM/regression results/wantChildren2014.csv')