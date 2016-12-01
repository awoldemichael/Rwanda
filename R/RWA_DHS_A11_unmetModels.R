
# Fertility models --------------------------------------------------------
# To complement Tim's model on who uses modern contraception, who is likely to have 
# unmet need? 
# Those who aren't using contraception are two groups:
# 1) the women who want to have more children (therefore needs are met)
# 2) those who would have liked to have contraception to limit # of children or to space them out.


# load data ----------------------------------------------------------------
source('~/GitHub/Rwanda/R/RWA_DHS_08_fertility.R')
library(mfx)

# model notes --------------------------------------------------------------
# • Only running women currently in unions
# • Though the DHS considers met need if want baby w/i next 2 years, running a general model for women who want more babies.


# scale factors -----------------------------------------------------------
w14_scaled = w14 %>% 
  filter(totLiving > 0) %>%  # include only women who already have a child
  stdize4regr()

# model on who wants more babies ------------------------------------------
baby_models = formulas(~moreChild_binary,
                       basic = 
                         # -- demographics --
                         ~age*rural +
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

want_children = w14_scaled %>% fit_with(glm, baby_models, family = binomial(link = 'logit'))

summary(want_children$basic)


# odds ratio --------------------------------------------------------------
children_or = w14_scaled %>% fit_with(logitor, baby_models)


# marginal effects --------------------------------------------------------
children_me = w14_scaled %>% fit_with(logitmfx, baby_models)


