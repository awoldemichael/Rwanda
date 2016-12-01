
# Fertility models --------------------------------------------------------
# To complement Tim's model on who uses modern contraception, who is likely to have 
# unmet need? 
# Those who aren't using contraception are two groups:
# 1) the women who want to have more children (therefore needs are met)
# 2) those who would have liked to have contraception to limit # of children or to space them out.


# load data ----------------------------------------------------------------
# source('~/GitHub/Rwanda/R/RWA_DHS_08_fertility.R')


# model notes --------------------------------------------------------------
# • Only running women currently in unions
# • Though the DHS considers met need if want baby w/i next 2 years, running a general model for women who want more babies.


# scale factors -----------------------------------------------------------
w14_scaled = w14 %>% stdize4regr()

# model on who wants more babies ------------------------------------------

want_children <- glm(moreChild_binary ~
                       # -- demographics --
                       age*rural +
                       age_gap +
                       religion +
                       age_firstSex +
                       
                       # numChildUnd5 +
                       totLiving*hasSon +
                      
                       # hasSon + 
                       # hasDaughter +
                       
                       # -- education --
                       educ +
                       educPartner +
                       # occupGroup +
                       
                       # -- wealth --
                       wealth + 
                       
                       # -- geo / connectivity --
                       lvdzone + 
                       altitude +
                       # rural +
                       
                       # -- health -- 
                       bedNetUse + 
                       went_doctor + 
                       # FPatHealth +
                       health_dist +
                       health_money +
                       goHealth_alone +
                       fp_radio +
                       fp_tv +
                       fp_news +
                       
                       # -- empowerment --
                       own_land +
                       own_house +
                       beating_idx +
                       
                       # -- male fertility desires --
                       moreChildHus,
                     data = w14_scaled, family = binomial(link = 'logit')) 

summary(want_children)

