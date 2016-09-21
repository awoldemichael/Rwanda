# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_05_importMother.R: import data from the mother module
#
# Script to pull stunting data and associated household- or
# child-level data for Rwanda from the CFSVA dataset
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 14 September 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License
#
# -------------------------------------------------------------------------

# Import in the raw data --------------------------------------------------
# Raw data is contained in three files:
# children's data: 'cfsva-2015-child-DB- annex.sav'
# household-level data:  'cfsva-2015-master-DB- annex.sav'
# women's data: 'cfsva-2015-mother-DB- annex.sav'

women_raw = read_sav(paste0(baseDir, 'RW_2015_CFSVA/cfsva-2015-mother-DB- annex.sav'))


# Can women be merged w/ kids? --------------------------------------------

x = left_join(ch, women_raw, by = c("parent_id" = "PARENT_KEY"))

# Result: some mergal, but also extra ch values (presumably from non-unqiueness of argument)

# PARENT_KEY isn't unique...
nrow(women_raw %>% select(PARENT_KEY) %>% distinct())

# .., but KEY is (mother's id?)
nrow(women_raw %>% select(KEY) %>% distinct())

# Unfortunately, mother's key in children's dataset is missing 415 values.
x = left_join(children_raw, women_raw, by = c('MHN_KEY' = 'KEY'))

# Simplest thing should be to do parent id + age of mother.
nrow(women_raw %>% select(PARENT_KEY, S13_02_2) %>% distinct())
# Okay.  25 mismatches.

# Throwing in BMI:
nrow(women_raw %>% select(PARENT_KEY, S13_02_2, BMI) %>% distinct())

# Yippee!
x = left_join(ch, women_raw, by = c("parent_id" = "PARENT_KEY", "mother_age" = "S13_02_2", "mother_BMI" = "BMI")) # unique

# Only doesn't merge properly, b/c imprecise precision of BMI.
x  %>% group_by(WDDS) %>% summarise(n()) # WDDS is only in mother's module, and contains no NAs

ch = ch %>% mutate(rounded_BMI = round(mother_BMI, 2))
women_raw = women_raw %>% mutate(rounded_BMI = round(BMI, 2))

x = left_join(ch, women_raw, by = c("parent_id" = "PARENT_KEY", "mother_age" = "S13_02_2", "rounded_BMI" = "rounded_BMI")) # unique

# Only doesn't merge properly, b/c imprecise precision of BMI.
x  %>% group_by(WDDS) %>% summarise(n()) # WDDS is only in mother's module, and contains no NAs

# Still unique
nrow(women_raw %>% mutate(bmi = round(BMI, 2)) %>% select(PARENT_KEY, S13_02_2, bmi) %>% distinct())
# down to 414.  (sigh)  NA for BMIs?  Gonna have to do this stepwise

# investigating whether there are interesting unique variables in the women's  --------



# pull relevant vars ------------------------------------------------------
women = women_raw %>% 
  select(KEY, PARENT_KEY)