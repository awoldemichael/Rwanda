# RWA_WFP_calcAvgs
# Laura Hughes, lhughes@usaid.gov, 6 April 2017
# Data are from the 2012 or 2015 Rwanda Comprehensive Food and Security Vulnerability Analysis,
# or the 2010 or 2014/2015 Rwanda Demographic and Health Survey


# Script to pull or calculate all derived summary stats at a given geographic level.
# All values are calculated at two geographic levels:
# 1. At the district (Admin2) level, with sampling weights applied if child-level measurement.
# For hh-level measurement (like FCS), can't apply weights b/c no PSU within the dataset.
# 2. At the FEWS NET livelihood zone level, without any weights.

# For DHS, indicators are:
# • [stunted] stunting (percentage)
# • [stuntingZ] stunting (mean z-score)
# • [wealth_idx] wealth index (mean index score)

# For CFSVA, indicators are:
# • [stunted] stunting (percentage height-for-age z-scores < -2)
# • [stuntingZ] stunting (mean z-score)
# • [wasted] wasting (percentage weight-for-age z-scores < -2)
# • [underwt] underweight (percentage height-for-weight < -2)
# • [median_pcExp] household-reported per capita monthly expenditures (median, in Rwandan francs)
# • [growing_XXXX] households growing a particular crop (percentage)
# • [FCS] food consumption score (mean)
# • [HDDS_24h] household dietary diversity score, reported from a 24 h recall (mean, from 0-12)
# • [months_food_access] combination of CFSVA-processed variables "FoodAccess" and "Months_FA" (mean number of months hh had a food access issue)
# • [CARI] CFSVA CARI index; combo of FCS and coping strategies index (percentage of each of the food secure categories)
# • [foodAccess_cat] category of food access issues over past year (percentage of each category)

# For all estimates, the mean (unless median where indicated) are provided, 
# along with the sample size [N], standard error [se], 
# and the lower [lb] and uppper bounds [ub] of a 95% confidence interval
# e.g. for stunting percentage there will be: stunted (mean), 
# stunted_lb (lower bound 95% CI), stunted_ub (upper bound 95% CI)


# setup -------------------------------------------------------------------
setwd('~/GitHub/Rwanda/R/')
exportDir = '~/Documents/USAID/Rwanda/processeddata/'  


# source DHS stats --------------------------------------------------------


# source CFSVA data -------------------------------------------------------
source('RWA_WFP_run2015.R')


# 2015 data ---------------------------------------------------------------

# using median per capita exp, since distribution seems to be skewed.
ggplot(hh, aes(x = monthly_pc_expend)) + geom_histogram(binwidth = 1000)
ggplot(hh, aes(x = log(monthly_pc_expend))) + geom_histogram(binwidth = 0.25)





# calculate CFSVA @ district & LZ level ----------------------------------------
# -- hh-level --

calcAvgs = function(df, variable, combined, by_var = 'livelihood_zone') {
  avg = calcPtEst(hh, variable, by_var = by_var, use_weights = FALSE)
  colnames(avg) = c(colnames(avg)[1], lapply(colnames(avg)[-1], function (x) paste0(variable, '_', x)))
  
  combined = full_join(combined, avg, by = by_var)
  return(combined)
}

cf_lz = data.frame(livelihood_zone = unique(hh$livelihood_zone))
cf_dist = data.frame(admin2 = unique(hh$admin2))

# Vars to loop over
vars = c(colnames(hh %>% select(contains('growing'))), 
         'FCS', 'months_food_access', 'HDDS_24h',
         'own_livestock', 'own_cattle', 'manage_livestock')

for (var in vars) {
  cf_lz = calcAvgs(hh, var, by_var = 'livelihood_zone', cf_lz)
  
  cf_dist = calcAvgs(hh, var, by_var = 'admin2', cf_dist)
}

# using median per capita exp, since distribution seems to be skewed.
ggplot(hh, aes(x = monthly_pc_expend)) + geom_histogram(binwidth = 1000)
ggplot(hh, aes(x = log(monthly_pc_expend))) + geom_histogram(binwidth = 0.25)


pcExp_dist = hh %>% 
  group_by(admin2) %>% 
  summarise(median_pc_expend = median(monthly_pc_expend), median_pc_expend_N = n())

pcExp_lz = hh %>% 
  group_by(livelihood_zone) %>% 
  summarise(median_pc_expend = median(monthly_pc_expend), median_pc_expend_N = n())


# -- child-level --
wasting_admin2 = calcPtEst(ch, 'isWasted', by_var = 'admin2',
                       psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

wasting_lz = calcPtEst(ch, 'isWasted', by_var = 'livelihood_zone', use_weights = FALSE)

underwt_admin2 = calcPtEst(ch, 'isUnderwt', by_var = 'admin2',
                                  psu_var = 'village', strata_var = 'admin2', weight_var = 'weight')

underwt_lz = calcPtEst(ch, 'isWasted', by_var = 'livelihood_zone', use_weights = FALSE)


# merge together all the CFSVA data ---------------------------------------
cf_dist = full_join(cf_dist, pcExp_dist, by = "admin2")

# stunting values calculated in "RWA_WFP_calcStunting.R"
colnames(stunting_admin2_cfsva) = c(colnames(stunting_admin2_cfsva)[1], 
                                    lapply(colnames(stunting_admin2_cfsva)[-1], function (x) paste0('stunted_', x)))
colnames(stuntingZ_admin2_cfsva) = c(colnames(stuntingZ_admin2_cfsva)[1], 
                                    lapply(colnames(stuntingZ_admin2_cfsva)[-1], function (x) paste0('stuntingZ_', x)))
colnames(wasting_admin2) = c(colnames(wasting_admin2)[1], 
                                     lapply(colnames(wasting_admin2)[-1], function (x) paste0('wasted_', x)))
colnames(underwt_admin2) = c(colnames(underwt_admin2)[1], 
                                     lapply(colnames(underwt_admin2)[-1], function (x) paste0('underwt_', x)))


cf_dist = full_join(cf_dist, stunting_admin2_cfsva, by = "admin2")
cf_dist = full_join(cf_dist, stuntingZ_admin2_cfsva, by = "admin2")
cf_dist = full_join(cf_dist, wasting_admin2, by = "admin2")
cf_dist = full_join(cf_dist, underwt_admin2, by = "admin2")


cf_lz = full_join(cf_lz, stunting_lz_cfsva, by = "livelihood_zone")
cf_lz = full_join(cf_lz, stuntingZ_lz_cfsva, by = "livelihood_zone")
cf_lz = full_join(cf_lz, wasting_lz, by = "livelihood_zone")
cf_lz = full_join(cf_lz, underwt_lz, by = "livelihood_zone")


# save --------------------------------------------------------------------
write.csv(cf_dist, paste0(exportDir, 'RWA_CFSVA_2015_district.csv'))
write.csv(cf_lz, paste0(exportDir, 'RWA_CFSVA_2015_lz.csv'))