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
# • [stunted] stunting (percentage)
# • [stuntingZ] stunting (mean z-score)
# • [wealth_idx] wealth index (mean index score)

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
vars = c(colnames(hh %>% select(contains('growing'))), 'FCS',
         'own_livestock', 'own_cattle', 'manage_livestock')

for (var in vars) {
  cf_lz = calcAvgs(hh, var, by_var = 'livelihood_zone', cf_lz)
  
  cf_dist = calcAvgs(hh, var, by_var = 'admin2', cf_dist)
}



pcExp_dist = hh %>% 
  group_by(admin2) %>% 
  summarise(median_pc_expend = median(monthly_pc_expend), median_pc_expend_N = n())

pcExp_lz = hh %>% 
  group_by(livelihood_zone) %>% 
  summarise(median_pc_expend = median(monthly_pc_expend), median_pc_expend_N = n())

# using median per capita exp, since distribution seems to be skewed.
ggplot(hh, aes(x = monthly_pc_expend)) + geom_histogram(binwidth = 1000)
ggplot(hh, aes(x = log(monthly_pc_expend))) + geom_histogram(binwidth = 0.25)

# merge together all the CFSVA data ---------------------------------------
cf_dist = full_join(cf_dist, pcExp_dist, by = "admin2")

cf_lz = full_join(cf_lz, pcExp_lz, by = "livelihood_zone")

# save --------------------------------------------------------------------
write.csv(cf_dist, paste0(exportDir, 'RWA_CFSVA_2015_district.csv'))
write.csv(cf_lz, paste0(exportDir, 'RWA_CFSVA_2015_lz.csv'))