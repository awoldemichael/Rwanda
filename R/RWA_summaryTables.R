# RWA_WFP_calcAvgs
# Laura Hughes, lhughes@usaid.gov, 6 April 2017
# Data are from the 2012 or 2015 Rwanda Comprehensive Food and Security Vulnerability Analysis,
# or the 2010 or 2014/2015 Rwanda Demographic and Health Survey


# Script to pull or calculate all derived summary stats at a given geographic level.
# All values are calculated at two geographic levels:
# 1. At the district (Admin2) level, with sampling weights applied
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

# calculate CFSVA @ district level ----------------------------------------

# using median per capita exp
ggplot(hh, aes(x = monthly_pc_expend)) + geom_histogram(binwidth = 1000)
ggplot(hh, aes(x = log(monthly_pc_expend)))+ geom_histogram(binwidth = 0.25)

# merge together all the CFSVA data ---------------------------------------


# save --------------------------------------------------------------------
write.csv(cf_dist, paste0(exportDir, 'RWA_CFSVA_2015_district.csv'))


