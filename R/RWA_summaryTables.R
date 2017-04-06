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
ggplot(hh, aes(x = log(monthly_pc_expend)))+ geom_histogram(binwidth = 0.25)





# calculate CFSVA @ district & LZ level ----------------------------------------
# -- hh-level --
pcExp_dist = hh %>% 
  group_by(admin2) %>% 
  summarise(median_pc_expend = median(monthly_pc_expend), median_pc_expend_N = n())

pcExp_lz = hh %>% 
  group_by(livelihood_zone) %>% 
  summarise(median_pc_expend = median(monthly_pc_expend), median_pc_expend_N = n())


fcs_dist = calcPtEst(hh, 'FCS', by_var = 'admin2', use_weights = FALSE)
colnames(fcs_dist) = c(colnames(fcs_dist)[1], lapply(colnames(fcs_dist)[-1], function (x) paste0('fcs_', x)))

fcs_lz = calcPtEst(hh, 'FCS', by_var = 'livelihood_zone', use_weights = FALSE)
colnames(fcs_lz) = c(colnames(fcs_lz)[1], lapply(colnames(fcs_lz)[-1], function (x) paste0('fcs_', x)))


growing_lz = hh %>% 
  select(contains('growing'), livelihood_zone, own_livestock, own_cattle, manage_livestock) %>% 
  group_by(livelihood_zone) %>% 
  summarise_each(funs(mean(., na.rm = TRUE), N = n(), sd = sd(., na.rm = TRUE)))


# using median per capita exp, since distribution seems to be skewed.
ggplot(hh, aes(x = monthly_pc_expend)) + geom_histogram(binwidth = 1000)
ggplot(hh, aes(x = log(monthly_pc_expend))) + geom_histogram(binwidth = 0.25)

# merge together all the CFSVA data ---------------------------------------
cf_dist = full_join(pcExp_dist, fcs_dist, by = "admin2")

cf_lz = full_join(pcExp_lz, fcs_lz, by = "livelihood_zone")

# save --------------------------------------------------------------------
write.csv(cf_dist, paste0(exportDir, 'RWA_CFSVA_2015_district.csv'))
write.csv(cf_lz, paste0(exportDir, 'RWA_CFSVA_2015_lz.csv'))