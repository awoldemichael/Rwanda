# Point estimates for DHS
# Data processed by Tim
df = read_dta('~/Documents/USAID/Rwanda/processeddata/DHS_2010_2015_analysis.dta')

df2 = removeAttributes(df) %>% filter(year==2014)


df2 = factorize(df2, df, 'lvdzone', 'lvdzone2')

# sans weights
st_dhs = calcPtEst(df2, 'stunted', by_var = 'lvdzone2', use_weights = FALSE)
write.csv(st_dhs, '~/Github/Rwanda/exported_data/DHS_stunted_unweighted.csv')


# mit weights
st_dhs = calcPtEst(df2, 'stunted', by_var = 'lvdzone2', use_weights = TRUE,
                   psu_var = 'psu', strata_var = 'strata', weight_var = 'cweight')
write.csv(st_dhs, '~/Github/Rwanda/exported_data/DHS_stunted_weighted.csv')
