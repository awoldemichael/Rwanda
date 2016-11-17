# Monthly pc expend comparison.
# Similar densities; shift to higher values for 2015.  May just be b/c of inflation.
ggplot(ch_hh2012, aes(x = log10(pc_exp_year/12))) +
  geom_density(fill = 'blue', alpha = 0.3) + 
  xlim(c(0,6)) + 
  geom_density(aes(x = log10(monthly_pc_expend)), data = ch_hh, fill = 'red', alpha  = 0.3 ) +
  theme_xygrid()


# Compare pc_exp with stunting relationship:
ggplot(ch_hh, aes(x = scale(log_pcexp), y = stuntingZ)) + 
  geom_smooth(colour = 'red') +
  geom_smooth(colour = 'blue', data = ch_hh2012) +
  geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid()
