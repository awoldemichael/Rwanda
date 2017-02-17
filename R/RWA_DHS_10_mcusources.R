# Import data on where married women access modern contraception.
# 
# Data are all from the 2014/2015 Rwanda DHS survey.
# Sources are pulled from the final report, extracted via Tabula.
# Percentage use of contraception pulled from StatCompiler; weighted by sampling weights.
# Laura Hughes, lhughes@usaid.gov


# libraries ---------------------------------------------------------------

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(data.table)


# import data -------------------------------------------------------------
mcu_type = read_excel('~/Documents/USAID/Rwanda/rawdata/RWA_DHS_MCU_type.xlsx')
mcu_source = read.csv('~/Documents/USAID/Rwanda/rawdata/RWA_DHS_MCU_source.csv', skip = 1)


# cleanup type -------------------------------------------------------
type = mcu_type %>% 
  select(year = `Survey Year`, value = `Characteristic Category`, type = Indicator) %>% 
  mutate(type_pct = value/100,
         type = str_replace_all(type, "Married women currently using ", "")) %>% 
  filter(!is.na(value))

type = type %>% filter(year == 2015)


# cleanup source ----------------------------------------------------------
# Source is a percentage of the women who are using that method.  So should sum to 100%, 
# but I actually want the product of the percentage using a type and the source.
public_src = c("Public sector", "Referral hospital",      
               "District hospital",       "Health center", "Health post",            
               "Outreach",                "Community health worker", "Other public")

priv_src = c("Private medical sector",  "Polyclinic",              "Clinic",                 
                "Dispensary",              "Pharmacy", "Family planning clinic", 
                "Other private")      

other_src = c("Other source", "Kiosk",                  
                "Church", "Friend/relative", "Other")

na_src = c("Don't know", "Missing")

# -- Classify groups --
src = mcu_source %>% 
  rename(source = Source) %>% 
  # tag as a group of sources or not
  mutate(isGroup = ifelse(source %in% c('Public sector', 'Private medical sector', 'Other source'), 1, 0))
         
         src  = src %>% 
         mutate(src_grp = case_when(src$source %in% public_src ~ 'public sector',
                             src$source %in% priv_src ~ 'private sector',
                             src$source %in% other_src ~ 'other',
                             src$source %in% na_src ~ 'unknown',
                             TRUE ~ 'gunk')) %>% 
  select(-Total) %>% 
  filter(src_grp != 'gunk')

# -- tidy --

src = src %>% 
  gather(key = type, value = val, -source, -src_grp, -isGroup)


other_priv = c('Clinic', 'Pharmacy', 'Polyclinic', 'Dispensary', 'Family planning clinic', 'Other private')
  # Public sector                  District hospital       Health center           Health post            
  #                              Community health worker             Private medical sector               
  #                                                                          
  #               Other source            Kiosk                   Church                                    )
other_pub = c('Other public', 'Referral hospital', 'Outreach')

other = c('Friend/relative', 'Other')


src = src %>% 
  mutate(src_pct = as.numeric(val)/100,
         type = case_when(src$type == 'IUD' ~ 'IUD',
                          src$type == 'sterilization' ~ 'female sterilization',
                          src$type == 'Injectables' ~ 'injections',
                          TRUE ~ str_to_lower(src$type)),
         grouped_src = case_when(src$source %in% other_priv ~ 'other private',
                                 src$source %in% other_pub ~ 'other public',
                                 src$source %in% other ~ 'other',
                                  src$source %in% na_src ~ 'unknown',
                                 TRUE ~ as.character(src$source))
         )

# -- lump categories --


# merge -------------------------------------------------------------------
mcu = full_join(src, type)

# Fix things that are knowledge rather than resource
mcu = mcu %>% 
  mutate(src_pct = case_when(mcu$type %like% 'lactational amenorrhea' ~ 1,
         mcu$type %like% 'standard days method' ~ 1,
         mcu$type %like% 'standard days method' ~ 1,
         TRUE ~ mcu$src_pct)) %>% 
  filter(isGroup == 0 | is.na(isGroup)) %>% 
  mutate(comb_pct = src_pct * type_pct) %>% 
  filter(comb_pct != 0)


write.csv(mcu, '~/Documents/USAID/Rwanda/processeddata/RWA_DHS_mcusource.csv')
