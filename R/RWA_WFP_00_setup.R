# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_00_setup.R: load packages and functions for analysis.
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


# Setup local directory ---------------------------------------------------
# -- Windows (within My Documents) --
baseDir = '~/Rwanda/rawdata/'

# -- Mac --
baseDir = '~/Documents/USAID/Rwanda/rawdata/'


# load appropriate packages -----------------------------------------------
library(data.table)
library(haven)
library(ggplot2)
library(tidyr)
library(lubridate)
library(llamar)
library(survey)
library(forcats)
library(modelr) # devtools::install_github('hadley/modelr')
library(RColorBrewer)
library(dplyr)
library(extrafont)
loadfonts()

# Set aesthetics ----------------------------------------------------------
font_light = 'Lato Light'
font_normal = 'Lato'

stunting_pal = c('#ffffe3', rev(brewer.pal(11, 'Spectral')[1:6]), '#68002d')
# linear range; max values for raster = 0.685; DHS @ admin2 = 0.574; DHS @ lz = 0.524; CFSVA @ admin2 = 0.678; CFSVA @ lz = 0.534
stunting_range = c(0, 0.70)





# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ARCHIVE: functions used from llamar -------------------------------------

# -- factorize --

# factorize = function(df, ref_df, var, new_var) {
#   # ref_df has labels associated with it.
#   # Note: can pipe with magrittr pipe, a la: df %>% factorize(ref_df, var, new_var)
#   
#   # -- check var is within both df and ref_df --
#   if(!var %in% colnames(df)) {
#     stop('variable is not in the current dataset (argument df)')
#   }
#   
#   if(!var %in% colnames(ref_df)) {
#     stop('variable is not in the reference dataset (argument ref_df)')
#   }
#   
#   # -- pull out the label values --
#   codebk = data.frame(code = attr(ref_df[[var]], 'labels'))
#   
#   # -- pull out the label names --
#   codebk = codebk %>% mutate(names =  row.names(codebk))
#   
#   # -- create a factor with the labels from the original dataset -- 
#   # levels will be sorted by the frequency of occurance (high to low)
#   df = df %>% 
#     mutate_(.dots = setNames(
#       list(paste0('forcats::fct_infreq(
#                   factor(', var, ',',
#                   'levels = ', list(codebk$code), ',',
#                   'labels = ', list(codebk$names),'))'
#                   )), new_var 
#       ))
#   
#   return(df)
# }



