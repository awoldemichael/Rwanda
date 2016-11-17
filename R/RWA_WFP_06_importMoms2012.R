# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_06_importMoms2012.R: import household-level data from 2012 dataset
#
# Script to pull stunting data and associated household- or
# child-level data for Rwanda from the CFSVA dataset
# 
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 14 November 2016
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

females2012_raw = read_sav(paste0(baseDir, 'RW_2012_CFSVA/cfsvans-2012- mother-v01.sav'))


# 2009 data are unprocessed.  Also doesn't include Kigali / urban areas.
# ch2009 = read_sav(paste0(baseDir, 'RW_2009_CFSVA/Section 13 enfants.sav'))



# Checking how weights should be applied ----------------------------------
females2012 = females2012_raw %>% 
  select(
    # -- survey data --
    hh_id,
    moth_ID, 
    weight = FINAL_norm_weight,
    strata_ID,
    interview_date = Q001_2,
    mother_id = Qrow, # mother number from maternal section.  Guessing line number?
    
    # -- geo --
    p_code, #admin1
    d_code, #admin2
    s_code, #admin3
    v_code, # village
    t_code, # team (enumerator?)
    final_urban,
    # fews_w = fews_code, # some slight mismatch w/ fews codes; assuming child is right ?  Women's at least has lots more NAs for some reason.
    
    # -- demographics -- 
    mother_age = Q102_02,
    
    # -- education --
    Q102_04,
    
    # -- health --
    mother_mosquito_net = Q102_09,
    stunted_mother = W_STUNT,
    mother_BMI = W_BMI
    )

# Notes on data -----------------------------------------------------------
