/*-------------------------------------------------------------------------------
# Name:		05_ModConAnalysis
# Purpose:	Create model of modern contraceptive use
# Author:	Tim Essam, Ph.D.
# Created:	2016/08/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

capture log close
log using "$pathlog/05_ModConAnalysis", replace
clear

* Women are unit of analysis so we will be using the IR file.
	use "$pathwomen/RWIR70FL.DTA"

* Sampling weights and geography variables
	g wweight = v005 / 1000000	
	clonevar district = sdistrict
	clonevar altitude = v040
	clonevar strata = v022
	
* Clone original DHS variables and simply rename
	clonevar ageGroup = v013
	clonevar occupGroup = v717
	clonevar pregnant = v213
	clonevar educ	= v149
	clonevar fecund = v623
	

* Variables needing a bit of additional processing
* Need to determine categories for this
	clonevar moreChild = v605
	clonevar moreChildHus = v621
	g byte curUnion = (v502 == 1)
	g byte nevUnion = (v502 == 0)
	la var curUnion "Current in union/living w/ man"
	la var nevUnion "never in union"
	clonevar maritialStatus = v501

* Family planning knowledge or outreach
	clonevar visHCtoldFP = v395
	clonevar famPlanWorkVisit = v393

* FP via media
	g byte famPlanRadio = inlist(1, v384a)
	la var famPlanRadio "heard family planning on radio in last few months"
	g byte famPlanTV = inlist(1, v384b)
	la var famPlanTV "heard family planning on tv in last few months"
	g byte famPlanPrint = inlist(1, v384c)
	la var famPlanPrint "read family planning in print media in last few months"
	g byte famPlanExp = inlist(1, v384c, v384b, v384a)

* Recodes to standardize variables and groups
** Religion
	recode v130 (1 = 1 "Catholic")(2 = 2 "Protestant")(3 = 3 "Adventist")(4 = 4 "Mulsim") /*
	*/  (5 6 7 96 = 5 "Other"), gen(religion)

** Urban
	recode v025 (1 = 1 "Urban")(2 = 0 "Rural"), gen(urban)

** Residence status
	g byte residStatus = v504 == 1
	la var residStatus "currently residing with partner/husband"

** Fertility preference
	g byte moreKidsWanted = v602 == 1
	la var moreKidsWanted "would like to have another child"

/* Parity parity is defined as the number of times that she has given 
	birth to a fetus with a gestational age of 24 weeks or more, 
	regardless of whether the child was born alive or was stillborn. */
	clonevar totChild = v201
	recode totChild (0 = 0 "no children")(1 2 = 1 "1-2 children")(3 4 = 2 "3-4 children") /*
	*/	(5 / 14 = 3 "5+ children"), gen(parity)

* Household assets
	clonevar wealthGroup = v190
	clonevar wealth = v191
	replace wealth = (wealth / 100000)

* Contraception Use
	g byte modernContra = v313 == 3
	la var modernContra "Use modern method of contraception (binary)"
	clonevar intentionContra = v364
	recode v361 (1 2 3 = 0 "using or used")(4 = 1 "never used"), gen(contraPattern)

* Sexual activity
	clonevar sexActivity = v536
	g byte sexuallyActive = (sexActivity == 1)
	la var sexuallyActive "recent activity in last 4 weeks"
	

egen clust_mcUSe = mean(modernContra), by(strata)
egen alt_mcUSE = mean(modernContra), by(altitude)
twoway(lowess modernContra wealth)


	
