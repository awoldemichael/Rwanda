/*-------------------------------------------------------------------------------
# Name:		00_SetupFolderGlobals
# Purpose:	Create series of folders Food for Ethiopia Vulnerability Analysis
# Author:	Tim Essam, Ph.D.
# Created:	2016/08/04
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

/* RUN/READ ME FIRST -- Make directories for the study.
 1. Requires root name for file folder structure
 2. Requires branch names for sub-folders
 3. Sets global macros for the study; These are used through the do files.
 4. TODO -- Script DOES NOT copy in raw data at this point.
 5. TODO -- Create program folder for calling custom programs.
*/
set more off
	
* install the confirm directory ado if not already installed
* list all known user-written .ados needed for project
local required_ados confirmdir mvfiles fs spatgsa  adolist labellist winsor2   
foreach x of local required_ados { 
	capture findfile `x'.ado
		if _rc==601 {
			cap ssc install `x'
		}
		else disp in yellow "`x' currently installed."
	}
*end

* Determine path for the study 
*global projectpath "U:/"
*global projectpath "C:/Users/t/Documents/"
global projectpath "C:/Users/Tim/Documents/Github/RwandaLAM"
cd "$projectpath"


/*---------------------------------
# Set Globals based on path above #
-----------------------------------*/
global date $S_DATE
local dir `c(pwd)'
global path "`dir'"
global pathdo "`dir'/Stata"
global pathdo2 "C:/Users/Tim/Documents/GitHub/RwandaLAM/Stata"
global pathlog  "`dir'/Stata/Log"
global pathin "`dir'/Datain"
global pathout "`dir'/Dataout"

* DHS specific paths
global pathkids "`dir'/Datain/RW_2014-15_DHS/rwkr70dt"
global pathwomen "`dir'/Datain/RW_2014-15_DHS/rwir70dt"
global pathmen "`dir'/Datain/RW_2014-15_DHS/rwmr70dt"
global pathroster "`dir'/Datain/RW_2014-15_DHS/rwpr70dt"
global pathhh "`dir'/Datain/RW_2014-15_DHS/rwhr70dt"

* Other paths
global pathgraph "`dir'/Plots/raw"
global pathxls "`dir'/Excel"
global pathreg "`dir'/Output"
global pathgis "`dir'/GIS"
global pathraw "`dir'/Rawdata"
global pathexport "`dir'/Export"
global pathR "`dir'/R"
*global pathPython "`dir'/Python"
global pathProgram "`dir'/Stata/Programs"
global pathSensitive "`dir'/Sensitive_Data"

* Project macros are defined as:
macro list 


* Clone and rename merge variables in each module to ensure mergability!!!




