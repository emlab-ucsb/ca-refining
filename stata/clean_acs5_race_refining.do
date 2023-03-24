/***************************   HEADER **********************************/
	clear all

	//set local directory
	if regexm("`c(pwd)'","")==1  { //Paige's machine
		global startDir ""
		global repoDir ""
	}
	if regexm("`c(pwd)'","/Users/kylemeng")==1  { //Kyle's machine
		global startDir ""
		global repoDir ""
		//sysdir set PLUS $startDir/../../toolbox/STATA_toolbox/plus
		sysdir set PLUS $repoDir/scripts/toolbox/STATA/plus 
	}
	else if regexm(c(os),"Windows")==1 { //Danae's machine
		global startDir "C:\Users\dhern125\Dropbox\UCSB-PhD\emLab\CALEPA\data_refining_paper"
		global repoDir "C:\Users\Danae\Documents\GitHub\us_ej_disparities"
	}
	if "$startDir"=="" exit 
	disp "local path is $startDir"

	//set subfolders
	global rawDir "$startDir/raw"
	global processedDir "$startDir/processed"
	global tempDir "$startDir/temp"
	global tablesDir "$repoDir/tables"
	global figuresDir "$repoDir/figures"
	
	
	***************2020
	import delimited using $rawDir/nhgis0024_csv/nhgis0024_ds249_20205_tract, clear
	
	gen total_pop=amp3e001
	gen hispanic=amp3e012
	gen white=amp3e003
	gen black=amp3e004
	gen aialnative=amp3e005
	gen asian=amp3e006
	
	gen median_income=amr8e001
	
	keep gisjoin total_pop hispanic white black aialnative asian median_income geoid year
	
	saveold $processedDir/pop_income_2020, replace
	
	***************2021
	import delimited using $rawDir/nhgis0024_csv/nhgis0024_ds254_20215_tract, clear
	
	gen total_pop=aooce001
	gen hispanic=aooce012
	gen white=aooce003
	gen black=aooce004
	gen aialnative=aooce005
	gen asian=aooce006
	
	gen median_income=aoqie001
	rename geo_id geoid
	keep gisjoin total_pop hispanic white black aialnative asian median_income geoid year
	
	saveold $processedDir/pop_income_2021, replace	