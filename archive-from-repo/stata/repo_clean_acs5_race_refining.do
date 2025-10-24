/***************************   HEADER **********************************/
	clear all

	//set local directory
	if regexm("`c(pwd)'","")==1  { //Paige's machine
		global startDir ""
		global repoDir ""
	}
	else if regexm(c(os),"Windows")==1 { //Danae's machine
		global startDir "drives/emlab/projects/current-projects/calepa-cn"
		global repoDir "C:\Users\Danae\Documents\GitHub\ca-refining"
	}
	if "$startDir"=="" exit 
	disp "local path is $startDir"

	//set subfolders
	global rawDir "$startDir/data/Census/nhgis_2020"
	global processedDir "$startDir/data/Census"
	
	***************2020
	import delimited using $rawDir/nhgis0024_csv/nhgis0024_ds249_20205_tract, clear
	
	gen total_pop=amp3e001
	gen hispanic=amp3e012
	gen white=amp3e003
	gen black=amp3e004
	gen aialnative=amp3e005
	gen asian=amp3e006
	
	gen median_income=amr8e001
	
	keep gisjoin total_pop hispanic white black aialnative asian median_income geoid year state
	
	saveold $processedDir/pop_income_2020, replace
		keep if state=="California"
		export delimited $processedDir/pop_CA_geoid.csv, replace
	
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
	
	
	****************2017-2021 poverty 
	import delimited using $rawDir/nhgis0029_csv/nhgis0029_csv/nhgis0029_ds254_20215_tract.csv
	
	
	rename aoqge001 total_pop 
	rename aoqge002 total_below_poverty
	rename aoqge003 total_above_poverty

	rename geo_id geoid
	keep gisjoin geoid year total_above_poverty total_below_poverty total_pop
	
	saveold $processedDir/pop_poverty_2020, replace
	