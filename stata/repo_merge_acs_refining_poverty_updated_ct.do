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
	global concentrationsDir "$startDir/outputs/refining-2023/health"
	global figuresDir "$repoDir/stata/figures"
	global graphDir "$repoDir/stata/gph"
*******************************************************************************	
	***INCOME
use $processedDir/pop_poverty_2020, clear	
	gen census_tract=substr(geoid,10,.)
	destring census_tract, replace
		drop year
	duplicates tag census_tract, gen(dup)
	sort geoid
	browse if dup==1
	
	saveold $processedDir/pop_poverty_2021_mod, replace
	
use $concentrationsDir/refining_health_census_tract, clear
	merge m:1 census_tract using $processedDir/pop_poverty_2021_mod
	
/*
    Result                           # of obs.
    -----------------------------------------
    not matched                        76,286
        from master                         0  (_merge==1)
        from using                     76,286  (_merge==2)

    matched                         1,475,658  (_merge==3)
    -----------------------------------------

*/	

gen total_below_poverty_pct=total_below_poverty/total_pop
gen total_above_poverty_pct=total_above_poverty/total_pop
rename total_pm25 totalpm25
	
local group "total_above_poverty total_below_poverty"

foreach g in `group'{
	gen `g'_num=totalpm25*`g'_pct*total_pop
	gen `g'_den=`g'_pct*total_pop
}
collapse (sum) total_above_poverty_num total_above_poverty_den total_below_poverty_num total_below_poverty_den, by(scen_id demand_scenario refining_scenario year)

gen NP=total_above_poverty_num/total_above_poverty_den
gen P=total_below_poverty_num/total_below_poverty_den

gen stat_PNP=P-NP


*FIGURE 1
preserve
keep if scen_id=="BAU historic exports"
	twoway (line P year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line NP year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Below poverty" 2 "Above poverty" ) size(medsmall)) title("BAU historic exports")
		graph export $figuresDir/bau_his_exports_pov.png, as(png) replace
restore

*FIGURE 2
preserve
keep if scen_id=="BAU historic production"
	twoway (line P year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line NP year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Below poverty" 2 "Above poverty" ) size(medsmall)) title("BAU historic production")
		graph export $figuresDir/bau_his_prod_pov.png, as(png) replace
restore

*FIGURE 3
preserve
keep if scen_id=="BAU low exports"
	twoway (line P year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line NP year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Below poverty" 2 "Above poverty" ) size(medsmall)) title("BAU low exports")
		graph export $figuresDir/bau_low_exports_pov.png, as(png) replace
restore

*FIGURE 4
preserve
keep if scen_id=="LC1 historic exports"
	twoway (line P year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line NP year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Below poverty" 2 "Above poverty" ) size(medsmall))   title("LC1 historic exports")
		graph export $figuresDir/lc1_his_exports_pov.png, as(png) replace
restore

*FIGURE 5
preserve
keep if scen_id=="LC1 historic production"
	twoway (line P year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line NP year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Below poverty" 2 "Above poverty" ) size(medsmall))  title("LC1 historic production")
		graph export $figuresDir/lc1_his_prod_pov.png, as(png) replace
restore

*FIGURE 6
preserve
keep if scen_id=="LC1 low exports"
	twoway (line P year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line NP year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Below poverty" 2 "Above poverty" ) size(medsmall))  title("LC1 low exports")
		graph export $figuresDir/lc1_low_exports_pov.png, as(png) replace
restore



*GAP
*FIGURE 1
preserve
keep if scen_id=="BAU historic exports"
	twoway (line stat_PNP year, lwidth(medthick) lcolor(turquoise) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap Below poverty and Above poverty" ) size(medsmall)) ylabel(0(0.01)0.04) title("BAU historic exports") saving($graphDir/bau_his_exp_pov_gap.gph, replace)
		graph export $figuresDir/bau_his_exports_pov_gap.png, as(png) replace
restore

*FIGURE 2
preserve
keep if scen_id=="BAU historic production"
	twoway (line stat_PNP year, lwidth(medthick) lcolor(turquoise) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap Below poverty and Above poverty" ) size(medsmall)) ylabel(0(0.01)0.04) title("BAU historic production") saving($graphDir/bau_his_prod_pov_gap.gph, replace)
		graph export $figuresDir/bau_his_prod_pov_gap.png, as(png) replace
restore

*FIGURE 3
preserve
keep if scen_id=="BAU low exports"
	twoway (line stat_PNP year, lwidth(medthick) lcolor(turquoise) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap Below poverty and Above poverty" ) size(medsmall)) ylabel(0(0.01)0.04) title("BAU low exports") saving($graphDir/bau_low_exp_pov_gap.gph, replace)
		graph export $figuresDir/bau_low_exports_pov_gap.png, as(png) replace
restore

*FIGURE 4
preserve
keep if scen_id=="LC1 historic exports"
	twoway (line stat_PNP year, lwidth(medthick) lcolor(turquoise) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap Below poverty and Above poverty" ) size(medsmall)) ylabel(0(0.01)0.04) title("LC1 historic exports") saving($graphDir/lc1_his_exp_pov_gap.gph, replace)
		graph export $figuresDir/lc1_his_exports_pov_gap.png, as(png) replace
restore

*FIGURE 5
preserve
keep if scen_id=="LC1 historic production"
	twoway (line stat_PNP year, lwidth(medthick) lcolor(turquoise) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap Below poverty and Above poverty" ) size(medsmall)) ylabel(0(0.01)0.04) title("LC1 historic production") saving($graphDir/lc1_his_prod_pov_gap.gph, replace)
		graph export $figuresDir/lc1_his_prod_pov_gap.png, as(png) replace
restore

*FIGURE 6
preserve
keep if scen_id=="LC1 low exports"
	twoway (line stat_PNP year, lwidth(medthick) lcolor(turquoise) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap Below poverty and Above poverty" ) size(medsmall)) ylabel(0(0.01)0.04) title("LC1 low exports") saving($graphDir/lc1_low_exp_pov_gap.gph, replace)
		graph export $figuresDir/lc1_low_exports_pov_gap.png, as(png) replace
restore

*FINAL FIGURE WITH RELEVANT SCENARIOS

grc1leg $graphDir/bau_his_prod_pov_gap.gph $graphDir/bau_his_exp_pov_gap.gph $graphDir/lc1_low_exp_pov_gap.gph, col(2)

	graph export $figuresDir/all_gap_poverty.png, as(png) replace
	
************************************************************	
keep if year==2045

keep year scen_id stat_*

sdf
reshape long stat_, i(scen_id year) j(group, string)

rename stat_ gap



la var gap "Gap value in end point (2045)"
gen groupt=" Poor-no poor gap" if group=="PNP"

save $dataDir/processed/gaps_2045_poor, replace
graph hbar (mean) gap, over(scen_id)  asyvars over(groupt)	
