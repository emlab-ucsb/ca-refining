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
********************************************************************************
import delimited $concentrationsDir/refining_health_census_tract.csv, clear

	saveold $concentrationsDir/refining_health_census_tract, replace
	
	
	
use $dataDir/processed/pop_income_2020, clear
*2020: 14000US01001020100
	gen census_tract=substr(geoid,8,.)
	destring census_tract, replace
		drop year
	duplicates tag census_tract, gen(dup)
	sort geoid
	browse if dup==1
	
	saveold $processedDir/pop_income_2020_mod, replace
	
use $concentrationsDir/refining_health_census_tract, clear
	merge m:1 census_tract using $processedDir/pop_income_2020_mod
	/*

    Result                           # of obs.
    -----------------------------------------
    not matched                        76,286
        from master                         0  (_merge==1)
        from using                     76,286  (_merge==2)

    matched                         1,475,658  (_merge==3)
    -----------------------------------------

	*/
	drop if _merge==2	
	
	gen minority_pct=(hispanic+black+aialnative+asian)/total_pop
	gen black_pct=black/total_pop
	gen hispanic_pct=hispanic/total_pop
	gen asian_pct=asian/total_pop
	gen aialnative_pct=aialnative/total_pop

	rename total_pm25 totalpm25
	
	gen white_num=totalpm25*(1-minority_pct)*total_pop
	gen white_den=(1-minority_pct)*total_pop

local group "minority black hispanic asian aialnative"

foreach g in `group'{
	gen `g'_num=totalpm25*`g'_pct*total_pop
	gen `g'_den=`g'_pct*total_pop
}

*creating dac index
gen dac_population=total_pop if disadvantaged=="Yes"
	replace dac_population=0 if disadvantaged=="No"
gen dac_num=totalpm25*dac_population
	gen dac_den=dac_population
gen nodac_population=total_pop if disadvantaged=="No"
	replace nodac_population=0 if disadvantaged=="Yes"
gen nodac_num=totalpm25*nodac_population
	gen nodac_den=nodac_population
	

collapse (sum) white_num white_den minority_num minority_den black_num black_den hispanic_num hispanic_den asian_num asian_den aialnative_num aialnative_den dac_num dac_den nodac_num nodac_den, by(scen_id demand_scenario refining_scenario year)

gen W=white_num/white_den
gen B=black_num/black_den
gen H=hispanic_num/hispanic_den
gen M=minority_num/minority_den
gen A=asian_num/asian_den
gen AIAL=aialnative_num/aialnative_den

gen DAC=dac_num/dac_den
gen nDAC=nodac_num/nodac_den

gen stat_BW=B-W
gen stat_MW=M-W
gen stat_HW=H-W
gen stat_AW=A-W
gen stat_AIALW=AIAL-W
gen stat_DAC=DAC-nDAC


*LEVELS
*FIGURE 1
preserve
keep if scen_id=="BAU historic exports"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2025(10)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("BAU historic exports") saving($graphDir/bau_his_exports.gph, replace)
		graph export $figuresDir/bau_his_exports.png, as(png) replace
restore

*FIGURE 2
preserve
keep if scen_id=="BAU historic production"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(10)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("BAU historic production") saving($graphDir/bau_his_prod.gph, replace)
		graph export $figuresDir/bau_his_prod.png, as(png) replace
restore

*FIGURE 3
preserve
keep if scen_id=="BAU low exports"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(10)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("BAU low exports") saving($graphDir/bau_low_exports.gph, replace)
		graph export $figuresDir/bau_low_exports.png, as(png) replace
restore

*FIGURE 4
preserve
keep if scen_id=="LC1 historic exports"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("LC1 historic exports")
		graph export $figuresDir/lc1_his_exports.png, as(png) replace
restore

*FIGURE 5
preserve
keep if scen_id=="LC1 historic production"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("LC1 historic production")
		graph export $figuresDir/lc1_his_prod.png, as(png) replace
restore

*FIGURE 6
preserve
keep if scen_id=="LC1 low exports"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("LC1 low exports") saving($graphDir/lc1_low_exports.gph, replace)
		graph export $figuresDir/lc1_low_exports.png, as(png) replace
restore

*FINAL FIGURE WITH RELEVANT SCENARIOS
grc1leg $graphDir/bau_his_prod.gph $graphDir/bau_his_exports.gph $graphDir/lc1_low_exports.gph, col(2)
	graph export $figuresDir/all_race.png, as(png) replace




***GAPS
*FIGURE 1
preserve
keep if scen_id=="BAU historic exports"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(15)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 4 "AIALw gap" ) size(medsmall)) title("BAU historic exports") saving($graphDir/bau_his_exp_gap.gph, replace)
		graph export $figuresDir/bau_his_exports_gap.png, as(png) replace
restore

*FIGURE 2
preserve
keep if scen_id=="BAU historic production"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 4 "AIALw gap" ) size(medsmall)) title("BAU historic production") saving($graphDir/bau_his_prod_gap.gph, replace)
		graph export $figuresDir/bau_his_prod_gap.png, as(png) replace
restore

*FIGURE 3
preserve
keep if scen_id=="BAU low exports"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 4 "AIALw gap" ) size(medsmall)) title("BAU low exports") saving($graphDir/bau_low_exp_gap.gph, replace)
		graph export $figuresDir/bau_low_exports_gap.png, as(png) replace
restore

*grc1leg $graphDir/bau_his_exp_gap.gph $graphDir/bau_his_prod_gap.gph $graphDir/bau_low_exp_gap.gph, col(3)

*FIGURE 4
preserve
keep if scen_id=="LC1 historic exports"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 4 "AIALw gap" ) size(medsmall)) title("LC1 historic exports") saving($graphDir/lc1_his_exp_gap.gph, replace)
		graph export $figuresDir/lc1_his_exp_gap.png, as(png) replace
restore

*FIGURE 5
preserve
keep if scen_id=="LC1 historic production"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 4 "AIALw gap" ) size(medsmall)) title("LC1 historic production") saving($graphDir/lc1_his_prod_gap.gph, replace)
		graph export $figuresDir/lc1_his_prod_gap.png, as(png) replace
restore

*FIGURE 6
preserve
keep if scen_id=="LC1 low exports"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(10)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 4 "AIALw gap" ) size(medsmall)) title("LC1 low exports") saving($graphDir/lc1_low_exp_gap.gph, replace)
		graph export $figuresDir/lc1_low_exp_gap.png, as(png) replace
restore

*FINAL FIGURE WITH RELEVANT SCENARIOS

grc1leg $graphDir/bau_his_prod_gap.gph $graphDir/bau_his_exp_gap.gph $graphDir/lc1_low_exp_gap.gph, col(2)
	graph export $figuresDir/all_gap_race.png, as(png) replace






***GAPS DAC AND NON DAC
*FIGURE 1
preserve
keep if scen_id=="BAU historic exports"
	twoway (line stat_DAC year, lwidth(medthick) lcolor(sea) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap DAC and non-DAC" ) size(medsmall)) ylabel(0(.1).4) title("BAU historic exports") saving($graphDir/bau_his_exp_dac_gap.gph, replace)
		graph export $figuresDir/bau_his_exports_dac_gap.png, as(png) replace
restore

*FIGURE 2
preserve
keep if scen_id=="BAU historic production"
	twoway (line stat_DAC year, lwidth(medthick) lcolor(sea) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap DAC and non-DAC" ) size(medsmall)) ylabel(0(.1).4) title("BAU historic production") saving($graphDir/bau_his_prod_dac_gap.gph, replace)
		graph export $figuresDir/bau_his_prod_dac_gap.png, as(png) replace
restore

*FIGURE 3
preserve
keep if scen_id=="BAU low exports"
	twoway (line stat_DAC year, lwidth(medthick) lcolor(sea) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap DAC and non-DAC" ) size(medsmall)) ylabel(0(.1).4) title("BAU low exports") saving($graphDir/bau_low_exp_dac_gap.gph, replace)
		graph export $figuresDir/bau_low_exports_dac_gap.png, as(png) replace
restore

*FIGURE 4
preserve
keep if scen_id=="LC1 historic exports"
	twoway (line stat_DAC year, lwidth(medthick) lcolor(sea) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap DAC and non-DAC" ) size(medsmall)) ylabel(0(.1).4) title("LC1 historic exports") saving($graphDir/lc1_his_exp_dac_gap.gph, replace)
		graph export $figuresDir/lc1_his_exports_dac_gap.png, as(png) replace
restore

*FIGURE 5
preserve
keep if scen_id=="LC1 historic production"
	twoway (line stat_DAC year, lwidth(medthick) lcolor(sea) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap DAC and non-DAC") size(medsmall)) ylabel(0(.1).4) title("LC1 historic production") saving($graphDir/lc1_his_prod_dac_gap.gph, replace)
		graph export $figuresDir/lc1_his_prod_dac_gap.png, as(png) replace
restore

*FIGURE 6
preserve
keep if scen_id=="LC1 low exports"
	twoway (line stat_DAC year, lwidth(medthick) lcolor(sea) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("Gap in PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Gap DAC and non-DAC") size(medsmall))  ylabel(0(.1).4) title("LC1 low exports") saving($graphDir/lc1_low_exp_dac_gap.gph, replace)
		graph export $figuresDir/lc1_low_exports_dac_gap.png, as(png) replace
restore

*FINAL FIGURE WITH RELEVANT SCENARIOS

grc1leg $graphDir/bau_his_prod_dac_gap.gph $graphDir/bau_his_exp_dac_gap.gph $graphDir/lc1_low_exp_dac_gap.gph, col(2)

	graph export $figuresDir/all_gap_dac.png, as(png) replace

*/


*******************************

keep if year==2045

keep year scen_id stat_*

reshape long stat_, i(scen_id year) j(group, string)

rename stat_ gap

la var gap "Gap value in end point (2045)"
gen groupt=" Black-white gap" if group=="BW"
	replace groupt="Asian-white gap" if group=="AW"
	replace groupt="AIAN-white gap" if group=="AIALW"
	replace groupt="Hispanic-white gap" if group=="HW"
	replace groupt="Minority-white gap" if group=="MW"
	replace groupt="DAC-nonDAC gap" if group=="DAC"
	
append using $processedDir/gaps_2045_poor	
graph hbar (mean) gap, over(scen_id)  asyvars over(groupt)
sdf