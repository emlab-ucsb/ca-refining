global dataDir "C:\Users\dhern125\Dropbox\UCSB-PhD\emLab\CALEPA\data_refining_paper"
global figuresDir "C:\Users\dhern125\Dropbox\UCSB-PhD\emLab\CALEPA\data_refining_paper\figures"

********************************************************************************
import delimited $dataDir/raw/refining-2023/refining_health_income_2023.csv, clear

	saveold $dataDir/processed/refining_health_income_2023, replace
	
	
	
use $dataDir/processed/pop_income_2020, clear
*2020: 14000US01001020100
	gen census_tract=substr(geoid,8,.)
	destring census_tract, replace
		drop year
	duplicates tag census_tract, gen(dup)
	sort geoid
	browse if dup==1
	
	saveold $dataDir/processed/pop_income_2020_mod, replace
	
use $dataDir/processed/refining_health_income_2023, clear
	merge m:1 census_tract using $dataDir/processed/pop_income_2020_mod
	/*
		*issue here
    Result                           # of obs.
    -----------------------------------------
    not matched                       269,189
        from master                   190,674  (_merge==1)
        from using                     78,515  (_merge==2)

    matched                         1,114,560  (_merge==3)
    -----------------------------------------
	*/
	drop if _merge==2
	
	
/*
	rename et1001 total_pop
	gen minority_pct=(et2002+et2003+et2004+et2005+et2006+et2007+et2008+et2009+et2010)/total_pop
	gen black_pct=et2002/total_pop
	gen hispanic_pct=(et2006+et2007+et2008+et2009+et2010)/total_pop
	sum minority_pct, det
*/	
	
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
collapse (sum) white_num white_den minority_num minority_den black_num black_den hispanic_num hispanic_den asian_num asian_den aialnative_num aialnative_den, by(scen_id demand_scenario refining_scenario year)

gen W=white_num/white_den
gen B=black_num/black_den
gen H=hispanic_num/hispanic_den
gen M=minority_num/minority_den
gen A=asian_num/asian_den
gen AIAL=aialnative_num/aialnative_den

gen stat_BW=B-W
gen stat_MW=M-W
gen stat_HW=H-W
gen stat_AW=A-W
gen stat_AIALW=AIAL-W

*FIGURE 1
preserve
keep if scen_id=="BAU historic exports"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("BAU historic exports")
		graph export $figuresDir/bau_his_exports.png, as(png)
restore

*FIGURE 2
preserve
keep if scen_id=="BAU historic production"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("BAU historic production")
		graph export $figuresDir/bau_his_prod.png, as(png)
restore

*FIGURE 3
preserve
keep if scen_id=="BAU low exports"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("BAU low exports")
		graph export $figuresDir/bau_low_exports.png, as(png)
restore

*FIGURE 4
preserve
keep if scen_id=="LC1 historic exports"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("LC1 historic exports")
		graph export $figuresDir/lc1_his_exports.png, as(png)
restore

*FIGURE 5
preserve
keep if scen_id=="LC1 historic production"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("LC1 historic production")
		graph export $figuresDir/lc1_his_prod.png, as(png)
restore

*FIGURE 6
preserve
keep if scen_id=="LC1 low exports"
	twoway (line B year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line H year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line W year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line A year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045))  (line AIAL year, lwidth(medthick) lcolor(green) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Black" 2 "Hispanic" 3 "white" 4 "Asian" 5 "American Indian" ) size(medsmall)) title("LC1 low exports")
		graph export $figuresDir/lc1_low_exports.png, as(png)
restore


***GAPS
*FIGURE 1
preserve
keep if scen_id=="BAU historic exports"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 5 "AIALw gap" ) size(medsmall)) title("BAU historic exports")
		graph export $figuresDir/bau_his_exports_gap.png, as(png)
restore

*FIGURE 2
preserve
keep if scen_id=="BAU historic production"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 5 "AIALw gap" ) size(medsmall)) title("BAU historic production")
		graph export $figuresDir/bau_his_prod_gap.png, as(png)
restore

*FIGURE 3
preserve
keep if scen_id=="BAU low exports"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 5 "AIALw gap" ) size(medsmall)) title("BAU low exports")
		graph export $figuresDir/bau_low_exports_gap.png, as(png)
restore

*FIGURE 4
preserve
keep if scen_id=="LC1 historic exports"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 5 "AIALw gap" ) size(medsmall)) title("LC1 historic exports")
		graph export $figuresDir/lc1_his_exports_gap.png, as(png)
restore

*FIGURE 5
preserve
keep if scen_id=="LC1 historic production"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 5 "AIALw gap" ) size(medsmall)) title("LC1 historic production")
		graph export $figuresDir/lc1_his_prod_gap.png, as(png)
restore

*FIGURE 6
preserve
keep if scen_id=="LC1 low exports"
	twoway (line stat_BW year, lwidth(medthick) lcolor(gs10) lpattern(dash) xlabel(2020(5)2045)) (line stat_HW year, lwidth(medthick) lcolor(sky) lpattern(dash) xlabel(2020(5)2045, labsize(medium)) ) (line stat_AW year, lwidth(medthick) lcolor(orange) lpattern(dash) xlabel(2020(5)2045))  (line stat_AIALW year, lwidth(medthick) lcolor(green) lpattern(dash) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Bw gap" 2 "Hw gap" 3 "Aw gap" 5 "AIALw gap" ) size(medsmall)) title("LC1 low exports")
		graph export $figuresDir/lc1_low_exports_gap.png, as(png)
restore



	
*******************************************************************************	
	***INCOME
use $dataDir/processed/pop_income_2021, clear	
*2021: 1400000US01001020100
	gen census_tract=substr(geoid,10,.)
	destring census_tract, replace
		drop year
	duplicates tag census_tract, gen(dup)
	sort geoid
	browse if dup==1
	
	saveold $dataDir/processed/pop_income_2021_mod, replace
	
use $dataDir/processed/refining_health_income_2023, clear
	merge m:1 census_tract using $dataDir/processed/pop_income_2021_mod
	
/*
*issue here

    -----------------------------------------
    not matched                       269,189
        from master                   190,674  (_merge==1)
        from using                     78,515  (_merge==2)

    matched                         1,114,560  (_merge==3)
    -----------------------------------------
*/	
	
	
	
egen deciles_inc=xtile(median_hh_income), nq(10) by(year)

forvalues d=1/10{
	gen dec_`d'=(deciles_inc==`d')
	gen deciles_num_`d'=total_pm25*dec_`d'*total_pop
	gen deciles_den_`d'=dec_`d'*total_pop
}

egen quantiles_inc=xtile(median_income), nq(4) by(year)

forvalues d=1/4{
	gen quan_`d'=(deciles_inc==`d')
	gen quantiles_num_`d'=total_pm25*quan_`d'*total_pop
	gen quantiles_den_`d'=quan_`d'*total_pop
}

collapse (sum) deciles_num_* deciles_den_* quantiles_den_* quantiles_num_*, by(scen_id demand_scenario refining_scenario year)

gen D1=deciles_num_1/deciles_den_1
gen D10=deciles_num_10/deciles_den_10
gen Q1=quantiles_num_1/quantiles_den_1
gen Q2=quantiles_num_2/quantiles_den_2
gen Q3=quantiles_num_3/quantiles_den_3
gen Q4=quantiles_num_4/quantiles_den_4


gen stat_deciles=D1-D10	
gen stat_quantiles=Q1-Q4

la var stat_deciles "Decile 1-Decile 10 Gap"
la var stat_deciles "Q 1-Q 10 Gap"


*FIGURE 1
preserve
keep if scen_id=="BAU historic exports"
	twoway (line Q1 year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line Q2 year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line Q3 year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line Q4 year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" ) size(medsmall)) title("BAU historic exports")
		graph export $figuresDir/bau_his_exports_inc.png, as(png) replace
restore

*FIGURE 2
preserve
keep if scen_id=="BAU historic production"
	twoway (line Q1 year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line Q2 year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line Q3 year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line Q4 year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" ) size(medsmall)) title("BAU historic production")
		graph export $figuresDir/bau_his_prod_inc.png, as(png) replace
restore

*FIGURE 3
preserve
keep if scen_id=="BAU low exports"
	twoway (line Q1 year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line Q2 year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line Q3 year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line Q4 year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" ) size(medsmall))  title("BAU low exports")
		graph export $figuresDir/bau_low_exports_inc.png, as(png) replace
restore

*FIGURE 4
preserve
keep if scen_id=="LC1 historic exports"
	twoway (line Q1 year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line Q2 year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line Q3 year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line Q4 year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" ) size(medsmall))  title("LC1 historic exports")
		graph export $figuresDir/lc1_his_exports_inc.png, as(png) replace
restore

*FIGURE 5
preserve
keep if scen_id=="LC1 historic production"
	twoway (line Q1 year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line Q2 year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line Q3 year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line Q4 year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" ) size(medsmall))  title("LC1 historic production")
		graph export $figuresDir/lc1_his_prod_inc.png, as(png) replace
restore

*FIGURE 6
preserve
keep if scen_id=="LC1 low exports"
	twoway (line Q1 year, lwidth(medthick) lcolor(gs10) lpattern(solid) xlabel(2020(5)2045)) (line Q2 year, lwidth(medthick) lcolor(sky) lpattern(solid) xlabel(2020(5)2045, labsize(medium)) ) (line Q3 year, lwidth(medthick) lcolor(black) lpattern(solid) xlabel(2020(5)2045)) (line Q4 year, lwidth(medthick) lcolor(orange) lpattern(solid) xlabel(2020(5)2045)), xtitle("")  ytitle("PM{sub:2.5} ({&mu}g/m{sup:3}/person)", size(medium)) legend(pos(6) col(5) order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" ) size(medsmall))  title("LC1 low exports")
		graph export $figuresDir/lc1_low_exports_inc.png, as(png) replace
restore



