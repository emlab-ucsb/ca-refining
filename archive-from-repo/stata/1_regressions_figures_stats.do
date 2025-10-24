




********************************************************************************
********************************************************************************
global startDir "/Users/paigeweber/Dropbox/Refining"
cd $startDir







********************************************************************************
********************************************************************************


*run code/0_data_prep.do // this creates temp.dta for analysis


use "data/temp.dta", clear

drop if tho_bbl ==.

isid ym product

rename tho_bbl bbl
 
reshape wide bbl, i(ym) j(product) string

rename bbl* *

********************************************************************************
********************************************************************************




gen NC_net = NC_FEx + NC_FIm
gen SC_net = SC_FEx + SC_FIm


replace NC_net = -1*NC_net
replace SC_net = -1*SC_net

su NC_net, detail
scalar NCmean = r(mean)

su SC_net, detail // mean is negative
scalar SCmean = r(mean)

* rescaling so both are above zero

scalar pct = 100*(SCmean - NCmean)/SCmean

di "percent larger exports in NC is:"
di pct 

gen pct = 100*(SC_net - NC_net)/SC_net

su pct, detail


bysort year: egen sum_pct = mean(pct)

gen pct_mean = .
gen pct_high = .
gen pct_low = .

su pct, detail
gen pct_all = r(mean)

foreach y of numlist 2007(1)2020{
	
	su pct if year ==`y', detail
	replace pct_mean = r(mean) if year ==`y'
	replace pct_high = r(mean) + r(sd) if year == `y'
	replace pct_low = r(mean) - r(sd) if year == `y'

	}

set scheme lean1

twoway (scatter pct_mean year, msymbol(o) mcolor(blue%60)) ///
		(rcap pct_high pct_low year) ///
		(line pct_all year, lpattern(dash)), ///
		 xtitle("") ytitle("Percent", size(medsmall)) ///
		xscale(r(2007 2020)) xlabel(2010(2)2020) ///
		legend(pos(6) col(3) label(1 "Annual mean") label( 2 "95 Pct. CI") label(3 "Average all years")) ///
		title("NC exports compared to SC exports", size(medsmall)) 

	

* sum to year since model does not include monthly


bysort year: egen yr_NC_net = sum(NC_net)
bysort year: egen yr_SC_net = sum(SC_net)

bysort year: egen yr_north = sum(north)
bysort year: egen yr_south = sum(south)

bysort year: egen yr_WTI = mean(WTI)

bysort year: gen count = _n
count if count ==1

gen northXWTI = north*WTI
gen southXWTI = south*WTI

gen WTIsq = WTI*WTI
gen northsq = north*north
gen southsq = south*south

gen yearsq = year*year


****************************************************


reghdfe NC_net north
estimates store m0

reghdfe NC_net north south WTI
estimates store m1

reghdfe NC_net north south WTI northXWTI southXWTI 
estimates store m2

reghdfe NC_net northsq southsq WTI WTIsq northXWTI southXWTI north south
estimates store m3

reghdfe NC_net northsq southsq WTI WTIsq northXWTI southXWTI north south year yearsq
estimates store m3b

reghdfe NC_net northsq southsq WTI WTIsq northXWTI southXWTI north south year yearsq N_to_S S_to_N
estimates store m3c



reghdfe SC_net south
estimates store m4a

reghdfe SC_net north south WTI
estimates store m4

reghdfe SC_net north south WTI northXWTI southXWTI 
estimates store m5

reghdfe SC_net north south northsq southsq WTI WTIsq northXWTI southXWTI
estimates store m6 

reghdfe SC_net north south northsq southsq WTI WTIsq northXWTI southXWTI year yearsq
estimates store m6b


reghdfe NC_net northsq southsq WTI WTIsq northXWTI southXWTI north south year yearsq N_to_S S_to_N
estimates store m6c


esttab m6b m6c m3b m3c, stats(N r2 stars) p






********************************************************************************
********************************************************************************

gen CA_net = NC_net + SC_net 
gen CA = north + south
gen CAXWTI = CA*WTI

gen CAsq = CA*CA

reghdfe CA_net CA WTI, absorb(i.year i.month)
estimates store m7

reghdfe CA_net CA WTI CAXWTI, absorb(i.year i.month)
estimates store m8

reghdfe CA_net CA CAsq WTI WTIsq CAXWTI, absorb(i.year i.month)
estimates store m9

esttab m7 m8 m9, stats(N r2 stars) p


reg CA_net WTI WTIsq i.year i.month
predict hat

gen resid = CA_net - hat

binscatter resid CA

binscatter CA CA_net

binscatter north NC_net

binscatter south SC_net


twoway (scatter CA_net CA) ///
		(lfit CA_net CA, lpattern(solid) lwidth(medthick) lcolor(midblue)), ///
		legend(off) ///
		xtitle("In state consumption (tho BBL)", size(small)) ytitle("Net exports (tho BBL)", size(small)) ///
		title("California", size(small)) xlabel(,labsize(small)) ylabel(,labsize(small))

graph save figures/CA_net.gph, replace

twoway (scatter NC_net north) ///
		(lfit NC_net north, lpattern(solid) lwidth(medthick) lcolor(emerald)), ///
		legend(off) ///
		xtitle("In state consumption (tho BBL)", size(small)) ytitle("Net exports (tho BBL)", size(small)) ///
		title("Northern CA", size(small)) xlabel(,labsize(small)) ylabel(,labsize(small)) yline(0, lpattern(dash))

graph save figures/NC_net.gph, replace

twoway (scatter SC_net north) ///
		(lfit SC_net north, lpattern(solid) lwidth(medthick) lcolor(purple)), ///
		legend(off) ///
		xtitle("In state consumption (tho BBL)", size(small)) ytitle("Net exports (tho BBL)", size(small)) ///
		title("Southern CA", size(small)) xlabel(,labsize(small)) ylabel(,labsize(small)) yline(0, lpattern(dash))

graph save figures/SC_net.gph, replace



graph combine figures/NC_net.gph figures/SC_net.gph, rows(1) ycommon 


********************************************************************************
********************************************************************************

* norht to south export share

gen NoverS = NC_net/SC_net
su NoverS, detail

* monthly 
twoway (line NC_net ym, lcolor(midblue)) ///
		(line SC_net ym, lcolor(gs8)), ///
		legend(pos(6) col(2) label(1 "North CA") label(2 "South CA")) ///
		yline(0, lcolor(black)) xtitle("") title("Net exports", size(medsmall)) ///
		ytitle("BBL (tho)")
		
* annual average


twoway (line yr_NC_net year, lcolor(midblue)) ///
		(line yr_north year, lcolor(gs8) yaxis(2) ytitle(BBL (tho))), ///
		legend(pos(6) col(2) label(1 "North CA net exports") label(2 "North comsumption")) ///
		xtitle("") title("North: Net exports and consumption", size(medsmall)) ///
		ytitle("BBL (tho)")

save figures/north_net_consumption.gph, replace

	
twoway (line yr_SC_net year, lcolor(green%60)) ///
		(line yr_south year, lcolor(gs8) yaxis(2) ytitle(BBL (tho))), ///
		legend(pos(6) col(2) label(1 "South CA net exports") label(2 "South comsumption")) ///
		xtitle("") title("South: Net exports and consumption", size(medsmall)) ///
		ytitle("BBL (tho)")

save figures/south_net_consumption.gph, replace

cd figures/
graph combine south_net_consumption.gph  north_net_consumption.gph



twoway (line yr_NC_net year, lcolor(midblue)) ///
		(line yr_SC_net year, lcolor(gs8) ), ///
		legend(pos(6) col(2) label(1 "North CA") label(2 "South CA")) ///
		yline(0, lcolor(black)) xtitle("") title("Net exports", size(medsmall)) ///
		ytitle("BBL (tho)")
		
twoway (line yr_SC_net year, lcolor(green%50)) ///
		(line yr_south year, lcolor(gs8) yaxis(2)), ///
		legend(pos(6) col(2) label(1 "North CA") label(2 "South CA")) ///
		yline(0, lcolor(black)) xtitle("") title("Net exports", size(medsmall)) ///
		ytitle("BBL (tho)")



twoway (line NC_net ym, lcolor(midblue) lpattern(dash)) ///
		(line NC_hat_3 ym, lcolor(gs6) lpattern(solid)), ///
		legend(pos(6) col(2) label(1 "Actuals") label(2 "Predicted")) ///
		yline(0, lcolor(black)) xtitle("") title("North, net exports", size(medsmall)) ///
		ytitle("BBL (tho)")
		
twoway (line SC_net ym, lcolor(green%60) lpattern(dash)) ///
		(line SC_hat_6 ym, lcolor(gs6) lpattern(solid)), ///
		legend(pos(6) col(2) label(1 "Actuals") label(2 "Predicted")) ///
		yline(0, lcolor(black)) xtitle("") title("South, net exports", size(medsmall)) ///
		ytitle("BBL (tho)")





