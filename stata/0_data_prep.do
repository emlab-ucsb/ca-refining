
********************************************************************************
********************************************************************************
global startDir "/Users/paigeweber/Dropbox/Refining"
cd $startDir







********************************************************************************
********************************************************************************

cd data/

********************************************************************************
* refinery bbl processed
import delimited "reg_refin_crude_receipts.csv", clear 

gen ym = ym(year,month)
format ym %tm

destring bbls, replace force

keep ym bbls region

sort ym region
bysort ym region: egen bbl_r = sum(bbls)
bysort ym region: gen count = _n

replace bbl_r = bbl_r/10^6
label variable bbl_r "bbl millions"
drop bbls 
keep if count ==1
drop count
rename bbl_r bbl
reshape wide bbl, i(ym) j(region) string




reshape long bbl, i(ym) j(product) string

sort ym product

gen tho_bbl = bbl*10^3
label variable tho_bbl "thousands of barrels"

drop bbl

save "reg_refin_ym.dta", replace

isid ym product
********************************************************************************


clear
import delimited "Cushing_OK_WTI_Spot_Price_FOB.csv", clear 

rename cushing* WTI

split(month), parse("-")
drop month

rename month2 year
destring year, replace

gen year2 = 2000 + year

replace year2 = year2-100 if year2>2023
tab year2


drop year
rename year2 year

rename month1 month

gen month2 = month(date(month,"M"))
drop month
rename month2 month
gen ym = ym(year,month)
format ym %tm

save "WTI_monthly_1986_2023.dta", replace
********************************************************************************

import excel "Finished_Products_Movements.xlsx", sheet("Gasoline Chart Data") cellrange(A5:FB24) clear
** UNITS IN THIS SHEET ARE THOUSANDS OF BARRELS **
duplicates drop

rename A product

drop if product ==""


foreach v of varlist C-FB{
	
	rename `v' date_`v'
}

drop B

reshape long date, i(product) j(ym) string

rename date quantity

egen seq month = seq(), f(1) t(12)
egen seq year = seq(), f(2007) t(2019) b(12)

drop ym

gen date = mdy(month, 1, year)
format date %td

gen ym = ym(year, month)
format ym %tm

drop date

*reshape wide to construct net variables
drop month year
br

isid ym product

rename quantity tho_bbl

* clean up product names
replace product = trim(product)

replace product = "NC_FEx" if product =="NC Foreign Export"
replace product = "NC_FIm" if product =="NC Foreign Import"
replace product = "NC_NetIm" if product =="NC Net Imports"
replace product = "NC_InterEx" if product =="NC Interstate Export"
replace product = "NC_InterIm" if product =="NC Interstate Import"
replace product = "NC_Reno" if product =="NC Reno (North)"
replace product = "N_to_S" if product =="North to South"
replace product = "S_to_N" if product =="South to North"

replace product = "Gas_BlndStck" if product =="Product: Gasoline & Blendstocks"
replace product = "SC_CalEvn_W_Adj" if product =="SC CalNEv P/L (West) Adjusted"
replace product = "SC_FEx" if product =="SC Foreign Export"
replace product = "SC_FIm" if product =="SC Foreign Import"
replace product = "SC_InterEx" if product =="SC Interstate Export"
replace product = "SC_InterIm" if product =="SC Interstate Import"

replace product = "SC_W_Adj" if product =="SC Phoenix (West) Adjusted"
replace product = "Pipeline_KM" if product =="Source: Pipeline (KM Export Spreadsheet)"

replace product = "SC_W_Adj" if product =="SC Phoenix (West) Adjusted"
replace product = "Pipeline_SC" if product =="Total SC Pipeline"


replace product = "SC_NetIm" if product =="SC Net Imports"


isid ym product




merge m:1 ym using "WTI_monthly_1986_2023"

keep if _merge ==3

tab year

drop _merge

append using "reg_refin_ym"

drop year
gen date = dofm(ym)
format date %d
gen year = year(date)

keep if year >2006

sort ym product

order ym product

drop month

gen month = month(date)

bysort ym: carryforward WTI, replace

tab year

isid ym product

drop if tho_bbl ==.
save "temp.dta", replace

