**Run regressions from table 3 - the main age-adjusted child mortality regressions
global breps 1000
global q 1000
set seed 8301983

*read in analysis dataset
use "$data/vs_mcaid" if inrange(year,1950,1979), clear

drop if inlist(stfips,2,4,15)

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

*create treatment variables
char exp[omit] -1
xi i.exp, pref(_T)

*create treatment variables
char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X
gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

*fixed effects
egen rxy = group(region year)
egen ymxy = group(ymcaid year)
egen str = group(stfips white)
egen stxy = group(stfips year)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.stfips, pref(S_)
xi i.stfips*year, pref(SY_)
drop SY_stfips*
xi i.str i.white*i.rxy i.white*pcinc i.white*hpc i.white*bpc, pref(wh_)

xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*

*generate and summarize the (log) outcome variable
gen lnamrch 	= ln(amrch)
qui sum amrch if exp==-1 & ~white [aw=popch]
local mdv = r(mean)
 

/********************************/
/**Grouped Event-Study, Panel A**/
/********************************/

local X3 = "S_* RXY* pcinc hpc bpc _Texp_* [aw=popch]"
local DX3 = "S_* RXY* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28 [aw=popch]"
*Run model `i'
reg lnamrch `X3' if ~white, cluster(stfips)
outreg2 using "$output/figureA3_3_year_FE_timing.xls", replace keep(RXYyear*) noparen noaster ctitle("ES") 
*Run model `i'
reg lnamrch `DX3' if ~white, cluster(stfips)
outreg2 using "$output/figureA3_3_year_FE_timing.xls", append keep(RXYyear*) noparen noaster ctitle("DD") 



local X3 = "S_* RXY* YMXY* pcinc hpc bpc HC_Texp_* [aw=popch]"
local DX3 = "S_* RXY* YMXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch]"
*Run model `i'
reg lnamrch `X3' if ~white, cluster(stfips)
outreg2 using "$output/figureA3_3_year_FE_timing.xls", append keep(RXYyear*) noparen noaster ctitle("ESHC") 
*Run model `i'
reg lnamrch `DX3' if ~white, cluster(stfips)
outreg2 using "$output/figureA3_3_year_FE_timing.xls", append keep(RXYyear*) noparen noaster ctitle("DDHC") 


