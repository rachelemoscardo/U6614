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

gen POST  		= exp>0 & exp<10

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

*generate and summarize the (log) outcome variable
gen lnamrch 	= ln(amrch)
qui sum amrch if exp==-1 & ~white [aw=popch]
local mdv = r(mean)
 

/********************************/
/**Grouped Event-Study, Panel A**/
/********************************/
local X1 = "S_* RXYyear* _Texp_1 _Texp_18 POST _Texp_28 [aw=popch]"
local X2 = "S_* RXYyear* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28 [aw=popch]"
local X3 = "S_* RXY* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28 [aw=popch]"
local X4 = "S_* RXY* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28"
local X5 = "S_* SY_* RXY* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28 [aw=popch]"
local X6 = "wh_* _Texp_1 _Texp_18 POST _Texp_28 [aw=popch]"

local r replace
forval i = 1/5{
	*Test equality of wls and ols (Deaton pg 72)
	if `i'==4{
		save "$datatemp/temp", replace
		drop if inlist(stfips,23,33,50)
		cap drop W* 
		qui gen W = 1/popch
		qui for var S_* RXY* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28: gen WX = W*X
		drop W
		qui reg lnamrch `X`i'' W* if ~white, cluster(stfips)
		di "WLS Test"
		testparm WPOST
		local wlstest = r(p)
	}
	if `i'~=4{
		local wlstest = -99
	}
	*Run model `i'
	reg lnamrch `X`i'' if ~white, cluster(stfips)
	outreg2 using "$output/tableA3_1_amr_timing_byspec.xls", `r' keep(POST) noparen noaster ctitle("NW_X`i'") addstat(wlstest, `wlstest')
	local r append
	if `i'==4{
		use "$datatemp/temp", clear
	}
}

**Pooled model with state-by-year FE
local wlstest = -99
for var _Texp_1 _Texp_18 POST _Texp_28: replace X = X*(white==0)
*Run model 6 
areg lnamrch `X6', a(stxy) cluster(str)
outreg2 using "$output/tableA3_1_amr_timing_byspec.xls", `r' keep(POST) noparen noaster ctitle("X6") addstat(wlstest, `wlstest')


char exp[omit] -1
xi i.exp, pref(_T)

replace POST  		= exp>0 & exp<10

***WHITE***
forval i = 1/5{
	*Test equality of wls and ols (Deaton pg 72)
	if `i'==4{
		save "$datatemp/temp", replace
		cap drop W* 
		qui gen W = 1/popch
		qui for var S_* RXY* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28: gen WX = W*X
		drop W
		qui reg lnamrch `X`i'' W* if white, cluster(stfips)
		di "WLS Test"
		testparm WPOST
		local wlstest = r(p)
	}
	if `i'~=4{
		local wlstest = -99
	}
	*Run model `i'
	reg lnamrch `X`i'' if white, cluster(stfips)
	outreg2 using "$output/tableA3_1_amr_timing_byspec.xls", `r' keep(POST) noparen noaster ctitle("W_X`i'") addstat(wlstest, `wlstest')
	local r append
	if `i'==4{
		use "$datatemp/temp", clear
	}
}

