**Run regressions from figure VI - the main age-adjusted child amrch regressions
use "$data/vs_mcaid" if inrange(year,1950,1979), clear

drop if inlist(stfips,2,4,15)

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

char exp[omit] -1
xi i.exp, pref(_T)

gen POST  		= exp>0 & exp<10

gen lnamrch = ln(amrch)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.stfips, pref(S_)

local X = "RXY* S_* pcinc hpc bpc _Texp*"
local DX = "RXY* S_* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28"
local r replace
	
/****************************/
/**NONWHITE CHILD AMR, 0-14**/
/****************************/
cap drop W* 
qui gen W = 1/popch
qui for var S_* RXY* pcinc hpc bpc _Texp_1 _Texp_18 POST _Texp_28: gen WX = W*X
drop W
qui reg lnamrch `DX' W* if ~white, cluster(stfips)
di "WLS Test"
testparm WPOST
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnamrch `DX' [aw=popch] if ~white, cluster(stfips)
	local dd = _b[POST]
	local ddse = _se[POST]
*Run ES Model 
	xi: reg lnamrch `X' [aw=popch] if ~white, cluster(stfips)
outreg2 using "$output/figureA3_2_amrch_timing.xls", `r' keep(_Texp_2-_Texp_27) noparen noaster ctitle("Nonwhite") addstat(dd, `dd', ddse, `ddse', wlstest, `wlstest')
local r append

/*************************/
/**WHITE CHILD AMR, 0-14**/
/*************************/
qui reg lnamrch `DX' W* if white, cluster(stfips)
di "WLS Test"
testparm WPOST
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnamrch `DX' [aw=popch] if white, cluster(stfips)
	local dd = _b[POST]
	local ddse = _se[POST]
*Run ES Model 
	xi: reg lnamrch `X' [aw=popch] if white, cluster(stfips)
outreg2 using "$output/figureA3_2_amrch_timing.xls", `r' keep(_Texp_2-_Texp_27) noparen noaster ctitle("White") addstat(dd, `dd', ddse, `ddse', wlstest, `wlstest')
local r append

