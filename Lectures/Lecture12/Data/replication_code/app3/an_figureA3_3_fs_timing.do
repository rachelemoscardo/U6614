clear 
clear matrix
set matsize 5000

use "$data/mvp_mcaid", clear
drop if stfips==4|stfips==2|stfips==15
cap drop HC*
cap drop _Texp*
char exp[omit] -1
xi i.exp, pref(_T)

gen POST = (exp>0 & exp<7)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.stfips, pref(S_)

replace rmvp_ch = . if year==1969 & stfips==18

local X3 = "RXY* pcinc hpc bpc _Texp*"
local DX3 = "RXY* pcinc hpc bpc _Texp_1 _Texp_5 POST _Texp_12"

*loop through models--defined within this file
local r replace
forval i = 3/3{		
	cap drop W* 
	qui gen W = 1/ch_pop
	qui for var S_* RXY* pcinc hpc bpc _Texp_1 _Texp_5 POST _Texp_12: gen WX = W*X
	drop W
	qui reg rmvp_ch `DX`i'' S_* W*, cluster(stfips)
	di "WLS Test"
	testparm WPOST
	local wlstest = r(p)

	xi: areg rmvp_ch `DX`i'' W* , a(stfips) cluster(stfips)
	
	*RUN DD MODEL
	xi: areg rmvp_ch `DX`i'' [aw=ch_pop], a(stfips) cluster(stfips)
	local dd = _b[POST]
	local ddse = _se[POST]
	
	*RUN ES MODEL
	xi: areg rmvp_ch `X`i'' [aw=ch_pop], a(stfips) cluster(stfips)
	
	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figureA3_3_fs_timing.xls", `r' keep(_Texp_2-_Texp_11) noparen noaster ctitle("X`i'") addstat(dd, `dd', ddse, `ddse', wlstest, `wlstest')
	local r append
}
exit

local r replace
local i = 3
egen stg = group(stfips)
qui sum stg
local m = r(mean)
forval g = 1/`m'{
	qui sum stfips if stg==`g'
	local s = r(mean)
	xi: areg rmvp_ch `DX`i'' if stfips~=`s' [aw=ch_pop], a(stfips) cluster(stfips)
	outreg2 using "$output/figureA3_3_fs_timing.xls", `r' keep(POST) noparen noaster ctitle("No `s'") 
	local r append
}
exit

	
