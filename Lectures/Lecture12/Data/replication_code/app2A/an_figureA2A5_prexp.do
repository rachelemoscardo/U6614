cap log close
clear
set mem 2000m
set matsize 500

use "$data/mvp_mcaid", clear
drop if stfips==54|stfips==4

*cap drop crate0*
cap drop _Texp*
char exp[omit] -1
xi i.exp, pref(_T)
for var _Texp*: gen HCX = crate0*X

cap drop _DD* 
cap drop D
egen D = cut(exp), at(-100,-3,-1,0,1,4,7,100)
char D[omit] -1
xi i.D, pref(_D)
for var _DD*: gen HCX = crate0*X

gen POST = (exp>0 & exp<7)
gen HCPOST = crate0*POST

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)
xi i.stfips*year, pref(SY_)
drop SY_stfips*

local X1 = "hc _Texp* HC_Texp* [aw=ch_pop]"
local X2 = "YMXY* pcinc hpc bpc HC_Texp* [aw=ch_pop]"
local X3 = "YMXY* RXY* pcinc hpc bpc HC_Texp* [aw=ch_pop]"
local X4 = "YMXY* RXY* pcinc hpc bpc HC_Texp*"
local X5 = "SY_* YMXY* RXY* pcinc hpc bpc HC_Texp* [aw=ch_pop]"

local DX1 = "hc _Texp* HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 [aw=ch_pop]"
local DX2 = "YMXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 [aw=ch_pop]"
local DX3 = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 [aw=ch_pop]"
local DX4 = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12"
local DX5 = "SY_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 [aw=ch_pop]"

*set the per-recip exp to zero when there are no recips
gen miss_prmvp_ch = prmvp_ch
replace prmvp_ch = 0 if rmvp_ch==0

*conflicting information for IN ni the 1969 files.
replace prmvp_ch = . if year==1969 & stfips==18
*replace prmvp_ch = . if year==1966 & stfips==26

local r replace
*loop through models--defined within this file
local r replace
forval i = 3/3{		
	local wlstest = -99
	if `i'==4{
		*TEST EQUALITY OF WLS AND OLS FOR THE ES MODEL (DEATON PG 72)
		cap drop W* 
		gen W = 1/ch_pop
		
		*make interactions of all the fixed effects (there are no FE for the simple DD)
		cap for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp*: gen WX = W*X
		drop W
		quietly xi: reg rmvp_ch W* S_* `X`i'' , cluster(stfips)				
		testparm Wcrate0*
		local wlstest = r(p)
	}

	*RUN DD MODEL
	xi: areg prmvp_ch `DX`i'' , a(stfips) cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
	
	*RUN DD MODEL (with missing)
	xi: areg miss_prmvp_ch `DX`i'' , a(stfips) cluster(stfips)
	local ddm = _b[HCPOST]
	local ddmse = _se[HCPOST]
	
	*RUN ES MODEL
	xi: areg prmvp_ch `X`i'' , a(stfips) cluster(stfips)

	*test joint significance of pre/post coefs
	testparm HC_Texp_2 - HC_Texp_3
		local pre = r(p)
	testparm HC_Texp_6 - HC_Texp_11
		local post = r(p)
		
	*test DD assumption
	*1. pre-coefs are jointly zero
	testparm HC_Texp_2 - HC_Texp_3
	*2. post-coefs are equal
	test 	_b[HC_Texp_6] = _b[HC_Texp_7] =  _b[HC_Texp_8] = _b[HC_Texp_9] = _b[HC_Texp_10] = _b[HC_Texp_11], accumulate
	local ddtest = r(p)

	*test piecewise linear assumption
	*1. pre-coefs are jointly zero
	testparm HC_Texp_2 - HC_Texp_3
	*2. post-coefs are equal
	test 	_b[HC_Texp_6] = _b[HC_Texp_7] =  _b[HC_Texp_8] = _b[HC_Texp_9], accumulate
	test 	_b[HC_Texp_10] = _b[HC_Texp_11], accumulate
	local pltest = r(p)		
	
	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figureA2A5.xls", `r' keep(HC_Texp_2-HC_Texp_11) noparen noaster ctitle("X`i'") addstat(pre, `pre', post, `post', ddtest, `ddtest', pltest, `pltest', wlstest, `wlstest', dd, `dd', ddse, `ddse', ddm, `ddm', ddmse, `ddmse')
	local r append
}
