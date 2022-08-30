global breps 1000
global q 1000
set seed 8301983

use "$data/mvp_mcaid", clear
*drop WV (see table notes)
drop if stfips==54 | stfips==4 

cap drop HC*
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


*fixed effects
egen rxy = group(region year)
egen ymxy = group(ymcaid year)
egen stxy = group(stfips year)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)
*xi i.stfips*year, pref(SY_)
*drop SY_stfips*

*replace rmvp_ch = . if year==1969 & stfips==18
drop if year==1969 & stfips==18

*define DD models
local X1 = "crate0 _Texp* HC_DD* [aw=ch_pop]"
local X2 = "S_* YMXY* pcinc hpc bpc HC_DD* [aw=ch_pop]"
local X3 = "S_* YMXY* RXY* pcinc hpc bpc HC_DD* [aw=ch_pop]"
local X4 = "S_* YMXY* RXY* pcinc hpc bpc HC_DD*"
local X5 = "S_* SY_* YMXY* RXY* pcinc hpc bpc HC_DD* [aw=ch_pop]"
local X7 = "S_* YMXY* RXY* pcinc hpc bpc (HC_DD* = ZHC_DD*) [aw=ch_pop]"

local DX1 = "crate0 _Texp* HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 [aw=ch_pop]"
local DX2 = "S_* YMXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 [aw=ch_pop]"
local DX3 = "S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 [aw=ch_pop]"
local DX4 = "S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12"
local DX5 = "S_* SY_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 [aw=ch_pop]"
local DX7 = "S_* YMXY* RXY* pcinc hpc bpc (HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 = ZHC_Texp_1 ZHC_Texp_5 ZHCPOST ZHC_Texp_12) [aw=ch_pop]"

sum rmvp_ch if exp>0 [aw=ch_pop]
local mdv = r(mean)

local r replace
foreach i in 3{
	*Test equality of wls and ols (Deaton pg 72)
	if `i'==4{
		cap drop W* 
		qui gen W = 1/ch_pop
		qui for var S_* YMXY* RXY* pcinc hpc bpc HC_DD*: gen WX = W*X
		drop W
		qui reg rmvp_ch `X`i'' W*, cluster(stfips)
		di "WLS Test"
		testparm WHC_DD_2 WHC_DD_5 WHC_DD_6
		local wlstest = r(p)
	}
	if `i'~=4{
		local wlstest = -99
	}

	*Run model `i'
	reg rmvp_ch `X`i'' , cluster(stfips)
	*test DD assumption
	testparm HC_DD_2
	test  _b[HC_DD_5] = _b[HC_DD_6], accumulate
	local ddtest = r(p)
	outreg2 using "$output/table2A.xls", `r' keep(HC_DD_2 HC_DD_5 HC_DD_6 HC_DD_7) noparen noaster ctitle("X`i'") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
	local r append
}

local i = 3
foreach type in hosp md pdrug dent{
sum rmvp_ch_`type' if exp>0 [aw=ch_pop]
local mdv = r(mean)
	*Test equality of wls and ols (Deaton pg 72)
	if `i'==4{
		cap drop W* 
		qui gen W = 1/ch_pop
		qui for var S_* YMXY* RXY* pcinc hpc bpc HC_DD*: gen WX = W*X
		drop W
		qui reg rmvp_ch_`type' `X`i'' W*, cluster(stfips)
		di "WLS Test"
		testparm WHC_DD_2 WHC_DD_5 WHC_DD_6
		local wlstest = r(p)
	}
	if `i'~=4{
		local wlstest = -99
	}
	*Run model `i'
	reg rmvp_ch_`type' `X`i'' , cluster(stfips)
	*test DD assumption
	testparm HC_DD_2
	test  _b[HC_DD_5] = _b[HC_DD_6], accumulate
	local ddtest = r(p)
	outreg2 using "$output/table2A.xls", `r' keep(HC_DD_2 HC_DD_5 HC_DD_6 HC_DD_7) noparen noaster ctitle("`type'") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
	local r append
}






***********************************
**************DD COEFS*************
***********************************
sum rmvp_ch if exp>0 [aw=ch_pop]
local mdv = r(mean)
local r replace
foreach i in 3{
	*Test equality of wls and ols (Deaton pg 72)
	if `i'==4{
		cap drop W* 
		qui gen W = 1/ch_pop
		qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 : gen WX = W*X
		drop W
		qui reg rmvp_ch `DX`i'' W*, cluster(stfips)
		di "WLS Test"
		testparm WHCPOST
		local wlstest = r(p)
	}
	if `i'~=4{
		local wlstest = -99
	}
	{
		*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
		/*************/
		cap drop e XB0
		*run actual model and store 
		qui reg rmvp_ch `DX`i'' , cluster(stfips)
		local t = _b[HCPOST]/_se[HCPOST]

		*residuals to resample
		replace HCPOST = 0
		qui reg rmvp_ch `DX`i'' , cluster(stfips)
		predict e, resid
		*fitted values imposing H_0 for wild-t: HCPOST==0
		predict XB0, xb
		replace HCPOST = crate0*POST

		cap postclose bsmvp`i'
		postfile bsmvp`i' wild_t using "$datatemp/bsmvp`i'.dta", replace

		forval j = 1/$breps{
			preserve
				*Wild Bootstrap
				qui gen temp1 = runiform() if stfips!=stfips[_n-1]
				qui egen temp2 = max(temp1), by(stfips)
				qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
				*do impose H0 for the t bootstrap
				cap drop ywild
				qui gen ywild = XB0+ewild
				qui areg ywild `DX`i'' , a(stfips) cluster(stfips)
				local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
				qui post bsmvp`i' (`wild_t')
			restore
		}
		postclose bsmvp`i'
		
		*get p-value by calculating the percentile of the new bootstrap t-dist
		preserve
			use "$datatemp/bsmvp`i'",clear
			local obs = _N+2
			set obs `obs'
			local n1 = `obs' - 1
			local n2 = `obs'
			replace wild_t = abs(`t') in `n1'
			gen realt = 1 in `n1'
			replace wild_t = -abs(`t') in `n2'
			replace realt = 2 in `n2'
			xtile xwild_t = wild_t, n($q)
			*the positive one
			sum xwild_t if _n==`n1'
			local bsp1 = (1-r(mean)/$q)
			*the negative one
			sum xwild_t if _n==`n2'
			local bsp2 = r(mean)/$q
			local bsp = `bsp1'+`bsp2'
		restore
		/*************/
	}			
	*Run model `i'
	reg rmvp_ch `DX`i'' , cluster(stfips)
	outreg2 using "$output/table2B.xls", `r' keep(HCPOST) noparen noaster ctitle("X`i'") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append
}


local i = 3
foreach type in hosp md pdrug dent{
sum rmvp_ch_`type' if exp>0 [aw=ch_pop]
local mdv = r(mean)
	*Test equality of wls and ols (Deaton pg 72)
	if `i'==4{
		cap drop W* 
		qui gen W = 1/ch_pop
		qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12 : gen WX = W*X
		drop W
		qui reg rmvp_ch_`type' `DX`i'' W*, cluster(stfips)
		di "WLS Test"
		testparm WHCPOST
		local wlstest = r(p)
	}
	if `i'~=4{
		local wlstest = -99
	}
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui reg rmvp_ch_`type' `DX`i'' , cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui reg rmvp_ch_`type' `DX`i'' , cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bs`type'`i'
	postfile bs`type'`i' wild_t using "$datatemp/bs`type'`i'.dta", replace

	forval j = 1/$breps{
		preserve
			*Wild Bootstrap
			qui gen temp1 = runiform() if stfips!=stfips[_n-1]
			qui egen temp2 = max(temp1), by(stfips)
			qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
			*do impose H0 for the t bootstrap
			cap drop ywild
			qui gen ywild = XB0+ewild
			qui areg ywild `DX`i'' , a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bs`type'`i' (`wild_t')
		restore
	}
	postclose bs`type'`i'
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bs`type'`i'",clear
		local obs = _N+2
		set obs `obs'
		local n1 = `obs' - 1
		local n2 = `obs'
		replace wild_t = abs(`t') in `n1'
		gen realt = 1 in `n1'
		replace wild_t = -abs(`t') in `n2'
		replace realt = 2 in `n2'
		xtile xwild_t = wild_t, n($q)
		*the positive one
		sum xwild_t if _n==`n1'
		local bsp1 = (1-r(mean)/$q)
		*the negative one
		sum xwild_t if _n==`n2'
		local bsp2 = r(mean)/$q
		local bsp = `bsp1'+`bsp2'
	restore
	/*************/
}		
	*Run model `i'
	reg rmvp_ch_`type' `DX`i'' , cluster(stfips)
	outreg2 using "$output/table2B.xls", `r' keep(HCPOST) noparen noaster ctitle("`type'") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append
}




exit
