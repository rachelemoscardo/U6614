**Run regressions from table 3 - the main age-adjusted child mortality regressions
global breps 1000
global q 1000
set seed 8301983

*read in analysis dataset
use "$data/vs_mcaid" if inrange(year,1950,1979), clear

drop if inlist(stfips,2,4,15)
drop if stfips==54 & white

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

*create treatment variables
char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X
for var _T*: gen ZHCX = crate1958*X
egen D 			= cut(exp), at(-17,-16,-11,-7,-1,0,1,5,10,1000)
char D[omit] -1
xi i.D, pref(_D)
for var _D*: gen HCX = crate0*X
for var _D*: gen ZHCX = crate1958*X
gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST
gen ZHCPOST		= crate1958*POST

*fixed effects
egen rxy = group(region year)
egen ymxy = group(ymcaid year)
egen str = group(stfips white)
egen stxy = group(stfips year)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)
xi i.stfips*year, pref(SY_)
drop SY_stfips*
xi i.str i.white*i.rxy i.white*i.ymxy i.white*pcinc i.white*hpc i.white*bpc, pref(wh_)

*generate and summarize the (log) outcome variable
gen lnasmr14 	= ln(asmr14)
qui sum asmr14 if exp==-1 & ~white [aw=pop14]
local mdv = r(mean)
 
 

/********************************/
/**Grouped Event-Study, Panel A**/
/********************************/
local X1 = "crate0 _Texp* HC_DD* [aw=pop14]"
local X2 = "S_* YMXY* pcinc hpc bpc HC_DD* [aw=pop14]"
local X3 = "S_* YMXY* RXY* pcinc hpc bpc HC_DD* [aw=pop14]"
local X4 = "S_* YMXY* RXY* pcinc hpc bpc HC_DD*"
local X5 = "S_* SY_* YMXY* RXY* pcinc hpc bpc HC_DD* [aw=pop14]"
local X6 = "wh_* HC_DD* [aw=pop14]"
local X7 = "S_* YMXY* RXY* pcinc hpc bpc (HC_DD_1 HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8 HC_DD_9 = ZHC_DD_1 ZHC_DD_2 ZHC_DD_3 ZHC_DD_4 ZHC_DD_6 ZHC_DD_7 ZHC_DD_8 ZHC_DD_9) [aw=pop14]"

local r replace
forval i = 1/5{
	*Test equality of wls and ols (Deaton pg 72)
	if `i'==4{
		save "$datatemp/temp", replace
		drop if inlist(stfips,23,33,50)
		cap drop W* 
		qui gen W = 1/pop14
		qui for var S_* YMXY* RXY* pcinc hpc bpc HC_DD*: gen WX = W*X
		drop W
		qui reg lnasmr14 `X`i'' W* if ~white, cluster(stfips)
		di "WLS Test"
		testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
		local wlstest = r(p)
	}
	if `i'~=4{
		local wlstest = -99
	}
	*Run model `i'
	reg lnasmr14 `X`i'' if ~white, cluster(stfips)
	*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
	outreg2 using "$output/tableA2D5A_asmr14_byspec.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("X`i'") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
	local r append
	if `i'==4{
		use "$datatemp/temp", clear
	}
}

**Pooled model with state-by-year FE
local wlstest = -99
for var HC_D*: replace X = X*(white==0)
cap egen hcnw = total(crate0*(white==0)), by(stfips year)
qui sum asmr14 if exp==-1 & hcnw & ~white [aw=pop14]
local nwmdv = r(mean)
qui sum asmr14 if exp==-1 & hcnw & white [aw=pop14]
local mdv = `nwmdv' - r(mean)

*Run model 6 
areg lnasmr14 `X6', a(stxy) cluster(str)
*test DD assumption
testparm HC_DD_2 HC_DD_3
test  _b[HC_DD_6] = _b[HC_DD_7], accumulate
local ddtest = r(p)
outreg2 using "$output/tableA2D5A_asmr14_byspec.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("X6") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')


*Run model 7 - Instrumenting for high/low group with 1958 high/low group
local wlstest = -99
ivregress 2sls lnasmr14 `X7' if ~white, vce(cluster stfips)
*test DD assumption
testparm HC_DD_2 HC_DD_3
test  _b[HC_DD_6] = _b[HC_DD_7], accumulate
local ddtest = r(p)
outreg2 using "$output/tableA2D5A_asmr14_byspec.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("X7") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')


/*************************************/
/**Single Coef Diff-in-Diff, Panel B**/
/*************************************/
gen RHCPOST = HCPOST
qui sum asmr14 if exp==-1 & hc & ~white [aw=pop14]
local mdv = r(mean)

local X1 = "crate0 _Texp* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=pop14]"
local X2 = "YMXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=pop14]"
local X3 = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=pop14]"
local X4 = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"
local X5 = "SY_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=pop14]"
local X6 = "wh_* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=pop14]"
local X7 = "S_* YMXY* RXY* pcinc hpc bpc (HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28  = ZHC_Texp_1 ZHC_Texp_18 ZHCPOST ZHC_Texp_28) [aw=pop14]"

/**********/
/****X1****/
/**********/
local r replace
local i = 1
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui reg lnasmr14 `X`i'' if ~white, cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui reg lnasmr14 `X`i'' if ~white, cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr14`i'
	postfile bsasmr14`i' wild_t using "$datatemp/bsasmr14`i'.dta", replace

	forval j = 1/$breps{
		preserve
			*Wild Bootstrap
			sort stfips year
			qui gen temp1 = runiform() if stfips!=stfips[_n-1]
			qui egen temp2 = max(temp1), by(stfips)
			qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
			*do impose H0 for the t bootstrap
			cap drop ywild
			qui gen ywild = XB0+ewild
			qui areg ywild `X`i'' if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr14`i' (`wild_t')
		restore
	}
	postclose bsasmr14`i'
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr14`i'",clear
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
	reg lnasmr14 `X`i'' if ~white, cluster(stfips)
	outreg2 using "$output/tableA2D5B_asmr14_byspec.xls", `r' keep(HCPOST) noparen noaster ctitle("X`i'") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append



/**********/
/****X2****/
/**********/
local i = 2
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr14`i'
	postfile bsasmr14`i' wild_t using "$datatemp/bsasmr14`i'.dta", replace

	forval j = 1/$breps{
		preserve
			*Wild Bootstrap
			sort stfips year
			qui gen temp1 = runiform() if stfips!=stfips[_n-1]
			qui egen temp2 = max(temp1), by(stfips)
			qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
			*do impose H0 for the t bootstrap
			cap drop ywild
			qui gen ywild = XB0+ewild
			qui areg ywild `X`i'' if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr14`i' (`wild_t')
		restore
	}
	postclose bsasmr14`i'
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr14`i'",clear
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
	areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	outreg2 using "$output/tableA2D5B_asmr14_byspec.xls", `r' keep(HCPOST) noparen noaster ctitle("X`i'") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')


/**********/
/****X3****/
/**********/
local i = 3
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr14`i'
	postfile bsasmr14`i' wild_t using "$datatemp/bsasmr14`i'.dta", replace

	forval j = 1/$breps{
		preserve
			*Wild Bootstrap
			sort stfips year
			qui gen temp1 = runiform() if stfips!=stfips[_n-1]
			qui egen temp2 = max(temp1), by(stfips)
			qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
			*do impose H0 for the t bootstrap
			cap drop ywild
			qui gen ywild = XB0+ewild
			qui areg ywild `X`i'' if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr14`i' (`wild_t')
		restore
	}
	postclose bsasmr14`i'
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr14`i'",clear
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
	areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	outreg2 using "$output/tableA2D5B_asmr14_byspec.xls", `r' keep(HCPOST) noparen noaster ctitle("X`i'") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')


/**********/
/****X4****/
/**********/
save "$datatemp/temp", replace
drop if inlist(stfips,23,33,50)
local i = 4
{
	cap drop W* 
	qui gen W = 1/pop14
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WX = W*X
	drop W
	qui areg lnasmr14 `X`i'' W* if ~white, a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr14`i'
	postfile bsasmr14`i' wild_t using "$datatemp/bsasmr14`i'.dta", replace

	forval j = 1/$breps{
		preserve
			*Wild Bootstrap
			sort stfips year
			qui gen temp1 = runiform() if stfips!=stfips[_n-1]
			qui egen temp2 = max(temp1), by(stfips)
			qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
			*do impose H0 for the t bootstrap
			cap drop ywild
			qui gen ywild = XB0+ewild
			qui areg ywild `X`i'' if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr14`i' (`wild_t')
		restore
	}
	postclose bsasmr14`i'
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr14`i'",clear
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
	areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	outreg2 using "$output/tableA2D5B_asmr14_byspec.xls", `r' keep(HCPOST) noparen noaster ctitle("X`i'") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

use "$datatemp/temp", clear

	
/**********/
/****X5****/
/**********/
local i = 5
local wlstest = -99
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr14`i'
	postfile bsasmr14`i' wild_t using "$datatemp/bsasmr14`i'.dta", replace

	forval j = 1/$breps{
		preserve
			*Wild Bootstrap
			sort stfips year
			qui gen temp1 = runiform() if stfips!=stfips[_n-1]
			qui egen temp2 = max(temp1), by(stfips)
			qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
			*do impose H0 for the t bootstrap
			cap drop ywild
			qui gen ywild = XB0+ewild
			qui areg ywild `X`i'' if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr14`i' (`wild_t')
		restore
	}
	postclose bsasmr14`i'
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr14`i'",clear
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
	areg lnasmr14 `X`i'' if ~white, a(stfips) cluster(stfips)
	outreg2 using "$output/tableA2D5B_asmr14_byspec.xls", `r' keep(HCPOST) noparen noaster ctitle("X`i'") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	
	
/**********/
/****X6****/
/**********/
local i = 6
**Pooled model with state-by-year FE
qui sum asmr14 if exp==-1 & hcnw & ~white [aw=pop14]
local nwmdv = r(mean)
qui sum asmr14 if exp==-1 & hcnw & white [aw=pop14]
local mdv = `nwmdv' - r(mean)
for var HC_Texp* HCPOST: replace X = X*(white==0)
local wlstest = -99
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr14 `X`i'' , a(stxy) cluster(str)
	local t = _b[HCPOST]/_se[HCPOST]
	
	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr14 `X`i'' , a(stxy) cluster(str)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST*(white==0)

	cap postclose bsasmr146
	postfile bsasmr146 wild_t using "$datatemp/bsasmr146.dta", replace

	forval j = 1/$breps{
		preserve
			*Wild Bootstrap
			sort stfips year
			qui gen temp1 = runiform() if stfips!=stfips[_n-1]
			qui egen temp2 = max(temp1), by(stfips)
			qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
			*do impose H0 for the t bootstrap
			cap drop ywild
			qui gen ywild = XB0+ewild
			qui areg ywild `X`i'', a(stxy) cluster(str)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr146 (`wild_t')
		restore
	}
	postclose bsasmr146
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr146",clear
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
*Run model 6
areg lnasmr14 `X`i'', a(stxy) cluster(str)
outreg2 using "$output/tableA2D5B_asmr14_byspec.xls", `r' keep(HCPOST) noparen noaster ctitle("X6") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')


/**********/
/****X7****/
/**********/
local i = 7
*Instrumenting for high/low group with 1958 high/low group
qui sum asmr14 if exp==-1 & hc & ~white [aw=pop14]
local mdv = r(mean)
local wlstest = -99

{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui ivregress 2sls lnasmr14 `X`i'' if ~white, vce(cluster stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui ivregress 2sls lnasmr14 `X`i'' if ~white, vce(cluster stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr147
	postfile bsasmr147 wild_t using "$datatemp/bsasmr147.dta", replace

	forval j = 1/$breps{
		preserve
			*Wild Bootstrap
			sort stfips year
			qui gen temp1 = runiform() if stfips!=stfips[_n-1]
			qui egen temp2 = max(temp1), by(stfips)
			qui gen ewild = -e*(temp2<.5) + e*(temp2>=.5)
			*do impose H0 for the t bootstrap
			cap drop ywild
			qui gen ywild = XB0+ewild
			qui ivregress 2sls ywild `X`i'' if ~white, vce(cluster stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr147 (`wild_t')
		restore
	}
	postclose bsasmr147
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr147",clear
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
*Run model 7
ivregress 2sls lnasmr14 `X`i'' if ~white, vce(cluster stfips)
outreg2 using "$output/tableA2D5B_asmr14_byspec.xls", `r' keep(HCPOST) noparen noaster ctitle("X7") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')


exit






