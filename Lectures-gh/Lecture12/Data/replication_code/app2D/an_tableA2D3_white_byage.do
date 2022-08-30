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
for var dmr1 nmr pnmr imr asmr14 asmr59 asmr1014: gen lnX = ln(X)
local r replace
/********************************/
/**Grouped Event-Study, Panel A**/
/********************************/
local X1 = "crate0 _Texp* HC_DD* [aw=wt]"
local X2 = "S_* YMXY* pcinc hpc bpc HC_DD* [aw=wt]"
local X3 = "S_* YMXY* RXY* pcinc hpc bpc HC_DD* [aw=wt]"
local X4 = "S_* YMXY* RXY* pcinc hpc bpc HC_DD*"
local X5 = "S_* SY_* YMXY* RXY* pcinc hpc bpc HC_DD* [aw=wt]"
local X6 = "wh_* HC_DD* [aw=wt]"
local X7 = "S_* YMXY* RXY* pcinc hpc bpc (HC_DD* = ZHC_DD*) [aw=wt]"

cap drop wt
gen wt = births
foreach var of varlist lndmr1 lnnmr lnpnmr lnimr lnasmr14 lnasmr59 lnasmr1014{
	*set weights
	if substr("`var'",1,6)=="lnasmr"{
		local a = substr("`var'",7,.)
		replace wt = pop`a'
	}

	*mdv
	cap drop testo
	gen testo = exp(`var')
	qui sum testo if exp==-1 & white [aw=wt]
	local mdv = r(mean)

	forval i = 1(2)3{
		*Run model `i'
		reg `var' `X`i'' if white, cluster(stfips)
		*test DD assumption
		testparm HC_DD_2 HC_DD_3 HC_DD_4
		test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
		local ddtest = r(p)
		outreg2 using "$output/tableA2D3A_white_byage.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("`var'_X`i'") addstat(ddtest, `ddtest', mdv, `mdv')
		local r append
	}

}




/*************************************/
/**Single Coef Diff-in-Diff, Panel B**/
/*************************************/
gen RHCPOST = HCPOST

local X1 = "crate0 _Texp* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=wt]"
local X2 = "YMXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=wt]"
local X3 = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=wt]"
local X4 = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"
local X5 = "SY_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=wt]"
local X6 = "wh_* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=wt]"
local X7 = "S_* YMXY* RXY* pcinc hpc bpc (HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28  = ZHC_Texp_1 ZHC_Texp_18 ZHCPOST ZHC_Texp_28) [aw=wt]"


cap drop wt
gen wt = births
foreach var of varlist lndmr1 lnnmr lnpnmr lnimr lnasmr14 lnasmr59 lnasmr1014{
	*set weights
	if substr("`var'",1,6)=="lnasmr"{
		local a = substr("`var'",7,.)
		replace wt = pop`a'
	}

	*mdv
	cap drop testo
	gen testo = exp(`var')
	qui sum testo if exp==-1 & white [aw=wt]
	local mdv = r(mean)
		
	/**********/
	/****X1****/
	/**********/
	local i = 1
	{
		*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
		/*************/
		cap drop e XB0
		*run actual model and store 
		qui reg `var' `X`i'' if white, cluster(stfips)
		local t = _b[HCPOST]/_se[HCPOST]

		*residuals to resample
		replace HCPOST = 0
		qui reg `var' `X`i'' if white, cluster(stfips)
		predict e, resid
		*fitted values imposing H_0 for wild-t: HCPOST==0
		predict XB0, xb
		replace HCPOST = crate0*POST

		cap postclose wbs`var'`i'
		postfile wbs`var'`i' wild_t using "$datatemp/wbs`var'`i'.dta", replace

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
				qui areg ywild `X`i'' if white, a(stfips) cluster(stfips)
				local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
				qui post wbs`var'`i' (`wild_t')
			restore
		}
		postclose wbs`var'`i'
		
		*get p-value by calculating the percentile of the new bootstrap t-dist
		preserve
			use "$datatemp/wbs`var'`i'",clear
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
		reg `var' `X`i'' if white, cluster(stfips)
		outreg2 using "$output/tableA2D3B_white_byage.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_X`i'") addstat(bsp, `bsp', mdv, `mdv')
		local r append
		
		
		
	/**********/
	/****X3****/
	/**********/
	local i = 3
	{
		*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
		/*************/
		cap drop e XB0
		*run actual model and store 
		qui areg `var' `X`i'' if white, a(stfips) cluster(stfips)
		local t = _b[HCPOST]/_se[HCPOST]

		*residuals to resample
		replace HCPOST = 0
		qui areg `var' `X`i'' if white, a(stfips) cluster(stfips)
		predict e, resid
		*fitted values imposing H_0 for wild-t: HCPOST==0
		predict XB0, xb
		replace HCPOST = crate0*POST

		cap postclose wbs`var'`i'
		postfile wbs`var'`i' wild_t using "$datatemp/wbs`var'`i'.dta", replace

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
				qui areg ywild `X`i'' if white, a(stfips) cluster(stfips)
				local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
				qui post wbs`var'`i' (`wild_t')
			restore
		}
		postclose wbs`var'`i'
		
		*get p-value by calculating the percentile of the new bootstrap t-dist
		preserve
			use "$datatemp/wbs`var'`i'",clear
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
		areg `var' `X`i'' if white, a(stfips) cluster(stfips)
		outreg2 using "$output/tableA2D3B_white_byage.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_X`i'") addstat(bsp, `bsp', mdv, `mdv')
}


exit






