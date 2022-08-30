**Run regressions from table 5 - health at birth outcomes
global breps 1000
global q 1000
set seed 8301983

use "$data/vs_mcaid" if inrange(year,1950,1979) & ~white, clear

drop if inlist(stfips,2,4,15)
drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

*create treatment variables
char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X

egen D 			= cut(exp), at(-17,-16,-11,-7,-1,0,1,5,10,1000)
char D[omit] -1
xi i.D, pref(_D)
for var _D*: gen HCX = crate0*X

gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

gen lnvlbwr		= ln(vlbwr) if ~inlist(stfips,9,25)
gen lnlbwr 		= ln(lbwr) if ~inlist(stfips,9,25)
gen dmmr		= mmr>0 if mmr<.

*fixed effects
xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)


/********************************/
/**Grouped Event-Study, Panel A**/
/********************************/
local X = "S_* YMXY* RXY* pcinc hpc bpc HC_DD*"
cap drop W* 
qui gen W = 1/births
qui for var S_* YMXY* RXY* pcinc hpc bpc HC_DD*: gen WX = W*X
drop W

local r replace
****************
*VERY LOW BIRTH WEIGHT*
****************
qui sum vlbwr if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg lnvlbwr `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg lnvlbwr `X' [aw=births], cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table5A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lnvlbwr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
local r append

******************
*LOW BIRTH WEIGHT*
******************
qui sum lbwr if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg lnlbwr `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg lnlbwr `X' [aw=births], cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table5A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lbwr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
local r append

*******************
*SEX RATIO AT BIRTH*
*******************
qui sum sr if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg sr `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg sr `X' [aw=births], cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table5A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("sr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')

*******************
*MATERNAL MORTALITY
*******************
qui sum mmr if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg mmr `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg mmr `X' [aw=births] , cluster(stfips)
*test DD assumption
	testparm HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table5A.xls", `r' keep(HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("mmr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')



/*************************************/
/**Single Coef Diff-in-Diff, Panel B**/
/*************************************/
local X = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"

cap drop W* 
qui gen W = 1/births
qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WX = W*X
drop W

local r replace

****************
*VERY LOW BIRTH WEIGHT
****************
qui sum vlbwr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnvlbwr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
	if `wlstest'==.{
		local wlstest = -99
	}
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnvlbwr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnvlbwr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsvlbwr
	postfile bsvlbwr wild_t using "$datatemp/bsvlbwr.dta", replace

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
			qui areg ywild `X' if ~white [aw=births], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsvlbwr (`wild_t')
		restore
	}
	postclose bsvlbwr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsvlbwr.dta", clear
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
*Run Model
	areg lnvlbwr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table5B.xls", `r' keep(HCPOST) noparen noaster ctitle("vlbwr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append

	
	
**************
*LOW BIRTH WEIGHT*
**************
qui sum lbwr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnlbwr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
	if `wlstest'==.{
		local wlstest = -99
	}	
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnlbwr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnlbwr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bslbwr
	postfile bslbwr wild_t using "$datatemp/bslbwr.dta", replace

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
			qui areg ywild `X' if ~white [aw=births], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bslbwr (`wild_t')
		restore
	}
	postclose bslbwr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bslbwr.dta", clear
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
*Run Model
	areg lnlbwr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table5B.xls", `r' keep(HCPOST) noparen noaster ctitle("lbwr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append

	
**check the pooled model lbwr vs sr	

*******************
*SEX RATIO AT BIRTH*
*******************
qui sum sr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg sr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
	if `wlstest'==.{
		local wlstest = -99
	}	
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg sr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg sr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bssr
	postfile bssr wild_t using "$datatemp/bssr.dta", replace

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
			qui areg ywild `X' if ~white [aw=births], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bssr (`wild_t')
		restore
	}
	postclose bssr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bssr.dta", clear	
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
*Run Model
	areg sr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table5B.xls", `r' keep(HCPOST) noparen noaster ctitle("sr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

****************
*MATERNAL MORTALITY
****************	
local X = "YMXY* RXY* pcinc hpc bpc HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28"
qui sum mmr if exp==-1   [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/births
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1-HC_Texp_10 HCPOST HC_Texp_19: gen WX = W*X
	drop W
	cap areg mmr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	cap testparm WHCPOST
	local wlstest = r(p)
	if `wlstest'==.{
		local wlstest = -99
	}	
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg mmr `X' if ~white  [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg mmr `X' if ~white  [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsmmr
	postfile bsmmr wild_t using "$datatemp/bsmmr.dta", replace

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
			qui areg ywild `X' if ~white   [aw=births], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsmmr (`wild_t')
		restore
	}
	postclose bsmmr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsmmr.dta", clear
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
*Run Model
	areg mmr `X' if ~white  [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table5B.xls", `r' keep(HCPOST) noparen noaster ctitle("mmr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append

	
xi: reg missr i.stfips i.year*i.region i.year*i.ymcaid pcinc hpc bpc HC_Texp_1-HC_Texp_3 HC_Texp_18 HCPOST HC_Texp_28 [aw=births] if year>=1952 & ~inlist(stfips,9,25) , cluster(stfips)


