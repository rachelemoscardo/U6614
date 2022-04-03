global breps 1000
global q 1000
set seed 8301983

**Run regressions from table 4 - the main infant outcome regressions
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

gen nmr1 = nmr-dmr1
gen lndmr1		= ln(dmr1)
gen lnnmr1 		= ln(nmr1)
gen lnnmr 		= ln(nmr)
gen lnpnmr 		= ln(pnmr)
gen lnimr		= ln(imr)

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
*FIRST DAY IMR*
****************
qui sum dmr1 if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg lndmr1 `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg lndmr1 `X' [aw=births], cluster(stfips) 
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table4A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lndmr1") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
local r append

**************
*Day 2-28*
**************
qui sum nmr1 if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg lnnmr1 `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg lnnmr1 `X' [aw=births], cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table4A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("nmr1") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
local r append

**************
*NEONATAL IMR*
**************
qui sum nmr if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg lnnmr `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg lnnmr `X' [aw=births], cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table4A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("nmr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
local r append

*******************
*POST-NEONATAL IMR*
*******************
qui sum pnmr if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg lnpnmr `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg lnpnmr `X' [aw=births], cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table4A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("pnmr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')

*******************
*IMR*
*******************
qui sum imr if exp==-1 [aw=births]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui reg lnimr `X' W* if ~inlist(stfips,23,33,50), cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	reg lnimr `X' [aw=births], cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table4A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("imr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')


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
*FIRST DAY IMR*
****************
qui sum dmr1 if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lndmr1 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lndmr1 `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lndmr1 `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsdmr1
	postfile bsdmr1 wild_t using "$datatemp/bsdmr1.dta", replace

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
			qui post bsdmr1 (`wild_t')
		restore
	}
	postclose bsdmr1
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsdmr1.dta", clear
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
	areg lndmr1 `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4B.xls", `r' keep(HCPOST) noparen noaster ctitle("dmr1") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append


**************
*Day 2-28*
**************
qui sum nmr1 if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnnmr1 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnnmr1 `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnnmr1 `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsnmr1
	postfile bsnmr1 wild_t using "$datatemp/bsnmr1.dta", replace

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
			qui post bsnmr1 (`wild_t')
		restore
	}
	postclose bsnmr1
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsnmr1.dta", clear
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
	areg lnnmr1 `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4B.xls", `r' keep(HCPOST) noparen noaster ctitle("nmr1") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append

	
	
	
**************
*NEONATAL IMR*
**************
qui sum nmr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnnmr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnnmr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnnmr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsnmr
	postfile bsnmr wild_t using "$datatemp/bsnmr.dta", replace

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
			qui post bsnmr (`wild_t')
		restore
	}
	postclose bsnmr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsnmr.dta", clear
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
	areg lnnmr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4B.xls", `r' keep(HCPOST) noparen noaster ctitle("nmr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append


*******************
*POST-NEONATAL IMR*
*******************
qui sum pnmr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnpnmr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnpnmr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnpnmr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bspnmr
	postfile bspnmr wild_t using "$datatemp/bspnmr.dta", replace

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
			qui post bspnmr (`wild_t')
		restore
	}
	postclose bspnmr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bspnmr.dta", clear	
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
	areg lnpnmr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4B.xls", `r' keep(HCPOST) noparen noaster ctitle("pnmr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

****************
*	IMR		*
****************	
	qui sum imr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnimr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnimr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnimr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsimr
	postfile bsimr wild_t using "$datatemp/bsimr.dta", replace

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
			qui post bsimr (`wild_t')
		restore
	}
	postclose bsimr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsimr.dta", clear
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
	areg lnimr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4B.xls", `r' keep(HCPOST) noparen noaster ctitle("imr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append

	

	
/*************************************/
/**Single Coef Diff-in-Diff, Panel C**/
/**CONTROLLING FOR BIRTHWEIGHT VARS **/
/*************************************/
local X = "YMXY* RXY* pcinc hpc bpc lbwr vlbwr HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"

cap drop W* 
qui gen W = 1/births
qui for var S_* YMXY* RXY* pcinc hpc bpc lbwr vlbwr HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WX = W*X
drop W

local r replace

****************
*FIRST DAY IMR*
****************
qui sum dmr1 if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lndmr1 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lndmr1 `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lndmr1 `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsdmr1
	postfile bsdmr1 wild_t using "$datatemp/bsdmr1.dta", replace

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
			qui post bsdmr1 (`wild_t')
		restore
	}
	postclose bsdmr1
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsdmr1.dta", clear
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
	areg lndmr1 `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4C.xls", `r' keep(HCPOST lbwr vlbwr) noparen noaster ctitle("dmr1") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append

	
**************
*Day 2-28*
**************
qui sum nmr1 if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnnmr1 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnnmr1 `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnnmr1 `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsnmr1
	postfile bsnmr1 wild_t using "$datatemp/bsnmr1.dta", replace

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
			qui post bsnmr1 (`wild_t')
		restore
	}
	postclose bsnmr1
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsnmr1.dta", clear
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
	areg lnnmr1 `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4C.xls", `r' keep(HCPOST lbwr vlbwr) noparen noaster ctitle("nmr1") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append

	
**************
*NEONATAL IMR*
**************
qui sum nmr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnnmr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnnmr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnnmr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsnmr
	postfile bsnmr wild_t using "$datatemp/bsnmr.dta", replace

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
			qui post bsnmr (`wild_t')
		restore
	}
	postclose bsnmr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsnmr.dta", clear
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
	areg lnnmr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4C.xls", `r' keep(HCPOST lbwr vlbwr) noparen noaster ctitle("nmr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append

	
**check the pooled model NMR vs PNMR	

*******************
*POST-NEONATAL IMR*
*******************
qui sum pnmr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnpnmr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnpnmr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnpnmr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bspnmr
	postfile bspnmr wild_t using "$datatemp/bspnmr.dta", replace

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
			qui post bspnmr (`wild_t')
		restore
	}
	postclose bspnmr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bspnmr.dta", clear	
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
	areg lnpnmr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4C.xls", `r' keep(HCPOST lbwr vlbwr) noparen noaster ctitle("pnmr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

****************
*	IMR		*
****************	
	qui sum imr if exp==-1 [aw=births]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnimr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnimr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnimr `X' if ~white [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsimr
	postfile bsimr wild_t using "$datatemp/bsimr.dta", replace

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
			qui post bsimr (`wild_t')
		restore
	}
	postclose bsimr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsimr.dta", clear
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
	areg lnimr `X' [aw=births], a(stfips) cluster(stfips)
	outreg2 using "$output/table4C.xls", `r' keep(HCPOST lbwr vlbwr) noparen noaster ctitle("imr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	local r append	


	
	
	
	

	

	
