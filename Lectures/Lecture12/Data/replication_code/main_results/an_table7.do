**Run regressions from table 7 - child mort by age/cause
global breps 1000
global q 1000
set seed 8301983

use "$data/vs_mcaid" if inrange(year,1950,1979) & ~white, clear

drop if inlist(stfips,2,4,15)

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X

egen D 			= cut(exp), at(-17,-16,-11,-7,-1,0,1,5,10,1000)
char D[omit] -1
xi i.D, pref(_D)
for var _D*: gen HCX = crate0*X

gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

gen lnasmr14 		= ln(asmr14)
gen lnasmr59 		= ln(asmr59)
gen lnasmr1014		= ln(asmr1014)
gen lnamrch_int		= ln(amrch_int)
gen lnamrch_ext		= ln(amrch_ext)
gen lnamrch_tr		= ln(amrch_tr)
replace amrch_untr	= amrch_untr+amrch_c4
gen lnamrch_untr	= ln(amrch_untr)

replace popch = pop14+pop59+pop1014

*fixed effects
xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)

/********************************/
/**Grouped Event-Study, Panel A**/
/********************************/
local X = "YMXY* RXY* pcinc hpc bpc HC_DD*"
local r replace

*********************
*CHILD MORTALITY, 1-4
*********************
qui sum asmr14 if exp==-1 & hc [aw=pop14]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/pop14
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_DD*: gen WX = W*X
	drop W
	qui areg lnasmr14 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	areg lnasmr14 `X' [aw=pop14], a(stfips) cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table7A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lnasmr14") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')
local r append

**********************
*CHILD MORTALITY, 5-9
**********************
qui sum asmr59 if exp==-1 & hc [aw=pop59]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/pop59
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_DD*: gen WX = W*X
	drop W
	qui areg lnasmr59 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	areg lnasmr59 `X' [aw=pop59], a(stfips) cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table7A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lnasmr59") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')

**********************
*CHILD MORTALITY, 10-14
**********************
qui sum asmr1014 if exp==-1 & hc [aw=pop1014]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/pop1014
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_DD*: gen WX = W*X
	drop W
	qui areg lnasmr1014 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	areg lnasmr1014 `X' [aw=pop1014], a(stfips) cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table7A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lnasmr1014") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')


******************************
*INTERNAL-CAUSE MORTALITY, 1-14
******************************
qui sum amrch_int if exp==-1 & hc [aw=popch]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/popch
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_DD*: gen WX = W*X
	drop W
	qui areg lnamrch_int `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	areg lnamrch_int `X' [aw=popch], a(stfips) cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table7A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lnamrch_int") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')

******************************
*EXTERNAL-CAUSE MORTALITY, 1-14
******************************
qui sum amrch_ext if exp==-1 & hc [aw=popch]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui areg lnamrch_ext `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_2 WHC_DD_3 WHC_DD_4 WHC_DD_6 WHC_DD_7  WHC_DD_8
	local wlstest = r(p)
*Run Model
	areg lnamrch_ext `X' [aw=popch], a(stfips) cluster(stfips)
*test DD assumption
	testparm HC_DD_2 HC_DD_3 HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table7A.xls", `r' keep(HC_DD_2 HC_DD_3 HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lnamrch_ext") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')

*******************************
*TREATABLE-CAUSE MORTALITY, 1-14
*******************************
qui sum amrch_tr if exp==-1 & hc [aw=popch]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui areg lnamrch_tr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_3 WHC_DD_6 WHC_DD_7
	local wlstest = r(p)
	if `wlstest'==.{
		local wlstest = -99
	}
*Run model
	areg lnamrch_tr `X' [aw=popch], a(stfips) cluster(stfips)
*test DD assumption
	testparm HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table7A.xls", `r' keep(HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lnamrch_tr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')


*********************************
*UNTREATABLE-CAUSE MORTALITY, 1-14
*********************************
qui sum amrch_untr if exp==-1 & hc [aw=popch]
local mdv = r(mean)
*WLS Test (Deaton pg 72)
	qui areg lnamrch_untr `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHC_DD_3 WHC_DD_6 WHC_DD_7
	local wlstest = r(p)
	if `wlstest'==.{
		local wlstest = -99
	}	
*Run model
	areg lnamrch_untr `X' [aw=popch], a(stfips) cluster(stfips)
*test DD assumption
	testparm HC_DD_4
	test  _b[HC_DD_7] = _b[HC_DD_8], accumulate
	local ddtest = r(p)
outreg2 using "$output/table7A.xls", `r' keep(HC_DD_4 HC_DD_6 HC_DD_7 HC_DD_8) noparen noaster ctitle("lnamrch_untr") addstat(wlstest, `wlstest', ddtest, `ddtest', mdv, `mdv')



/*************************************/
/**Single Coef Diff-in-Diff, Panel B**/
/*************************************/
local X = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"
local XT = "YMXY* RXY* pcinc hpc bpc HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28"
local r replace

**********************
*CHILD MORTALITY, 1-4
**********************
qui sum asmr14 if exp==-1 & hc [aw=pop14]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/pop14
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WX = W*X
	drop W
	qui areg lnasmr14 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr14 `X' if ~white [aw=pop14], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr14 `X' if ~white [aw=pop14], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr14
	postfile bsasmr14 wild_t using "$datatemp/bsasmr14.dta", replace

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
			qui areg ywild `X' if ~white [aw=pop14], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr14 (`wild_t')
		restore
	}
	postclose bsasmr14
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr14.dta", clear
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
	areg lnasmr14 `X' [aw=pop14], a(stfips) cluster(stfips)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnasmr14") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
local r append

***********************
*CHILD MORTALITY, 5-9
***********************
qui sum asmr59 if exp==-1 & hc [aw=pop59]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/pop59
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WX = W*X
	drop W
	qui areg lnasmr59 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr59 `X' if ~white [aw=pop59], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr59 `X' if ~white [aw=pop59], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr59
	postfile bsasmr59 wild_t using "$datatemp/bsasmr59.dta", replace

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
			qui areg ywild `X' if ~white [aw=pop59], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr59 (`wild_t')
		restore
	}
	postclose bsasmr59
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr59.dta", clear
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
	areg lnasmr59 `X' [aw=pop59], a(stfips) cluster(stfips)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnasmr59") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

	
***********************
*CHILD MORTALITY, 5-9
***********************
qui sum asmr1014 if exp==-1 & hc [aw=pop1014]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/pop1014
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WX = W*X
	drop W
	qui areg lnasmr1014 `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr1014 `X' if ~white [aw=pop1014], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr1014 `X' if ~white [aw=pop1014], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsasmr1014
	postfile bsasmr1014 wild_t using "$datatemp/bsasmr1014.dta", replace

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
			qui areg ywild `X' if ~white [aw=pop1014], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsasmr1014 (`wild_t')
		restore
	}
	postclose bsasmr1014
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsasmr1014.dta", clear
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
	areg lnasmr1014 `X' [aw=pop1014], a(stfips) cluster(stfips)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnasmr1014") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')
	
******************************	
*INTERNAL-CAUSE MORTALITY, 1-14
******************************
qui sum amrch_int if exp==-1 & hc [aw=popch]
local mdv = r(mean)
{
	*WLS Test (Deaton pg 72)
	cap drop W* 
	qui gen W = 1/popch
	qui for var S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WX = W*X
	drop W
	qui areg lnamrch_int `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnamrch_int `X' if ~white [aw=popch], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnamrch_int `X' if ~white [aw=popch], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsamrch_int
	postfile bsamrch_int wild_t using "$datatemp/bsamrch_int.dta", replace
	
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
			qui areg ywild `X' if ~white [aw=popch], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsamrch_int (`wild_t')
		restore
	}
	postclose bsamrch_int
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsamrch_int.dta", clear
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
	areg lnamrch_int `X' [aw=popch], a(stfips) cluster(stfips)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnamrch_int") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

******************************
*EXTERNAL-CAUSE MORTALITY, 1-14
******************************
qui sum amrch_ext if exp==-1 & hc [aw=popch]
local mdv = r(mean)
{
	*WLS Test (Deaton pg 72)
	qui areg lnamrch_ext `X' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
	di "WLS Test"
	testparm WHCPOST
	local wlstest = r(p)
*Wild percentile-t bootstrap
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnamrch_ext `X' if ~white [aw=popch], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnamrch_ext `X' if ~white [aw=popch], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsamrch_ext
	postfile bsamrch_ext wild_t using "$datatemp/bsamrch_ext.dta", replace

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
			qui areg ywild `X' if ~white [aw=popch], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsamrch_ext (`wild_t')
		restore
	}
	postclose bsamrch_ext
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsamrch_ext.dta", clear
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
	areg lnamrch_ext `X' [aw=popch], a(stfips) cluster(stfips)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnamrch_ext") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

*******************************
*TREATABLE-CAUSE MORTALITY, 1-14
*******************************
qui sum amrch_tr if exp==-1 & hc [aw=popch]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnamrch_tr `XT' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
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
	qui areg lnamrch_tr `XT' if ~white [aw=popch], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnamrch_tr `XT' if ~white [aw=popch], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsamrch_tr
	postfile bsamrch_tr wild_t using "$datatemp/bsamrch_tr.dta", replace

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
			qui areg ywild `XT' if ~white [aw=popch], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsamrch_tr (`wild_t')
		restore
	}
	postclose bsamrch_tr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsamrch_tr.dta", clear
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
	areg lnamrch_tr `XT' [aw=popch], a(stfips) cluster(stfips)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnamrch_tr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

*********************************
*UNTREATABLE-CAUSE MORTALITY, 1-14
*********************************
qui sum amrch_untr if exp==-1 & hc [aw=popch]
local mdv = r(mean)
{
*WLS Test (Deaton pg 72)
	qui areg lnamrch_untr `XT' W* if ~inlist(stfips,23,33,50), a(stfips) cluster(stfips)
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
	qui areg lnamrch_untr `XT' if ~white [aw=popch], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnamrch_untr `XT' if ~white [aw=popch], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsamrch_untr
	postfile bsamrch_untr wild_t using "$datatemp/bsamrch_untr.dta", replace

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
			qui areg ywild `XT' if ~white [aw=popch], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsamrch_untr (`wild_t')
		restore
	}
	postclose bsamrch_untr
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsamrch_untr.dta", clear
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
	areg lnamrch_untr `XT' [aw=popch], a(stfips) cluster(stfips)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnamrch_untr") addstat(bsp, `bsp', wlstest, `wlstest', mdv, `mdv')

save "$datatemp/t6temp", replace

/***********************************************************************************/
/**stack internal and external causes, estimated a joint model and test differences*/
/***********************************************************************************/
use "$datatemp/t6temp", replace
keep stfips year region ymcaid pcinc hpc bpc crate0 POST HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 popch lnamrch_int lnamrch_ext
ren lnamrch_int lnamrch1
ren lnamrch_ext lnamrch0
reshape long lnamrch, i(stfips year) j(intern)
*to get the outreg output to line up
egen sti = group(stfips intern)
xi i.stfips i.year*i.ymcaid i.year*i.region 
for var _I* pcinc hpc bpc: gen INT_X = intern*X
drop _Istfip*
for var HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen ext_X = X
for var HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: replace X = intern*X
{
*Wild percentile-t bootstrap
/*************/
cap drop e XB0
*run actual model and store 
qui areg lnamrch _I* pcinc hpc bpc INT* ext* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch], a(sti) cluster(stfips)
local t = _b[HCPOST]/_se[HCPOST]

*residuals to resample
replace HCPOST = 0
qui areg lnamrch _I* pcinc hpc bpc INT* ext* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch], a(sti) cluster(stfips)
predict e, resid
*fitted values imposing H_0 for wild-t: HCPOST==0
predict XB0, xb
replace HCPOST = crate0*POST*intern

cap postclose bsamrch_intext
postfile bsamrch_intext wild_t using "$datatemp/bsamrch_intext.dta", replace

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
		qui areg ywild _I* pcinc hpc bpc INT* ext* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch], a(sti) cluster(stfips)
		local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
		qui post bsamrch_intext (`wild_t')
	restore
}
postclose bsamrch_intext

*get p-value by calculating the percentile of the new bootstrap t-dist
preserve
	use "$datatemp/bsamrch_intext.dta", clear
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
areg lnamrch _I* pcinc hpc bpc INT* ext* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch], a(sti) cluster(stfips)
test HCPOST
local p = r(p)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnamrch_intext") addstat(bsp, `bsp', pooltest, `p')




/***************************************************************************************/
/**stack treatable and untreatable causes, estimated a joint model and test differences*/
/***************************************************************************************/
use "$datatemp/t6temp", replace
keep stfips year region ymcaid pcinc hpc bpc crate0 POST HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28 popch lnamrch_tr lnamrch_untr
ren lnamrch_tr lnamrch1
ren lnamrch_untr lnamrch0
reshape long lnamrch, i(stfips year) j(tr)
*to get the outreg output to line up
egen stt = group(stfips tr)
xi i.stfips i.year*i.ymcaid i.year*i.region  
for var _I* pcinc hpc bpc: gen TR_X = tr*X
drop _Istfip*
for var HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28: gen UNTR_X = X
for var HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28: replace X = tr*X	
{
*Wild percentile-t bootstrap
/*************/
cap drop e XB0
*run actual model and store 
qui areg lnamrch _I* pcinc hpc bpc TR* UNTR* HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch], a(stt) cluster(stfips)
local t = _b[HCPOST]/_se[HCPOST]

*residuals to resample
replace HCPOST = 0
qui areg lnamrch _I* pcinc hpc bpc TR* UNTR* HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch], a(stt) cluster(stfips)
predict e, resid
*fitted values imposing H_0 for wild-t: HCPOST==0
predict XB0, xb
replace HCPOST = crate0*POST*tr

cap postclose bsamrch_truntr
postfile bsamrch_truntr wild_t using "$datatemp/bsamrch_truntr.dta", replace

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
		qui areg ywild _I* pcinc hpc bpc TR* UNTR* HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch], a(stt) cluster(stfips)
		local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
		qui post bsamrch_truntr (`wild_t')
	restore
}
postclose bsamrch_truntr

*get p-value by calculating the percentile of the new bootstrap t-dist
preserve
	use "$datatemp/bsamrch_truntr.dta", clear
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
areg lnamrch _I* pcinc hpc bpc TR* UNTR* HC_Texp_1-HC_Texp_10 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch], a(stt) cluster(stfips)
test HCPOST
local p = r(p)
outreg2 using "$output/table7B.xls", `r' keep(HCPOST) noparen noaster ctitle("lnamrch_truntr") addstat(bsp, `bsp', pooltest, `p')


exit
