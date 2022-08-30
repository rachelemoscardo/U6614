**Run regressions from figure VI - the main age-adjusted child mortality regressions
use "$data/vs_mcaid" if inrange(year,1950,1979), clear

drop if inlist(stfips,2,4,15)
drop if stfips==54 & white

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X


gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

gen lnamrch = ln(amrch)

cap drop W* 
qui gen W = 1/popch
qui xi i.stfips i.year*i.region i.year*i.ymcaid
qui for var _I* pcinc hpc bpc HC_Texp*: gen WX = W*X
drop W

local X = "i.stfips i.year*i.region i.year*i.ymcaid pcinc hpc bpc HC_Texp*"
local DX = "i.stfips i.year*i.region i.year*i.ymcaid pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"
local r replace


*pooled DD model, test diff
qui xi i.stfips i.year*i.region i.year*i.ymcaid
qui for var _I* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WH_X = white*X
egen str = group(stfips white)
xi: reg lnamrch `DX' white WH_* [aw=popch], cluster(str)
local diffp = normal(-abs(_b[WH_HCPOST]/_se[WH_HCPOST]))*2
drop WH_*
	
/****************************/
/**NONWHITE CHILD AMR, 0-14**/
/****************************/
*WLS Test
qui xi: reg lnamrch `X' W* if ~white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_27
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnamrch `DX' [aw=popch] if ~white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnamrch `X' [aw=popch] if ~white, cluster(stfips)
*test pre- and post- sig
	testparm HC_Texp_2 - HC_Texp_16
	local pre = r(p)
	testparm HC_Texp_19 - HC_Texp_27
	local post = r(p)
*test DD assumption
	testparm HC_Texp_2 - HC_Texp_16
	test 	_b[HC_Texp_19] = _b[HC_Texp_20] =  ///
			_b[HC_Texp_21] = _b[HC_Texp_22] = _b[HC_Texp_23] = ///
			_b[HC_Texp_24] = _b[HC_Texp_25] = _b[HC_Texp_26] = ///
			_b[HC_Texp_27], accumulate
	local ddtest = r(p)
outreg2 using "$output/figure6.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("Nonwhite") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post', diffp, `diffp')
local r append

/*************************/
/**WHITE CHILD AMR, 0-14**/
/*************************/
*WLS Test
qui xi: reg lnamrch `X' W* if white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_27
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnamrch `DX' [aw=popch] if white , cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnamrch `X' [aw=popch] if white, cluster(stfips)
*test pre- and post- sig
	testparm HC_Texp_2 - HC_Texp_16
	local pre = r(p)
	testparm HC_Texp_19 - HC_Texp_27
	local post = r(p)
*test DD assumption
	testparm HC_Texp_2 - HC_Texp_16
	test 	_b[HC_Texp_19] = _b[HC_Texp_20] =  ///
			_b[HC_Texp_21] = _b[HC_Texp_22] = _b[HC_Texp_23] = ///
			_b[HC_Texp_24] = _b[HC_Texp_25] = _b[HC_Texp_26] = ///
			_b[HC_Texp_27], accumulate
	local ddtest = r(p)
outreg2 using "$output/figure6.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("White") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post', diffp, `diffp')
local r append



