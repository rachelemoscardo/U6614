**Run regressions from figure VI - the main age-adjusted child mortality regressions
use "$data/vs_mcaid" if inrange(year,1950,1979), clear

drop if inlist(stfips,2,4,15)
drop if stfips==54 & white

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = hc*X


gen POST  		= exp>0 & exp<10
gen HCPOST		= hc*POST

gen lnnmr = ln(nmr)
gen lnpnmr = ln(pnmr)
gen lnasmr14 = ln(asmr14)
gen lnasmr514 = ln(asmr514)

cap drop W* 
qui gen W = 1/popch
qui xi i.stfips i.year*i.region i.year*i.ymcaid
qui for var _I* pcinc hpc bpc HC_Texp*: gen WX = W*X
drop W

local X = "i.stfips i.year*i.region i.year*i.ymcaid pcinc hpc bpc HC_Texp*"
local DX = "i.stfips i.year*i.region i.year*i.ymcaid pcinc hpc bpc HC_Texp_1 HC_Texp_9 HCPOST HC_Texp_19*"
local r replace

/****************************/
/**NONWHITE NEONATAL IMR**/
/****************************/
*WLS Test
qui xi: reg lnnmr `X' W* if ~white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_18
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnnmr `DX' [aw=births] if ~white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnnmr `X' [aw=births] if ~white, cluster(stfips)
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
outreg2 using "$output/figureA2C3_asmr_binary.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("nwnmr") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post')
local r append

/*************************/
/**WHITE NEONATAL IMR**/
/*************************/
*WLS Test
qui xi: reg lnnmr `X' W* if white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_18
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnnmr `DX' [aw=births] if white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnnmr `X' [aw=births] if white, cluster(stfips)
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
outreg2 using "$output/figureA2C3_asmr_binary.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("wnmr") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post')
local r append



/******************************/
/**NONWHITE POST-NEONATAL IMR**/
/******************************/
*WLS Test
qui xi: reg lnpnmr `X' W* if ~white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_18
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnpnmr `DX' [aw=births] if ~white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnpnmr `X' [aw=births] if ~white, cluster(stfips)
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
outreg2 using "$output/figureA2C3_asmr_binary.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("nwpnmr") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post')
local r append

/***************************/
/**WHITE POST-NEONATAL IMR**/
/***************************/
*WLS Test
qui xi: reg lnpnmr `X' W* if white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_18
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnpnmr `DX' [aw=births] if white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnpnmr `X' [aw=births] if white, cluster(stfips)
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
outreg2 using "$output/figureA2C3_asmr_binary.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("wpnmr") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post')
local r append




/****************************/
/**NONWHITE CHILD ASMR 1-4**/
/****************************/
*WLS Test
qui xi: reg lnasmr14 `X' W* if ~white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_18
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnasmr14 `DX' [aw=pop14] if ~white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnasmr14 `X' [aw=pop14] if ~white, cluster(stfips)
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
outreg2 using "$output/figureA2C3_asmr_binary.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("nwasmr14") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post')
local r append

/*************************/
/**WHITE CHILD ASMR, 1-4**/
/*************************/
*WLS Test
qui xi: reg lnasmr14 `X' W* if white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_18
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnasmr14 `DX' [aw=pop14] if white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnasmr14 `X' [aw=pop14] if white, cluster(stfips)
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
outreg2 using "$output/figureA2C3_asmr_binary.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("wasmr14") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post')
local r append


/****************************/
/**NONWHITE CHILD ASMR 5-14**/
/****************************/
*WLS Test
qui xi: reg lnasmr514 `X' W* if ~white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_18
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnasmr514 `DX' [aw=pop514] if ~white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnasmr514 `X' [aw=pop514] if ~white, cluster(stfips)
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
outreg2 using "$output/figureA2C3_asmr_binary.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("nwasmr514") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post')
local r append

/*************************/
/**WHITE CHILD ASMR, 5-14**/
/*************************/
*WLS Test
qui xi: reg lnasmr514 `X' W* if white, cluster(stfips)
di "WLS Test"
testparm WHC_Texp_2-WHC_Texp_18
local wlstest = r(p)

*Run DD Model, store treatment effect
	xi: reg lnasmr514 `DX' [aw=pop514] if white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
*Run ES Model 
	xi: reg lnasmr514 `X' [aw=pop514] if white, cluster(stfips)
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
outreg2 using "$output/figureA2C3_asmr_binary.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("wasmr514") addstat(wlstest, `wlstest', ddtest, `ddtest', dd, `dd', ddse, `ddse', pre, `pre', post, `post')
local r append







