use "$data/othprog_mcaid", clear
	
char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X
gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

*fixed effects
egen rxy = group(region year)
egen ymxy = group(ymcaid year)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)

local X "YMXY* RXY* pcinc hpc bpc HC_Texp*"
local DX "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_9 HCPOST HC_Texp_19"
local r replace

foreach var of varlist pcrfund_hs pcrfund_health pcrfund_chc pccase_fs{	
	cap drop pwt
	if "`var'"=="pcrfund_hs"{
		gen pwt = hspop
	}
	if "`var'"~="pcrfund_hs"{
		gen pwt = poptot
	}	
	/***************OLS**************/
	*RUN DD MODEL
	areg `var' `DX' , a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
	
	*RUN ES MODEL
	areg `var' `X' , a(stfips) cluster(stfips)

	*test joint significance of pre/post coefs
	testparm HC_Texp_2 - HC_Texp_7
		local pre = r(p)
	testparm HC_Texp_9 - HC_Texp_18
		local post = r(p)

	*test DD assumption
	*1. pre-coefs are jointly zero	
	testparm HC_Texp_2 - HC_Texp_7
	*2. post-coefs are equal
	test 	_b[HC_Texp_9] = _b[HC_Texp_10] = _b[HC_Texp_11] =  ///
			_b[HC_Texp_12] = _b[HC_Texp_13] = _b[HC_Texp_14] = ///
			_b[HC_Texp_15] = _b[HC_Texp_16] = _b[HC_Texp_17] = ///
			_b[HC_Texp_18], accumulate
	local ddtest = r(p)

	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("`var'_ols") addstat(pre, `pre', post, `post', ddtest, `ddtest', dd, `dd', ddse, `ddse')
		local r append


	/***************WLS**************/		
	*RUN DD MODEL
	areg `var' `DX' [aw=pwt] , a(stfips) cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	

	*RUN ES MODEL
	areg `var' `X' [aw=pwt], a(stfips) cluster(stfips)

	*test joint significance of pre/post coefs
	testparm HC_Texp_2 - HC_Texp_7
		local pre = r(p)
	testparm HC_Texp_9 - HC_Texp_18
		local post = r(p)
		
	*test DD assumption
	*1. pre-coefs are jointly zero
	testparm HC_Texp_2 - HC_Texp_7
	*2. post-coefs are equal
	test 	_b[HC_Texp_9] = _b[HC_Texp_10] = _b[HC_Texp_11] =  ///
			_b[HC_Texp_12] = _b[HC_Texp_13] = _b[HC_Texp_14] = ///
			_b[HC_Texp_15] = _b[HC_Texp_16] = _b[HC_Texp_17] = ///
			_b[HC_Texp_18], accumulate
	local ddtest = r(p)

	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("`var'_wls") addstat(pre, `pre', post, `post', ddtest, `ddtest', dd, `dd', ddse, `ddse')
}
	

	
	
use "$data/vs_mcaid" if inrange(year,1959,1979), clear
replace crate = crate/100
char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X
gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

*fixed effects
egen rxy = group(region year)
egen ymxy = group(ymcaid year)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)


local X "YMXY* RXY* pcinc hpc bpc HC_Texp*"
local DX "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_9 HCPOST HC_Texp_19"
	
*White AFDC
	/***************OLS**************/
	*RUN DD MODEL
	areg crate `DX' if white, a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	
	
	*RUN ES MODEL
	areg crate `X' if white, a(stfips) cluster(stfips)
	
	*test joint significance of pre/post coefs
	testparm HC_Texp_2 - HC_Texp_7
		local pre = r(p)
	testparm HC_Texp_9 - HC_Texp_18
		local post = r(p)
	
	*test DD assumption
	*1. pre-coefs are jointly zero
	testparm HC_Texp_2 - HC_Texp_7
	*2. post-coefs are equal
	test 	_b[HC_Texp_9] = _b[HC_Texp_10] = _b[HC_Texp_11] =  ///
			_b[HC_Texp_12] = _b[HC_Texp_13] = _b[HC_Texp_14] = ///
			_b[HC_Texp_15] = _b[HC_Texp_16] = _b[HC_Texp_17] = ///
			_b[HC_Texp_18], accumulate
	local ddtest = r(p)
	
	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("wcrate_ols") addstat(pre, `pre', post, `post', ddtest, `ddtest', dd, `dd', ddse, `ddse')
	local r append
	

	/***************WLS**************/		
	*RUN DD MODEL
	areg crate `DX' [aw=crpop] if white, a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	
	
	*RUN ES MODEL
	areg crate `X' [aw=crpop] if white, a(stfips) cluster(stfips)
	
	*test joint significance of pre/post coefs
	testparm HC_Texp_2 - HC_Texp_7
		local pre = r(p)
	testparm HC_Texp_9 - HC_Texp_18
		local post = r(p)
		
	*test DD assumption
	*1. pre-coefs are jointly zero
	testparm HC_Texp_2 - HC_Texp_7
	*2. post-coefs are equal
	test 	_b[HC_Texp_9] = _b[HC_Texp_10] = _b[HC_Texp_11] =  ///
			_b[HC_Texp_12] = _b[HC_Texp_13] = _b[HC_Texp_14] = ///
			_b[HC_Texp_15] = _b[HC_Texp_16] = _b[HC_Texp_17] = ///
			_b[HC_Texp_18], accumulate
	local ddtest = r(p)

	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("wcrate_wls") addstat(pre, `pre', post, `post', ddtest, `ddtest', dd, `dd', ddse, `ddse')

	
*Nonwhite AFDC
	/***************OLS**************/
	*RUN DD MODEL
	areg crate `DX' if ~white, a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	
	
	*RUN ES MODEL
	areg crate `X' if ~white, a(stfips) cluster(stfips)
	
	*test joint significance of pre/post coefs
	testparm HC_Texp_2 - HC_Texp_7
		local pre = r(p)
	testparm HC_Texp_9 - HC_Texp_18
		local post = r(p)
	
	*test DD assumption
	*1. pre-coefs are jointly zero
	testparm HC_Texp_2 - HC_Texp_7
	*2. post-coefs are equal
	test 	_b[HC_Texp_9] = _b[HC_Texp_10] = _b[HC_Texp_11] =  ///
			_b[HC_Texp_12] = _b[HC_Texp_13] = _b[HC_Texp_14] = ///
			_b[HC_Texp_15] = _b[HC_Texp_16] = _b[HC_Texp_17] = ///
			_b[HC_Texp_18], accumulate
	local ddtest = r(p)
	
	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("nwcrate_ols") addstat(pre, `pre', post, `post', ddtest, `ddtest', dd, `dd', ddse, `ddse')
	local r append
	

	/***************WLS**************/		
	*RUN DD MODEL
	areg crate `DX' [aw=crpop] if ~white, a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	
	
	*RUN ES MODEL
	areg crate `X' [aw=crpop] if ~white, a(stfips) cluster(stfips)
	
	*test joint significance of pre/post coefs
	testparm HC_Texp_2 - HC_Texp_7
		local pre = r(p)
	testparm HC_Texp_9 - HC_Texp_18
		local post = r(p)
		
	*test DD assumption
	*1. pre-coefs are jointly zero
	testparm HC_Texp_2 - HC_Texp_7
	*2. post-coefs are equal
	test 	_b[HC_Texp_9] = _b[HC_Texp_10] = _b[HC_Texp_11] =  ///
			_b[HC_Texp_12] = _b[HC_Texp_13] = _b[HC_Texp_14] = ///
			_b[HC_Texp_15] = _b[HC_Texp_16] = _b[HC_Texp_17] = ///
			_b[HC_Texp_18], accumulate
	local ddtest = r(p)

	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("nwcrate_wls") addstat(pre, `pre', post, `post', ddtest, `ddtest', dd, `dd', ddse, `ddse')

	
**MVP
use "$data/mvp_mcaid", clear
drop if stfips==54|stfips==4

for var pcmvp*: replace X = X*1000

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X
gen POST  		= exp>0 & exp<7
gen HCPOST		= crate0*POST

*fixed effects
egen rxy = group(region year)
egen ymxy = group(ymcaid year)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)


local X "YMXY* RXY* pcinc hpc bpc HC_Texp*"
local DX "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_5 HCPOST HC_Texp_12"
local r replace	
*MVP Rate
	/***************OLS**************/
	*RUN DD MODEL
	areg rmvp_ch `DX', a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	
	
	*RUN ES MODEL
	areg rmvp_ch `X', a(stfips) cluster(stfips)
		
	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9B.xls", `r' keep(HC_Texp_2-HC_Texp_11) noparen noaster ctitle("rmvp_ols") addstat(dd, `dd', ddse, `ddse')
	local r append
	

	/***************WLS**************/		
	*RUN DD MODEL
	areg rmvp_ch `DX' [aw=ch_pop] , a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	
	
	*RUN ES MODEL
	areg rmvp_ch `X' [aw=ch_pop] , a(stfips) cluster(stfips)

	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9B.xls", `r' keep(HC_Texp_2-HC_Texp_11) noparen noaster ctitle("rmvp_wls") addstat(dd, `dd', ddse, `ddse')

	
*PC MVP Exp
	/***************OLS**************/
	*RUN DD MODEL
	areg pcmvp_ch `DX', a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	
	
	*RUN ES MODEL
	areg pcmvp_ch `X', a(stfips) cluster(stfips)
		
	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9B.xls", `r' keep(HC_Texp_2-HC_Texp_11) noparen noaster ctitle("pcmvp_ols") addstat(dd, `dd', ddse, `ddse')
	local r append
	

	/***************WLS**************/		
	*RUN DD MODEL
	areg pcmvp_ch `DX' [aw=ch_pop] , a(stfips) cluster(stfips)		
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]	
	
	*RUN ES MODEL
	areg pcmvp_ch `X' [aw=ch_pop] , a(stfips) cluster(stfips)

	*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
	outreg2 using "$output/figure9B.xls", `r' keep(HC_Texp_2-HC_Texp_11) noparen noaster ctitle("pcmvp_wls") addstat(dd, `dd', ddse, `ddse')
	
	
