global breps 1000
global q 1000
set seed 8301983

use "$data/vs_mcaid" if white & year<=1979, replace
keep stfips year hb births
ren hb hbw
ren births birthsw
save "$datatemp/hbw", replace
use "$data/vs_mcaid" if ~white & year<=1979, replace
merge 1:1 stfips year using "$datatemp/hbw"
drop _merge
gen diff = hbr - hbw

drop if inlist(stfips,2,4,15)

gen error = (year==1965)*(stfips==13|stfips==22|stfips==38|stfips==42|stfips==38|stfips==51|stfips==54) 
gen deepsouth = ~(stfips~=1 & stfips~=13 & stfips~=22 & stfips~=28 & stfips~=45)

*treatment vars
cap drop exp
gen exp = year - ymcaid
recode exp (10/22 = 10) (-23/-17 = -17)
char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X

gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

gen trend = exp*(exp>-17 & exp<10)
gen trend_POST = trend*POST
for var trend trend_POST: gen HCX = crate0*X

*fixed effects
xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)


************
*ALL STATES*
************
*define ES models
local X "error YMXY* RXY* pcinc hpc HC_Texp*"
local r replace
*WLS
xi: areg diff `X' [aw=births], a(stfips) cluster(stfips)

*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
outreg2 using "$output/figureA2E3_hospbirths.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("w_all")
local r append 

************
*SOUTH ONLY*
************
*define ES models
local X "error _Texp* i.deepsouth*i.year HC_Texp*"
*WLS
xi: areg diff `X' if region==4|region==3, a(stfips) cluster(stfips)

*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
outreg2 using "$output/figureA2E3_hospbirths.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("w_south")


************
*ALL STATES*
************
*define DD models
local X "error YMXY* RXY* pcinc hpc HC_Texp_1 HC_Texp_2 HC_Texp_18 HCPOST HC_Texp_28"
local r replace
*WLS
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg diff `X' [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg diff `X' [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bshbwls
	postfile bshbwls wild_t using "$datatemp/bshbwls.dta", replace

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
			qui areg ywild `X' [aw=births], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bshbwls (`wild_t')
		restore
	}
	postclose bshbwls
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bshbwls",clear
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
areg diff `X' [aw=births] , a(stfips) cluster(stfips)
*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
outreg2 using "$output/figureA2E3_hospbirths_dd.xls", `r' keep(HCPOST) noparen noaster ctitle("w_all") addstat(bsp, `bsp')
local r append

************
*SOUTH ONLY*
************
*define ES models
local X "error _Texp* i.deepsouth*i.year HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"

*WLS
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui xi: areg diff `X' if region==4|region==3, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui xi: areg diff `X' if region==4|region==3, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bshbwls_s
	postfile bshbwls_s wild_t using "$datatemp/bshbwls_s.dta", replace

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
			qui xi: areg ywild `X' if region==4|region==3, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bshbwls_s (`wild_t')
		restore
	}
	postclose bshbwls_s
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bshbwls_s",clear
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


xi: areg diff `X' if region==4|region==3, a(stfips) cluster(stfips)

*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
outreg2 using "$output/figureA2E3_hospbirths_dd.xls", `r' keep(HCPOST) noparen noaster ctitle("uw_south") addstat(bsp, `bsp')











			********************************
			********************************
			********************************
			********************************
			********************************
			*************BINARY*************
			********************************
			********************************
			********************************
			********************************
use "$data/vs_mcaid" if white & year<=1979, replace
keep stfips year hb births
ren hb hbw
ren births birthsw
save "$datatemp/hbw", replace
use "$data/vs_mcaid" if ~white & year<=1979, replace
merge 1:1 stfips year using "$datatemp/hbw"
drop _merge
gen diff = hbr - hbw

drop if inlist(stfips,2,4,15)

gen error = (year==1965)*(stfips==13|stfips==22|stfips==38|stfips==42|stfips==38|stfips==51|stfips==54) 
gen deepsouth = ~(stfips~=1 & stfips~=13 & stfips~=22 & stfips~=28 & stfips~=45)

*treatment vars
cap drop exp
gen exp = year - ymcaid
recode exp (10/22 = 10) (-23/-17 = -17)
char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = hc*X

gen POST  		= exp>0 & exp<10
gen HCPOST		= hc*POST

gen trend = exp*(exp>-17 & exp<10)
gen trend_POST = trend*POST
for var trend trend_POST: gen HCX = hc*X

*fixed effects
xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)

************
*ALL STATES*
************
*define ES models
local X "error YMXY* RXY* pcinc hpc HC_Texp*"

*WLS
xi: areg diff `X' [aw=births], a(stfips) cluster(stfips)

*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
outreg2 using "$output/figureA2E3_hospbirths.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("bw_all")

************
*SOUTH ONLY*
************
*define ES models
local X "error _Texp* i.deepsouth*i.year HC_Texp*"
*WLS
xi: areg diff `X' if region==4|region==3, a(stfips) cluster(stfips)

*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
outreg2 using "$output/figureA2E3_hospbirths.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("buw_south")

************
*ALL STATES*
************
*define DD models
local X "error YMXY* RXY* pcinc hpc HC_Texp_1 HC_Texp_2 HC_Texp_18 HCPOST HC_Texp_28"
*WLS
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg diff `X' [aw=births], a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg diff `X' [aw=births], a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = hc*POST

	cap postclose bshbwls
	postfile bshbwls wild_t using "$datatemp/bshbwls.dta", replace

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
			qui areg ywild `X' [aw=births], a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bshbwls (`wild_t')
		restore
	}
	postclose bshbwls
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bshbwls",clear
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
areg diff `X' [aw=births] , a(stfips) cluster(stfips)
*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
outreg2 using "$output/figureA2E3_hospbirths_dd.xls", `r' keep(HCPOST) noparen noaster ctitle("bw_all") addstat(bsp, `bsp')


************
*SOUTH ONLY*
************
*define ES models
local X "error _Texp* i.deepsouth*i.year HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"

*WLS
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui xi: areg diff `X' if region==4|region==3, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui xi: areg diff `X' if region==4|region==3, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = hc*POST

	cap postclose bshbwls_s
	postfile bshbwls_s wild_t using "$datatemp/bshbwls_s.dta", replace

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
			qui xi: areg ywild `X' if region==4|region==3, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bshbwls_s (`wild_t')
		restore
	}
	postclose bshbwls_s
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bshbwls_s",clear
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


xi: areg diff `X' if region==4|region==3, a(stfips) cluster(stfips)

*OUTREG THE ES RESULTS WITH TEST p-values AND DD ESTIMATE
outreg2 using "$output/figureA2E3_hospbirths_dd.xls", `r' keep(HCPOST) noparen noaster ctitle("buw_south") addstat(bsp, `bsp')



exit
