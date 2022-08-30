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
gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST
gen ZHCPOST		= crate1958*POST

gen lnamrch = ln(amrch)

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

local X1 = "crate0 _Texp* HC_Texp* [aw=popch]"
local X2 = "S_* YMXY* pcinc hpc bpc HC_Texp* [aw=popch]"
local X3 = "S_* YMXY* RXY* pcinc hpc bpc HC_Texp* [aw=popch]"
local X4 = "S_* YMXY* RXY* pcinc hpc bpc HC_Texp*"
local X5 = "S_* SY_* YMXY* RXY* pcinc hpc bpc HC_Texp* [aw=popch]"
local X6 = "wh_* HC_Texp* [aw=popch]"
local X7 = "S_* YMXY* RXY* pcinc hpc bpc (HC_Texp* = ZHC_Texp*) [aw=popch]"

local DX1 = "crate0 _Texp* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch]"
local DX2 = "S_* YMXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch]"
local DX3 = "S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch]"
local DX4 = "S_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"
local DX5 = "S_* SY_* YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch]"
local DX6 = "wh_* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch]"
local DX7 = "S_* YMXY* RXY* pcinc hpc bpc (HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 = ZHC_Texp_1 ZHC_Texp_18 ZHCPOST ZHC_Texp_28) [aw=popch]"


/*********************/
/**WHITE*/
/*********************/
for var _Texp* POST: replace HCX = crate0*X
forval i = 1/5{
	reg lnamrch HC_Texp_2-HC_Texp_5 `DX`i'' if white, cluster(stfips)
	local dd = _b[HCPOST]
	local ddse = _se[HCPOST]
	*Run model `i'
	reg lnamrch `X`i'' if white, cluster(stfips)
	outreg2 using "$output/figureA2C9_amr_w_byspec.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("X`i'") addstat(dd, `dd', ddse, `ddse')
	local r append
}

**Pooled model with state-by-year FE
for var HC*: replace X = X*(white==1)
cap egen hcnw = total(crate0*(white==1)), by(stfips year)
qui sum amrch if exp==-1 & hcnw & white [aw=popch]
local wmdv = r(mean)
qui sum amrch if exp==-1 & hcnw & ~white [aw=popch]
local mdv = `wmdv' - r(mean)

*Run model 6 
areg lnamrch `DX6', a(stxy) cluster(str)
local dd = _b[HCPOST]
local ddse = _se[HCPOST]
areg lnamrch `X6', a(stxy) cluster(str)
outreg2 using "$output/figureA2C9_amr_w_byspec.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("X6") addstat(dd, `dd', ddse, `ddse')


*Run model 7 - Instrumenting for high/low group with 1958 high/low group
ivregress 2sls lnamrch `DX7' if white, vce(cluster stfips)
local dd = _b[HCPOST]
local ddse = _se[HCPOST]
ivregress 2sls lnamrch `X7' if white, vce(cluster stfips)
outreg2 using "$output/figureA2C9_amr_w_byspec.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("X7") addstat(dd, `dd', ddse, `ddse')



