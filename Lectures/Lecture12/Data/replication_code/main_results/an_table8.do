global breps 1000
global q 1000
set seed 8301983

**Run regressions from table 8 - the main age-adjusted child mortality regressions
use "$data/vs_mcaid" if inrange(year,1950,1979), clear

egen hcw = total(crate0*white), by(stfips year)

drop if white

drop if inlist(stfips,2,4,15)

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X

egen D 			= cut(exp), at(-17,-16,-11,-7,-1,0,1,5,10,100)
char D[omit] -1
xi i.D, pref(_D)
for var _D*: gen HCX = crate0*X

gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

gen lnamrch 	= ln(amrch)
gen lnnmr 		= ln(nmr)
gen lnpnmr 		= ln(pnmr)
gen lnasmr14 	= ln(asmr14)


xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid*
xi i.stfips, pref(S_)

/***************************************/
/**Adding High-White-AFDC Interactions**/
/***************************************/
for var HC_Texp* HCPOST: gen WX = hcw*X

local X = "S_* RXY* YMXY* pcinc hpc bpc WHC_Texp_1 WHC_Texp_9 WHCPOST WHC_Texp_19 HC_Texp_1 HC_Texp_9 HCPOST HC_Texp_19"
local r replace

*CHILD AMR
qui sum amrch if exp==-1 & hc & ~white [aw=popch]
local mdv = r(mean)
cap erase "$datatemp/bsfalse.dta"

{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnamrch `X' [aw=popch] if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnamrch `X' [aw=popch] if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsfalse
	postfile bsfalse wild_t using "$datatemp/bsfalse.dta", replace

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
			qui areg ywild `X' [aw=popch] if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsfalse (`wild_t')
		restore
	}
	postclose bsfalse
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsfalse",clear
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

xi: reg lnamrch `X' if ~white [aw=popch], cluster(stfips)
outreg2 using "$output/table8.xls", `r' keep(HCPOST WHCPOST) noparen noaster ctitle("amrch") addstat(bsp, `bsp', mdv, `mdv')
local r append

*NEONATAL MORTALITY
qui sum nmr if exp==-1 & hc & ~white [aw=births]
local mdv = r(mean)
cap erase "$datatemp/bsfalse.dta"
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnnmr `X' [aw=births] if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnnmr `X' [aw=births] if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsfalse
	postfile bsfalse wild_t using "$datatemp/bsfalse.dta", replace

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
			qui areg ywild `X' [aw=births] if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsfalse (`wild_t')
		restore
	}
	postclose bsfalse
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsfalse",clear
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

xi: reg lnnmr `X' if ~white [aw=births], cluster(stfips)
outreg2 using "$output/table8.xls", `r' keep(HCPOST WHCPOST) noparen noaster ctitle("nmr") addstat(bsp, `bsp', mdv, `mdv')


*YOUNG CHILD ASMR
qui sum asmr14 if exp==-1 & hc & ~white [aw=pop14]
local mdv = r(mean)
cap erase "$datatemp/bsfalse.dta"

{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr14 `X' [aw=pop14] if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr14 `X' [aw=pop14] if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsfalse
	postfile bsfalse wild_t using "$datatemp/bsfalse.dta", replace

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
			qui areg ywild `X' [aw=pop14] if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsfalse (`wild_t')
		restore
	}
	postclose bsfalse
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsfalse",clear
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

xi: reg lnasmr14 `X' if ~white [aw=pop14], cluster(stfips)
outreg2 using "$output/table8.xls", `r' keep(HCPOST WHCPOST) noparen noaster ctitle("asmr14") addstat(bsp, `bsp', mdv, `mdv')


/*****************************/
/**Adding Nonwhite-AFDC Rate**/
/*****************************/
gen crate66 = crate*(year>=1966)
local X = "S_* RXY* YMXY* pcinc hpc bpc crate crate66 HC_Texp_1 HC_Texp_9 HCPOST HC_Texp_19"

*CHILD AMR
qui sum amrch if exp==-1 & hc & ~white [aw=popch]
local mdv = r(mean)

cap erase "$datatemp/bsfalse.dta"

{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnamrch `X' [aw=popch] if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnamrch `X' [aw=popch] if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsfalse
	postfile bsfalse wild_t using "$datatemp/bsfalse.dta", replace

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
			qui areg ywild `X' [aw=popch] if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsfalse (`wild_t')
		restore
	}
	postclose bsfalse
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsfalse",clear
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

xi: reg lnamrch `X' if ~white [aw=popch], cluster(stfips)
outreg2 using "$output/table8.xls", `r' keep(HCPOST) noparen noaster ctitle("amrch") addstat(bsp, `bsp', mdv, `mdv')

*NEONATAL MORTALITY
qui sum nmr if exp==-1 & hc & ~white [aw=births]
local mdv = r(mean)

cap erase "$datatemp/bsfalse.dta"
{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnnmr `X' [aw=births] if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnnmr `X' [aw=births] if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsfalse
	postfile bsfalse wild_t using "$datatemp/bsfalse.dta", replace

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
			qui areg ywild `X' [aw=births] if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsfalse (`wild_t')
		restore
	}
	postclose bsfalse
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsfalse",clear
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


xi: reg lnnmr `X' if ~white [aw=births], cluster(stfips)
outreg2 using "$output/table8.xls", `r' keep(HCPOST) noparen noaster ctitle("nmr") addstat(bsp, `bsp', mdv, `mdv')

*POST-NEONATAL MORTALITY
*qui sum pnmr if exp==-1 & hc & ~white [aw=births]
*local mdv = r(mean)
*xi: reg lnpnmr `X' if ~white [aw=births], cluster(stfips)
*outreg2 using "$output/table8.xls", `r' keep(HCPOST) noparen noaster ctitle("pnmr") addstat(bsp, `bsp', mdv, `mdv')

*YOUNG CHILD ASMR
qui sum asmr14 if exp==-1 & hc & ~white [aw=pop14]
local mdv = r(mean)
cap erase "$datatemp/bsfalse.dta"

{
	*get wild percentile-t bootstrap distribution and calculate the p-value: pass the model spec `i'
	/*************/
	cap drop e XB0
	*run actual model and store 
	qui areg lnasmr14 `X' [aw=pop14] if ~white, a(stfips) cluster(stfips)
	local t = _b[HCPOST]/_se[HCPOST]

	*residuals to resample
	replace HCPOST = 0
	qui areg lnasmr14 `X' [aw=pop14] if ~white, a(stfips) cluster(stfips)
	predict e, resid
	*fitted values imposing H_0 for wild-t: HCPOST==0
	predict XB0, xb
	replace HCPOST = crate0*POST

	cap postclose bsfalse
	postfile bsfalse wild_t using "$datatemp/bsfalse.dta", replace

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
			qui areg ywild `X' [aw=pop14] if ~white, a(stfips) cluster(stfips)
			local wild_t = (_b[HCPOST] - 0)/_se[HCPOST]
			qui post bsfalse (`wild_t')
		restore
	}
	postclose bsfalse
	
	*get p-value by calculating the percentile of the new bootstrap t-dist
	preserve
		use "$datatemp/bsfalse",clear
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
xi: reg lnasmr14 `X' if ~white [aw=pop14], cluster(stfips)
outreg2 using "$output/table8.xls", `r' keep(HCPOST) noparen noaster ctitle("asmr14") addstat(bsp, `bsp', mdv, `mdv')
exit
	
