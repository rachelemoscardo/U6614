global breps 10
global q 5
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



/*************************************/
/**		CONTROLLING FOR PNMR VARS 	**/
/*************************************/
local X = "YMXY* RXY* pcinc hpc bpc lnpnmr HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"

cap drop W* 
qui gen W = 1/births
qui for var S_* YMXY* RXY* pcinc hpc bpc lnpnmr HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28: gen WX = W*X
drop W

local r replace

****************
*FIRST DAY IMR*
****************
qui sum dmr1 if exp==-1 [aw=births]
local mdv = r(mean)
areg lndmr1 `X' [aw=births], a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D6_pnmr_control.xls", `r' keep(HCPOST) noparen noaster ctitle("dmr1, pnmr") addstat(mdv, `mdv')
local r append

	
	
**************
*NEONATAL IMR*
**************
qui sum nmr if exp==-1 [aw=births]
local mdv = r(mean)
*Run Model
areg lnnmr `X' [aw=births], a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D6_pnmr_control.xls", `r' keep(HCPOST) noparen noaster ctitle("nmr, pnmr") addstat(mdv, `mdv')
local r append

	
	

/*************************************/
/**		CONTROLLING FOR BW*YEAR VARS 	**/
/*************************************/
xi i.year*lbw i.year*vlbwr 
local X = "YMXY* RXY* pcinc hpc bpc _I* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"

****************
*FIRST DAY IMR*
****************
qui sum dmr1 if exp==-1 [aw=births]
local mdv = r(mean)
areg lndmr1 `X' [aw=births], a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D6_pnmr_control.xls", `r' keep(HCPOST) noparen noaster ctitle("dmr1, bw*year") addstat(mdv, `mdv')
local r append

	
	
**************
*NEONATAL IMR*
**************
qui sum nmr if exp==-1 [aw=births]
local mdv = r(mean)
areg lnnmr `X' [aw=births], a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D6_pnmr_control.xls", `r' keep(HCPOST) noparen noaster ctitle("nmr, bw*year") addstat(mdv, `mdv')
local r append
	

	
/*************************************/
/**		CONTROLLING FOR BW*YEAR VARS 	**/
/*************************************/
xi i.region*lbw i.region*vlbwr 
local X = "YMXY* RXY* pcinc hpc bpc _I* HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28"

****************
*FIRST DAY IMR*
****************
qui sum dmr1 if exp==-1 [aw=births]
local mdv = r(mean)
areg lndmr1 `X' [aw=births], a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D6_pnmr_control.xls", `r' keep(HCPOST) noparen noaster ctitle("dmr1, bw*region") addstat(mdv, `mdv')
local r append

	
	
**************
*NEONATAL IMR*
**************
qui sum nmr if exp==-1 [aw=births]
local mdv = r(mean)
areg lnnmr `X' [aw=births], a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D6_pnmr_control.xls", `r' keep(HCPOST) noparen noaster ctitle("nmr, bw*region") addstat(mdv, `mdv')
local r append
	
