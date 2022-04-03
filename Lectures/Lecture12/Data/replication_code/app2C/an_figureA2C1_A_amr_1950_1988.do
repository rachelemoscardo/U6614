**Run regressions from figure VI - the main age-adjusted child mortality regressions
use "$data/vs_mcaid" if inrange(year,1950,1988), clear

drop if inlist(stfips,2,4,15)
*drop if stfips==34
*drop if (stfips==23|stfips==33|stfips==50) & ~white
drop if stfips==54 & white

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (19/100=19)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X

gen lnamrch = ln(amrch)

local X = "i.stfips i.year*i.region i.year*i.ymcaid pcinc hpc bpc HC_Texp*"
local r replace


/****************************/
/**NONWHITE CHILD AMR, 0-14**/
/****************************/
*Run ES Model 
	xi: reg lnamrch `X' [aw=popch] if ~white, cluster(stfips)
outreg2 using "$output/figureA2C1_A_amr_1950_1988.xls", `r' keep(HC_Texp_2-HC_Texp_36) noparen noaster ctitle("Nonwhite")
local r append

/*************************/
/**WHITE CHILD AMR, 0-14**/
/*************************/
*Run ES Model 
	xi: reg lnamrch `X' [aw=popch] if white, cluster(stfips)
outreg2 using "$output/figureA2C1_A_amr_1950_1988.xls", `r' keep(HC_Texp_2-HC_Texp_36) noparen noaster ctitle("White")
local r append



