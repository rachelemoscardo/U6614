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
local X = "YMXY* RXY* pcinc hpc bpc HC_Texp*"
local r replace

areg lnamrch_int `X' [aw=popch], a(stfips) cluster(stfips)
outreg2 using "$output/figureA2C7_int_ext.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("armch_int") 
local r append

areg lnamrch_ext `X' [aw=popch], a(stfips) cluster(stfips)
outreg2 using "$output/figureA2C7_int_ext.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("armch_ext") 

