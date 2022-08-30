*read in analysis dataset
use "$data/vs_mcaid" if inrange(year,1950,1979) & ~white, clear

gen low_black_share = inlist(stfips,2,3,16,340,35,38,46,49,53,56)
label var low_black_share "States where <50% of nonwhite children are black"

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

*create treatment variables
char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X
gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

*fixed effects
egen rxy = group(region year)
egen ymxy = group(ymcaid year)
egen str = group(stfips white)
egen stxy = group(stfips year)

xi i.region*i.year, pref(RXY)
drop RXYregion*
xi i.stfips, pref(S_)
xi i.stfips*year, pref(SY_)
drop SY_stfips*
xi i.str i.white*i.rxy i.white*i.ymxy i.white*pcinc i.white*hpc i.white*bpc, pref(wh_)


xi i.ymcaid*i.year, pref(YMXY)
drop YMXYyear* YMXYymcaid* YMXYymcXyea_1982* YMXYymcXyea_1972*
for var HC*: replace X = 0 if stfips==4

*generate and summarize the (log) outcome variable
gen lnamrch 	= ln(amrch)
gen lndmr1 		= ln(dmr1)
gen lnnmr 		= ln(nmr)
gen lnpnmr 		= ln(pnmr)
gen lnasmr14	= ln(asmr14)
 
local X = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=births]"

local r replace
foreach var of varlist lndmr1 lnnmr lnpnmr{
*baseline
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15), a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'")
local r append

*add arizona
areg `var' `X`i'' if ~white & ~inlist(stfips,2,15), a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_AZ")

*No Deep South
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15,1,13,22,28,37), a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_DS")

*No South
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15) & region~=4, a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_S")

*No South OR Border
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15) & region~=3 & region~=4, a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_S_B")

*No Low Black-Share States
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15) & ~low_black_share, a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_LBS")

*No Early Abortion States
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15,6,36,53), a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_EAS")
}



local X = "YMXY* RXY* pcinc hpc bpc HC_Texp_1 HC_Texp_18 HCPOST HC_Texp_28 [aw=popch]"

foreach var of varlist lnamrch lnasmr14{
*baseline
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15), a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'")

*add arizona
areg `var' `X`i'' if ~white & ~inlist(stfips,2,15), a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_AZ")

*No Deep South
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15,1,13,22,28,37), a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_DS")

*No South
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15) & region~=4, a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_S")

*No South OR Border
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15) & region~=3 & region~=4, a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_S_B")

*No Low Black-Share States
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15) & ~low_black_share, a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_LBS")

*No Early Abortion States
areg `var' `X`i'' if ~white & ~inlist(stfips,2,4,15,6,36,53), a(stfips) cluster(stfips)
outreg2 using "$output/tableA2D1_alt_samples.xls", `r' keep(HCPOST) noparen noaster ctitle("`var'_NO_EAS")
}



















