**Run regressions from figure VI - the main age-adjusted child mortality regressions
use "$data/vs_mcaid" if inrange(year,1950,1979), clear
drop if inlist(stfips,2,4,15)

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X

*for var nmr pnmr asmr14 asmr514: gen lnX = ln(X)
gen nmr1 = nmr-dmr1
gen lndmr1		= ln(dmr1)
gen lnnmr1 		= ln(nmr1)
*gen lnnmr 		= ln(nmr)
gen lnpnmr 		= ln(pnmr)
*gen lnimr		= ln(imr)

for var asmr14 asmr59 asmr1014: gen lnX = ln(X)


cap drop W* 
qui gen W = 1/births
qui xi i.stfips i.year*i.region i.year*i.ymcaid
qui for var _I* pcinc hpc bpc HC_Texp*: gen WX = W*X
drop W

gen pretrend = exp*crate0*inrange(exp,-16,0)
xi: reg lndmr1 i.stfips i.year*i.region i.ymcaid*i.year pcinc hpc bpc HC_Texp_1 pretrend HC_Texp_18-HC_Texp_28 [aw=births] if ~white, cluster(stfips)
xi: reg lnnmr1 i.stfips i.year*i.region i.ymcaid*i.year pcinc hpc bpc HC_Texp_1 pretrend HC_Texp_18-HC_Texp_28 [aw=births] if ~white, cluster(stfips)
xi: reg lnpnmr i.stfips i.year*i.region i.ymcaid*i.year pcinc hpc bpc HC_Texp_1 pretrend HC_Texp_18-HC_Texp_28 [aw=births] if ~white, cluster(stfips)

local X = "i.stfips i.year*i.region i.ymcaid*i.year pcinc hpc bpc HC_Texp*"

local r replace
xi: reg lndmr1 `X' [aw=births] if ~white, cluster(stfips)
outreg2 using "$output/figure7.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("dmr")
local r append

xi: reg lnnmr1 `X' [aw=births] if ~white, cluster(stfips)
outreg2 using "$output/figure7.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("nmrx")

xi: reg lnpnmr `X' [aw=births] if ~white, cluster(stfips)
outreg2 using "$output/figure7.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("pnmr")

xi: reg lnasmr14 `X' [aw=pop14] if ~white, cluster(stfips)
outreg2 using "$output/figure7.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("asmr14")

xi: reg lnasmr59 `X' [aw=pop59] if ~white, cluster(stfips)
outreg2 using "$output/figure7.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("asmr59")

xi: reg lnasmr1014 `X' [aw=pop1014] if ~white, cluster(stfips)
outreg2 using "$output/figure7.xls", `r' keep(HC_Texp_2-HC_Texp_27) noparen noaster ctitle("asmr1014")



exit


