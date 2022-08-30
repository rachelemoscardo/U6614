**Run regressions from figure VI - the main age-adjusted child mortality regressions
use "$data/vs_mcaid" if inrange(year,1959,1979), clear

drop if inlist(stfips,2,4,15)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X

*for var nmr pnmr asmr14 asmr514: gen lnX = ln(X)
gen dmmr = mmr>0 if mmr<.

local X = "i.stfips i.year*i.region i.ymcaid*i.year pcinc hpc bpc HC_Texp*"

local r replace
xi: reg mmr `X' [aw=births] if ~white, cluster(stfips)
outreg2 using "$output/figureA2C6_mmr.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("mmr")
local r append

xi: reg dmmr `X' [aw=births] if ~white, cluster(stfips)
outreg2 using "$output/figureA2C6_mmr.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("dmr")

for var _T*: replace HCX = hc*X

xi: reg mmr `X' [aw=births] if ~white, cluster(stfips)
outreg2 using "$output/figureA2C6_mmr.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("mmr_b")
local r append

xi: reg dmmr `X' [aw=births] if ~white, cluster(stfips)
outreg2 using "$output/figureA2C6_mmr.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("dmr_b")

xi: reg mmr `X' [aw=births] if ~white & region~=5, cluster(stfips)
outreg2 using "$output/figureA2C6_mmr.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("mmr_b_no_w")
local r append

xi: reg dmmr `X' [aw=births] if ~white & region~=5, cluster(stfips)
outreg2 using "$output/figureA2C6_mmr.xls", `r' keep(HC_Texp_2-HC_Texp_18) noparen noaster ctitle("dmr_b_no_w")
