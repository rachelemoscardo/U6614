use "$data/vs_mcaid" if inrange(year,1979,1988), clear
xi i.year, noomit
for var _I*: gen CRX = crate0*X
gen cy = crate0*year

xi: reg medelig i.year crate0 cy if ~white & year<=1988 [aw=popch], cluster(stfips)
local b = _b[cy]
local bse = _se[cy]
xi: reg medelig i.year CR* if ~white & year<=1988 [aw=popch], cluster(stfips)
outreg2 using "$output/figureA2C1_B_simelig.xls", replace keep(CR*) noparen noaster addstat(b, `b', se, `bse')

xi: reg simelig i.year crate0 cy if ~white & year<=1988 [aw=popch], cluster(stfips)
local b = _b[cy]
local bse = _se[cy]
xi: reg simelig i.year CR* if ~white & year<=1988 [aw=popch], cluster(stfips)
outreg2 using "$output/figureA2C1_B_simelig.xls", append keep(CR*) noparen noaster addstat(b, `b', se, `bse')

