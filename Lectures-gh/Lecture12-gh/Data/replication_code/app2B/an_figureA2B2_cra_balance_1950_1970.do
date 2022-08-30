use $data/balance, clear
keep if inlist(year,1950,1960,1970)

drop if inlist(stfips,2,4,15)

*get 1965 slope in levels and p-value on test that the year interactions are diff from zero (common tren
xi i.year, noomit
for var _Iyear*: gen CX = crate0*X
gen cy = crate0*(year-1960)

local r replace


*************************
*****Census outcomes*****
*************************
local r replace
for var dpov smh g12: replace X = X*100
foreach var of varlist dpov smh earn g12{
	*NONWHITE
	sum `var' if white==0 & year==1960 [aw=poptot]
	local mdv = r(mean) 
	reg `var' _Iyear_1950 _Iyear_1970 cy crate0 if white==0 [aw=cenwt], cluster(stfips)
	local l = _b[cy]
	local lse = _se[cy]
	reg `var' _I* C_I* if white==0 [aw=cenwt], cluster(stfips)
	outreg2 using "$output/figureA2B2_cra_balance_1950_1970.xls", `r' noparen noaster keep(C_I*) ctitle("nw `var'") addstat(mdv, `mdv', l, `l', lse, `lse')
	local r append
	
	*WHITE
	sum `var' if white==1 & year==1960 [aw=poptot]
	local mdv = r(mean) 
	reg `var' _Iyear_1950 _Iyear_1970 cy crate0 if white==1 [aw=cenwt], cluster(stfips)
	local l = _b[cy]
	local lse = _se[cy]
	reg `var' _I* C_I*  if white==1 [aw=cenwt], cluster(stfips)
	outreg2 using "$output/figureA2B2_cra_balance_1950_1970.xls", `r' noparen noaster keep(C_I*) ctitle("w `var'") addstat(mdv, `mdv', l, `l', lse, `lse')
}

foreach var of varlist er gr{
	*NONWHITE
	sum `var' if white==0 & year==1960 [aw=poptot]
	local mdv = r(mean) 
	reg `var' _Iyear_1950 _Iyear_1970 cy crate0 if white==0 [aw=cenwt], cluster(stfips)
	local l = _b[cy]
	local lse = _se[cy]	
	reg `var' _I* C_I*  if white==0 [aw=cenwt], cluster(stfips)
	outreg2 using "$output/figureA2B2_cra_balance_1950_1970.xls", `r' noparen noaster keep(C_I*) ctitle("nw `var'") addstat(mdv, `mdv', l, `l', lse, `lse')
}


exit



