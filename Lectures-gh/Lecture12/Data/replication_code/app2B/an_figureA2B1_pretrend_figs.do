use "$data/balance", clear
drop if year>1967

drop if inlist(stfips,2,4,15)

gen lnamrch = ln(amrch)
gen lnimr = ln(imr)

*get 1965 slope in levels and p-value on test that the year interactions are diff from zero (common trends)
*char year[omit] 1965
xi i.year, noomit
for var _I*: gen CX = crate0*X

local r replace

gen cy = crate0*(year-1965)


***********************
**VITAL STATS OUTCOMES*
***********************
for var ih is im: replace X = X*100

foreach var of varlist amrch imr lbwr bpc hpc ih is im{
	cap drop wt
	if "`var'"=="amrch"{
		gen wt = popch
	}
	if "`var'"~="amrch"{
		gen wt = births
	}	
	*NONWHITE
	sum `var' if white==0 & year==1965 [aw=wt]
	local mdv = r(mean) 
	reg `var' _I* cy crate0 if white==0 [aw=wt], cluster(stfips)
	local l = _b[cy]
	local lse = _se[cy]	
	reg `var' _I* C* crate if white==0 [aw=wt], cluster(stfips)
	testparm C*
	local F = r(F)
	local p = r(p)
	reg `var' _I* C* if white==0 [aw=wt], nocon cluster(stfips)
	outreg2 using "$output/figureA2B1_pretrend_figs.xls", `r' noparen noaster keep(C*) ctitle("nw `var'") addstat(mdv, `mdv', F, `F', p, `p', l ,`l', lse, `lse')
	local r append
	
	*WHITE
	sum `var' if white==1 & year==1965 [aw=wt]
	local mdv = r(mean) 
	reg `var' _I* cy crate0 if white==1 [aw=wt], cluster(stfips)
	local l = _b[cy]
	local lse = _se[cy]	
	reg `var' _I* C* crate if white==1 [aw=wt], cluster(stfips)
	testparm C*
	local F = r(F)
	local p = r(p)
	reg `var' _I* C* if white==1 [aw=wt], nocon cluster(stfips)		
	outreg2 using "$output/figureA2B1_pretrend_figs.xls", `r' noparen noaster keep(C*) ctitle("w `var'") addstat(mdv, `mdv', F, `F', p, `p', l ,`l', lse, `lse')
}


exit

***************************
**not race-specific
***************************
for var ih is im: replace X = X*100
foreach var of varlist bpc hpc ih is im /*rmvp_ch*/{
	sum `var' if white==0 & year==1965 [aw=poptot]
	local mdv = r(mean) 
	reg `var' _I* C* if white==0 [aw=poptot], nocon cluster(stfips)
	testparm C*
	local F = r(F)
	local p = r(p)
	*local l = _b[crate0]
	*local lse = _se[crate0]	
	outreg2 using "$output/figureA2B1_pretrend_figs.xls", `r' noparen noaster keep(C*) ctitle("nw `var'") addstat(mdv, `mdv', F, `F', p, `p', l ,`l', lse, `lse')
	local r append
	
	sum `var' if white==1 & year==1965 [aw=poptot]
	local mdv = r(mean)
	reg `var' _I* C* if white==1 [aw=poptot], nocon cluster(stfips)
	testparm C*
	local F = r(F)
	local p = r(p)
	*local l = _b[crate0]
	*local lse = _se[crate0]	
	outreg2 using "$output/figureA2B1_pretrend_figs.xls", `r' noparen noaster keep(C*) ctitle("w `var'") addstat(mdv, `mdv', F, `F', p, `p', l ,`l', lse, `lse')	
}


drop _I* C*
char year[omit] 1962
xi i.year
for var _I*: gen CX = crate0*X

gen lng = ln(gexp*1000/gpop)
local var lng
	sum `var' if white==0 & year==1962 [aw=gpop]
	local mdv = r(mean) 
	reg `var' _I* C* if white==0 [aw=gpop], nocon cluster(stfips)
	testparm C*
	local F = r(F)
	local p = r(p)
	*local l = _b[crate0]
	*local lse = _se[crate0]	
	outreg2 using "$output/figureA2B1_pretrend_figs.xls", `r' noparen noaster keep(C*) ctitle("nw `var'") addstat(mdv, `mdv', F, `F', p, `p', l ,`l', lse, `lse')
	local r append
	
	sum `var' if white==1 & year==1962 [aw=gpop]
	local mdv = r(mean)
	reg `var' _I* C* if white==1 [aw=gpop], nocon cluster(stfips)
	testparm C*
	local F = r(F)
	local p = r(p)
	*local l = _b[crate0]
	*local lse = _se[crate0]	
	outreg2 using "$output/figureA2B1_pretrend_figs.xls", `r' noparen noaster keep(C*) ctitle("w `var'") addstat(mdv, `mdv', F, `F', p, `p', l ,`l', lse, `lse')	


