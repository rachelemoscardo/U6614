use $data/balance, clear
drop if year>1967

char year[omit] 1965
xi i.year
gen cy = crate0*(year-1965)

local r replace

***********************
**VITAL STATS OUTCOMES*
***********************
foreach var of varlist amrch imr vlbwr lbwr{
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
	outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0 cy) ctitle("nw `var'") addstat(mdv, `mdv')
	local r append
	
	*WHITE
	sum `var' if white==1 & year==1965 [aw=wt]
	local mdv = r(mean) 
	reg `var' _I* cy crate0 if white==1 [aw=wt], cluster(stfips)
	outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0 cy) ctitle("w `var'") addstat(mdv, `mdv')
	
}

*************************
*****Census outcomes*****
*************************
replace cy = crate0*(year-1960)
for var dpov smh g12: replace X = X*100
foreach var of varlist dpov smh earn g12{
	*NONWHITE
	sum `var' if white==0 & year==1960 [aw=poptot]
	local mdv = r(mean) 
	reg `var' _Iyear_1950 cy crate0 if white==0 [aw=cenwt], cluster(stfips)
	outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0 cy) ctitle("nw `var'") addstat(mdv, `mdv')

	*WHITE
	sum `var' if white==1 & year==1960 [aw=poptot]
	local mdv = r(mean) 
	reg `var' _Iyear_1950 cy crate0 if white==1 [aw=cenwt], cluster(stfips)
	outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0 cy) ctitle("w `var'") addstat(mdv, `mdv')	
	
}


***********************
**AFDC OUTCOMES*
***********************
*NONWHITE
sum afdcpay if white==0 [aw=afdcwt]
local mdv = r(mean) 
reg afdcpay crate0 if white==0 [aw=afdcwt], cluster(stfips)
outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0) ctitle("nw afdcpay") addstat(mdv, `mdv')

*WHITE
sum afdcpay if white==1 [aw=afdcwt]
local mdv = r(mean) 
reg afdcpay crate0 if white==1 [aw=afdcwt], cluster(stfips)
outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0) ctitle("w afdcpay") addstat(mdv, `mdv')



***************************
**not race-specific
***************************
replace cy = crate0*(year - 1962)
gen lng = ln(gexp*1000/gpop)
local var lng
	sum `var' if white==0 & year==1962 [aw=gpop]
	local mdv = r(mean) 
	reg `var' _Iyear* cy crate0 if white==0 [aw=gpop], cluster(stfips)
	outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0 cy) ctitle("nw `var'") addstat(mdv, `mdv')
	
	sum `var' if white==1 & year==1962 [aw=gpop]
	local mdv = r(mean)
	reg `var' _Iyear* cy crate0 if white==1 [aw=gpop], cluster(stfips)
	outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0 cy) ctitle("w `var'") addstat(mdv, `mdv')	

	
for var ih im: replace X = X*1000
foreach var of varlist bpc ih im{
	sum `var' if white==0 & year==1965 [aw=poptot]
	local mdv = r(mean) 
	reg `var' _Iyear* cy crate0 if white==0 [aw=poptot], cluster(stfips)
	outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0 cy) ctitle("nw `var'") addstat(mdv, `mdv')
	
	sum `var' if white==1 & year==1965 [aw=poptot]
	local mdv = r(mean)
	reg `var' _Iyear* cy crate0 if white==1 [aw=poptot], cluster(stfips)
	outreg2 using "$output/table1.xls", `r' noparen noaster keep(crate0 cy) ctitle("w `var'") addstat(mdv, `mdv')	
}



exit


