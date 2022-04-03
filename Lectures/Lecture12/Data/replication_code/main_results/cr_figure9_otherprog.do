clear 
clear matrix
pause on

xmluse  "$output/figure9B.xls", clear  cells(A3:E29) first
drop in 1
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach stat in dd ddse{
	sum rmvp_wls if VARIABLES=="`stat'"
	local rmvp`stat' : display %03.2f r(mean)*100

	sum pcmvp_wls if VARIABLES=="`stat'"
	local pcmvp`stat' : display %06.0fc r(mean)	
}

drop if _n>=19
quietly destring _all, replace ignore(",")

ren VARIABLES exp
gen b = exp<.
replace exp = exp-5
local obs = _N
forval i = 2/`obs'{
	local j = `i'-1
	replace exp = exp[`j'] in `i' if exp[`i']==.
}

local obs =_N+1
set obs `obs'
for var _all: replace X = 0 in `obs'
replace b = 1 in `obs'
replace exp = -1 in `obs'
for var *mvp*: ren X X_
reshape wide *mvp*_, i(exp) j(b)
save "$datatemp/f9temp", replace


clear 
clear matrix
pause on
xmluse  "$output/figure9.xls", clear  cells(A3:M46) first
drop in 1
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
destring _all, replace ignore(",")

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach stat in dd ddse pre post{
	sum pcrfund_hs_wls if VARIABLES=="`stat'"
	local hs`stat' : display %6.0fc round(r(mean))
	
	sum pcrfund_health_wls if VARIABLES=="`stat'"
	local health`stat' : display %6.0fc round(r(mean))

	sum pcrfund_chc_wls if VARIABLES=="`stat'"
	local chc`stat' : display %6.0fc round(r(mean))

	sum pccase_fs_wls if VARIABLES=="`stat'"
	local fs`stat' : display %03.2f r(mean)*100

	sum wcrate_wls if VARIABLES=="`stat'"
	local wcrate`stat' : display %03.2f r(mean)*100

	sum nwcrate_wls if VARIABLES=="`stat'"
	local nwcrate`stat' : display %03.2f r(mean)*100
}

local obs = _N
drop in 33/`obs'
cap destring VARIABLES, replace

ren VARIABLES exp
gen b = exp<.
replace exp = exp-9
local obs = _N
forval i = 2/`obs'{
	local j = `i'-1
	replace exp = exp[`j'] in `i' if exp[`i']==.
}

local obs =_N+1
set obs `obs'
for var _all: replace X = 0 in `obs'
replace b = 1 in `obs'
replace exp = -1 in `obs'
for var *wls: ren X X_
drop *ols
reshape wide *wls_, i(exp) j(b)

merge 1:1 exp using "$datatemp/f9temp"



cap drop ub* lb*
gen ubm = pcmvp_wls_1 + 1.96*pcmvp_wls_0
gen lbm = pcmvp_wls_1 - 1.96*pcmvp_wls_0


/**FUNDING PANEL - HS, CHC, OTHER CAP HEALTH**/
#delimit ;
scatter pcmvp_wls_1 pcrfund_hs_wls_1 pcrfund_health_wls_1 pcrfund_chc_wls_1 ubm lbm exp if exp>=-7 & exp<=9, 
		msymbol(T Oh Sh i i i i)									
		mcolor(gray forest_green maroon dkorange)
		msize(large large large large )								
		lpattern(solid solid solid solid dash dash dot dot)
		lwidth(medthick thick thick thick medium medium medium medium)
		lcolor(gray forest_green maroon dkorange gray gray)
		cmissing(n n n n n n)
		connect(l l l l l l l)									
		legend(order(1 2 3 4) rows(2) label(1 "Public Insurance (w/ Medicaid)" "per 1,000 Children 0-19") label(2 "Head Start per 1,000" "Children 1-9") label(3 "CAP Health") label(4 "Community" "Health Centers") size(medsmall) region(style(none)))
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-7(3)9, labsize(medium))
		ylabel(-25000 0 25000, labsize(medium))
		xtitle("Years since Medicaid Implementation", size(medium))
		ytitle("Real Expenditures ($2012)", size(medsmall))
		title("{it: A. Expenditures per 1,000 Residents}", size(medium) color(black))
		text(40000 -3.9 "DD Estimates:"  "Head Start: `hsdd' (s.e. = `hsddse')" "CAP Health: `healthdd' (s.e. = `healthddse')" "CHC: `chcdd' (s.e. = `chcddse')" "Medicaid: `pcmvpdd' (s.e. = `pcmvpddse')", j(left) size(medsmall) box fcolor(white) margin(small))			
		graphregion(fcolor(white) color(white) icolor(white) margin(zero)) saving("$output/panela.gph", replace) 
		;
	#delimit cr;	

	
/*Caseload PANEL - **/
for var rmvp* pccase* nwcrate* wcrate*: replace X = X*100
cap drop ub* lb*
gen ubm = rmvp_wls_1 + 1.96*rmvp_wls_0
gen lbm = rmvp_wls_1 - 1.96*rmvp_wls_0


	#delimit ;
	twoway scatter rmvp_wls_1 pccase_fs_wls_1 nwcrate_wls_1 wcrate_wls_1 ubm lbm exp if exp>=-7 & exp<=9, 
		msymbol(T Oh Sh i i i i)									
		mcolor(gray red blue green)
		msize(large large large large )								
		lpattern(solid solid solid solid dash dash dot dot)
		lwidth(medthick thick thick thick medium medium medium medium)
		lcolor(gray red blue green gray gray)
		cmissing(n n n n n n)
		connect(l l l l l l l)										
		legend(order(1 2 3 4) rows(2) label(1 "Public Insurance (incl." "Medicaid) per Child") label(2 "Food Stamp Cases" "per Resident") label(3 "Nonwhite AFDC Rate") label(4 "White AFDC Rate") region(style(none)))
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-7(3)9, labsize(medium))
		ylabel(-5(5)5, labsize(medium))
		xtitle("Years Since Medicaid Implementation", size(medium))
		ytitle("Per-Capita Cases x 100", size(medium))
		title("{it: B. Per-Capita Cases}", size(medium) color(black))
		text(5.5 -3.3 "DD Estimates:"  "Food Stamps: `fsdd' (s.e. = `fsddse')" "Medicaid: `rmvpdd' (s.e. = `rmvpddse')" "Nonwhite AFDC: `nwcratedd' (s.e. = `nwcrateddse')" "White AFDC: `wcratedd' (s.e. = `wcrateddse')" , j(left) size(medsmall) box fcolor(white) margin(small))			
		graphregion(fcolor(white) color(white) icolor(white))  saving("$output/panelb.gph", replace);
		
	#delimit cr;		
#delimit ;
graph combine
"$output/panela.gph" "$output/panelb.gph",
col(1) row(2)
imargin(tiny) 
graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 
;
#delimit cr;

graph display, 	xsize(5.93) ysize(8.1)
graph export "$output/figure9_otherprog.emf", replace


erase "$output/panela.gph" 
erase "$output/panelb.gph"
exit
