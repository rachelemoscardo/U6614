clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2E2_sr.xls", clear  cells(A3:B65) first
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach stat in dd ddse pre post ddtest wlstest{
	sum sr if VARIABLES=="`stat'"
	local sr`stat' : display %03.2f r(mean)*100
	
}

	
drop if _n>=51
cap destring VARIABLES, replace
ren VARIABLES exp
gen b = exp<.
replace exp = exp-18
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
for var sr: ren X X_
reshape wide *sr_, i(exp) j(b)

for var *_0 *_1: replace X = X*100

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
**sr/vsr Kids
	cap drop lb* ub*
	local a = 1
	gen lblbw = sr_1 - 1.96*sr_0 if exp~=-1
	gen ublbw = sr_1 + 1.96*sr_0 if exp~=-1

	gen srdd 	= `srdd' if exp>=1 & exp<=9
	
	#delimit ;
	twoway (scatter sr_1 lblbw ublbw srdd exp, 
			lpattern(solid dot dot solid solid) 
			lcolor(purple purple purple magenta) 
			lwidth(thick medium medium thick thick)
			msymbol(Oh i i i i i i) 
			mcolor(purple purple purple magenta) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-.6(.3).6, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("Sex Ratio x 100", size(medium))
			title("", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(-.55 4 "DD Estimate: `srdd' (s.e. = `srddse')" , j(left) size(small) box fcolor(white) margin(small))
			)
			(pcarrowi .53 -3.8 .5 -1.1, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(.55 -7.1 "Year Before Medicaid", j(left) size(medium)));
			;
	#delimit cr;	
	
graph display, 	xsize(7.5) ysize(5.5) 
graph export "$output/figureA2E2_sr.emf", replace

