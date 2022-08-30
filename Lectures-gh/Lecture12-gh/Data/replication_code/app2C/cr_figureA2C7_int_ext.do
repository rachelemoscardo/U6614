clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2C7_int_ext.xls", clear  cells(A3:C54) first
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress

	
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
for var armch_int armch_ext: ren X X_
reshape wide *_, i(exp) j(b)

for var *_0 *_1: replace X = X*100

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
	cap drop lb* ub*
	local a = 1
	gen lbi = armch_int_1 - 1.96*armch_int_0 if exp~=-1
	gen ubi = armch_int_1 + 1.96*armch_int_0 if exp~=-1

	gen lbe = armch_ext_1 - 1.96*armch_ext_0 if exp~=-1
	gen ube = armch_ext_1 + 1.96*armch_ext_0 if exp~=-1
	
	 
	#delimit ;
	twoway (scatter armch_ext_1 armch_int_1 ube lbe lbi ubi exp, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon red blue) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Oh D i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon red blue) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(, labsize(medium))
			legend(off)
			xsize(7.5) ysize(5.5) 			
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("log Mortality Rate x 100", size(medium))
			title("", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			)
			(pcarrowi -5 -4 -2.1 3, text(-5 -6.4 "Internal Causes") lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))
			(pcarrowi 3 4 -.3 3, text(3.4 4 "External Causes") lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))			
			;
	#delimit cr;	
	
graph display, 	xsize(7.5) ysize(5.5) 
graph export "$output/figureA2C7_int_ext.emf", replace

