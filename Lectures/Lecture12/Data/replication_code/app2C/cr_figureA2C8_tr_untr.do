clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2C8_tr_untr.xls", clear  cells(A3:C36) first
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress

	
drop if _n>=51
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
for var armch_tr armch_untr: ren X X_
reshape wide *_, i(exp) j(b)

for var *_0 *_1: replace X = X*100

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
	cap drop lb* ub*
	local a = 1
	gen lbi = armch_tr_1 - 1.96*armch_tr_0 if exp~=-1
	gen ubi = armch_tr_1 + 1.96*armch_tr_0 if exp~=-1

	gen lbe = armch_untr_1 - 1.96*armch_untr_0 if exp~=-1
	gen ube = armch_untr_1 + 1.96*armch_untr_0 if exp~=-1
	
	 
	#delimit ;
	twoway (scatter armch_untr_1 armch_tr_1 ube lbe lbi ubi exp, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon red blue) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Th S i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon red blue) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-7(3)9, labsize(medium))
			ylabel(, labsize(medium))
			legend(off)
			xsize(7.5) ysize(5.5) 			
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("log Mortality Rate x 100", size(medium))
			title("", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) )
			(pcarrowi -5 -2 -2.1 3, text(-5.1 -3.8 "Treatable Causes") lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))
			(pcarrowi 3 4 -.1 5, text(3.8 4 "Untreatable Causes") lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))			
			;
	#delimit cr;	
	
graph display, 	xsize(7.5) ysize(5.5) 
graph export "$output/figureA2C8_tr_untr.emf", replace

