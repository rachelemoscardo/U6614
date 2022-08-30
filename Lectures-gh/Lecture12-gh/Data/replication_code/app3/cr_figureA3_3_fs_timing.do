clear 
clear matrix
pause on

xmluse  "$output/figureA3_3_fs_timing.xls", clear  cells(A3:B29) first
drop in 1
replace VARIABLES = subinstr(VARIABLES,"_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress

**make locals out of the DD estimates and the F-test p-values for a textbox

foreach stat in dd ddse{
	sum X3 if VARIABLES=="`stat'"
	local `stat' : display %03.2f round(r(mean)*10000)/100
}


drop if _n>=19

cap destring VARIABLES, replace
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
for var *X*: ren X X_
reshape wide *X*_, i(exp) j(b)

for var *_0 *_1: replace X = X*100
graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
**CHILDREN
	cap drop lb* ub*
	gen lb = X3_1 - 1.96*X3_0 if exp~=-1
	gen ub = X3_1 + 1.96*X3_0 if exp~=-1
	
	#delimit ;
	twoway (scatter X3_1 lb ub exp, 
			lpattern(solid dash dash) 
			lcolor(forest_green forest_green forest_green ) 
			lwidth(thick medium medium)
			msymbol(O i i i i) 
			mcolor(forest_green forest_green forest_green) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-3(3)6, labsize(medium))
			ylabel(, labsize(medium))
			xsize(7) ysize(5.5) 			
			legend(off)
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("Share Using Public Insurance", size(medium))
			title("", size(large) color(black))			
			text(3.05 -1.5 "DD Estimate:"  "`dd' (s.e. = `ddse')", j(left) size(medsmall) box fcolor(white) margin(small))			
			graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) )
			(pcarrowi 7.60 .02 6.8 -.75, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(7.805 1.2 "Year Before Medicaid", j(left) size(medium)));
	#delimit cr;	
	
graph export "$output/figureA3_3_fs_timing.emf", replace
	
di "pre: `pre'"
di "post: `post'"
di "ddtest: `ddtest'"	
di "pltest: `pltest'"	
di "wlstest: `wlstest'"	
di "wlstestdd: `wlstestdd'"	

exit	
	
