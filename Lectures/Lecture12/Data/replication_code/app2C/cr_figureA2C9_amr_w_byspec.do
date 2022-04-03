clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2C9_amr_w_byspec.xls", clear  cells(A3:H54) first
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
for var X*: ren X X_
reshape wide *X?_, i(exp) j(b)

for var *_0 *_1: replace X = X*100

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
cap drop lb* ub*
gen ub = X3_1 + 1.96*X3_0
gen lb = X3_1 - 1.96*X3_0	


#delimit ;
twoway (scatter X3_1 X1_1 X5_1 lb ub exp, 
		lpattern(solid solid solid dot dot dash dash solid solid) 
		lcolor(maroon midblue green maroon maroon red blue) 
		lwidth(thick medthick medthick medium medium medium medium thick thick)
		msymbol(T Oh Sh i i i i i i) 
		mcolor(maroon midblue green maroon maroon red blue) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-16(3)9, labsize(medium))
		ylabel(, labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(order(1 2 3) rows(3) label(1 "Preferred (Col. 2)") label(2 "Simple (Col. 1)") label(3 "Linear Trends (Col. 4)") ring(0) bplace(nw) bmargin(small) rowgap(.1) size(small))
		xtitle("Years since Medicaid Implementation", size(medsmall))
		ytitle("log Mortality Rate x 100", size(medsmall))
		title("{it: A. Adding Covariates}", size(medlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		saving("$output/panela.gph", replace)
		)
		;


twoway (scatter X3_1 X4_1 lb ub exp, 
		lpattern(solid solid dot dot dash dash solid solid) 
		lcolor(maroon green maroon maroon red blue) 
		lwidth(thick medthick medium medium medium medium thick thick)
		msymbol(T Sh i i i i i i) 
		mcolor(maroon green maroon maroon red blue) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-16(3)9, labsize(medium))
		ylabel(, labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(order(1 2) rows(2) label(1 "Preferred (Col. 2)") label(2 "Unweighted (Col. 3)") ring(0) bplace(sw) bmargin(small) rowgap(.1) size(small))
		xtitle("Years since Medicaid Implementation", size(medsmall))
		ytitle("log Mortality Rate x 100", size(medsmall))
		title("{it: B. Population Weighting}", size(medlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		saving("$output/panelb.gph", replace)
		)
		;

twoway (scatter X3_1 X6_1 lb ub exp, 
		lpattern(solid solid dot dot dash dash solid solid) 
		lcolor(maroon green maroon maroon red blue) 
		lwidth(thick medthick medium medium medium medium thick thick)
		msymbol(T Sh i i i i i i) 
		mcolor(maroon green maroon maroon red blue) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-16(3)9, labsize(medium))
		ylabel(, labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(order(1 2) rows(2) label(1 "Preferred (Col. 2)") label(2 "DDD (Col. 5)") ring(0) bplace(se) bmargin(small) rowgap(.1) size(small))
		xtitle("Years since Medicaid Implementation", size(medsmall))
		ytitle("log Mortality Rate x 100", size(medsmall))
		title("{it: C. Triple-Difference by Race}", size(medlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		saving("$output/panelc.gph", replace)
		)
		;		
		
twoway (scatter X3_1 X7_1 lb ub exp, 
		lpattern(solid solid dot dot dash dash solid solid) 
		lcolor(maroon green maroon maroon red blue) 
		lwidth(thick medthick medium medium medium medium thick thick)
		msymbol(T Sh i i i i i i) 
		mcolor(maroon green maroon maroon red blue) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-16(3)9, labsize(medium))
		ylabel(, labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(order(1 2) rows(2) label(1 "Preferred (Col. 2)") label(2 "1958 IV (Col. 6)") ring(0) bplace(se) bmargin(small) rowgap(.1) size(small))
		xtitle("Years since Medicaid Implementation", size(medsmall))
		ytitle("log Mortality Rate x 100", size(medsmall))
		title("{it: D. IV Using 1958 AFDC}", size(medlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		saving("$output/paneld.gph", replace)
		)
		;		
		
#delimit ;
graph combine
"$output/panela.gph" "$output/panelb.gph"
"$output/panelc.gph" "$output/paneld.gph",
 col(2) row(1)
imargin(tiny) 
graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
;

graph display, 	xsize(8) ysize(6); 
graph export "$output/figureA2C9_amr_w_byspec.emf", replace;
#delimit cr;

erase "$output/panela.gph" 
erase "$output/panelb.gph"
erase "$output/panelc.gph" 
erase "$output/paneld.gph"


