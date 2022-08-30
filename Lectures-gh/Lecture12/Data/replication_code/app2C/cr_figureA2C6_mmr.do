clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2C6_mmr.xls", clear  cells(A3:H36) first
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
for var *mr*: ren X X_
reshape wide *mr*_, i(exp) j(b)

for var *_0 *_1: replace X = X

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
cap drop lb* ub*
gen ub = mmr_1 + 1.96*mmr_0
gen lb = mmr_1 - 1.96*mmr_0	

#delimit ;
twoway (scatter mmr_1 lb ub exp, 
		lpattern(solid dot dot dash dash solid solid) 
		lcolor(green maroon maroon red blue) 
		lwidth(thick medthick medthick medium medium medium medium thick thick)
		msymbol(T i i i i i i) 
		mcolor(green maroon maroon red blue) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-7(3)9, labsize(medium))
		ylabel(, labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(off region(style(none)))
		xtitle("Years since Medicaid Implementation", size(medsmall))
		ytitle("Maternal Mortality Rate x 100", size(medsmall))
		title("{it: A. MMR, Continuous Spec.}", size(medlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		saving("$output/panela.gph", replace)
		)
		;
cap drop lb* ub*;
		
gen ub = dmr_1 + 1.96*dmr_0;
gen lb = dmr_1 - 1.96*dmr_0;

#delimit ;
twoway (scatter dmr_1 lb ub exp, 
		lpattern(solid dot dot dash dash solid solid) 
		lcolor(green maroon maroon red blue) 
		lwidth(thick medthick medthick medium medium medium medium thick thick)
		msymbol(T i i i i i i) 
		mcolor(green maroon maroon red blue) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-7(3)9, labsize(medium))
		ylabel(, labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(off region(style(none)))
		xtitle("Years since Medicaid Implementation", size(medsmall))
		ytitle("", size(medsmall))
		title("{it: B. P(MMR>0), Continuous Spec.}", size(medlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		saving("$output/panelb.gph", replace)
		)
		;


cap drop lb* ub*;
gen ub = mmr_b_1 + 1.96*mmr_b_0;
gen lb = mmr_b_1 - 1.96*mmr_b_0	;

#delimit ;
twoway (scatter mmr_b_no_w_1 mmr_b_1 lb ub exp, 
		lpattern(solid solid dot dot dash dash solid solid) 
		lcolor(blue green green green) 
		lwidth(thick thick medthick medthick medium medium medium medium thick thick)
		msymbol(Th T i i i i i i) 
		mcolor(blue green green green) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-7(3)9, labsize(medium))
		ylabel(, labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(off region(style(none)))
		xtitle("Years since Medicaid Implementation", size(medsmall))
		ytitle("Maternal Mortality Rate x 100", size(medsmall))
		title("{it: C. MMR, Binary Spec.}", size(medlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		saving("$output/panelc.gph", replace)
		text(-2.5 8 "Dropping" "the West")
		text(1.5 7 "Full Sample")
		)
		;
cap drop lb* ub*;
		
gen ub = dmr_b_1 + 1.96*dmr_b_0;
gen lb = dmr_b_1 - 1.96*dmr_b_0;

twoway (scatter dmr_b_no_w_1 dmr_b_1 lb ub exp, 
		lpattern(solid solid dot dot dash dash solid solid) 
		lcolor(blue green green green) 
		lwidth(thick thick medthick medthick medium medium medium medium thick thick)
		msymbol(Th T i i i i i i) 
		mcolor(blue green green green) 
		c(l l l l l l l l l) 
		cmissing(n n n n n n n) 
		xline(-1, lcolor(black)) 
		yline(0, lcolor(black)) 
		xlabel(-7(3)9, labsize(medium))
		ylabel(, labsize(medium))
		xsize(7.5) ysize(5.5) 			
		legend(off region(style(none)))
		xtitle("Years since Medicaid Implementation", size(medsmall))
		ytitle("", size(medsmall))
		title("{it: D. P(MMR>0), Binary Spec.}", size(medlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(zero))
		saving("$output/paneld.gph", replace)
		text(-.07 8 "Dropping" "the West")
		text(.12 7 "Full Sample")		
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
graph export "$output/figureA2C6_mmr.emf", replace;
#delimit cr;

erase "$output/panela.gph" 
erase "$output/panelb.gph"
erase "$output/panelc.gph" 
erase "$output/paneld.gph"



