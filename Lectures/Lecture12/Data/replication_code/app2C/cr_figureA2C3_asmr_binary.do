clear 
clear matrix
pause on

/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2C3_asmr_binary.xls", clear  cells(A3:I65) first
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress
drop in 1
*replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach stat in dd ddse wlstestdd{
	sum wnmr if VARIABLES=="`stat'"
	local wnmr`stat' : display %04.3f r(mean)
	sum nwnmr if VARIABLES=="`stat'"
	local nwnmr`stat' : display %04.3f r(mean)
	
	sum wpnmr if VARIABLES=="`stat'"
	local wpnmr`stat' : display %04.3f r(mean)
	sum nwpnmr if VARIABLES=="`stat'"
	local nwpnmr`stat': display %04.3f r(mean)

	sum wasmr14 if VARIABLES=="`stat'"
	local wasmr14`stat' : display %04.3f r(mean)
	sum nwasmr14 if VARIABLES=="`stat'"
	local nwasmr14`stat' : display %04.3f r(mean)	

	sum wasmr514 if VARIABLES=="`stat'"
	local wasmr514`stat' : display %04.3f r(mean)
	sum nwasmr514 if VARIABLES=="`stat'"
	local nwasmr514`stat' : display %04.3f r(mean)		
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
for var nw* w*: ren X X_
reshape wide nw*_ w*_, i(exp) j(b)



graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
**Neonatal IMR
	cap drop lb* ub*
	gen lbw = wnmr_1 - 1.96*wnmr_0 if exp~=-1
	gen ubw = wnmr_1 + 1.96*wnmr_0 if exp~=-1
	
	gen lbnw = nwnmr_1 - 1.96*nwnmr_0 if exp~=-1
	gen ubnw = nwnmr_1 + 1.96*nwnmr_0 if exp~=-1

	#delimit ;
	scatter wnmr_1 nwnmr_1 lbw ubw lbnw ubnw exp, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon red blue) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Sh T i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon red blue) 
			msize(large large)			
			c(l l l l l l l l l) 
			cmissing(y y n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-.2(.1).2, labsize(medium))			
			legend(off)
			xtitle("")
			ytitle("log Mortality Rate")
			title("{it: A. Neonatal Infant Mortality}", size(medlarge) color(black))
			text(.15 1.5 "White DD Estimate = `wnmrdd' (s.e. = `wnmrddse')", j(left) size(small) box fcolor(white) margin(small))			
			text(-.22 -.5 "Nonwhite DD Estimate = `nwnmrdd' (s.e. = `nwnmrddse')", j(left) size(small) box fcolor(white) margin(small))			
			graphregion(fcolor(white) color(white) icolor(white) ) 
			saving("$output/figureA2C3_nmr.gph", replace);
	#delimit cr;

**Post-Neonatal IMR
	cap drop lb* ub*
	gen lbw = wpnmr_1 - 1.96*wpnmr_0 if exp~=-1
	gen ubw = wpnmr_1 + 1.96*wpnmr_0 if exp~=-1
	
	gen lbnw = nwpnmr_1 - 1.96*nwpnmr_0 if exp~=-1
	gen ubnw = nwpnmr_1 + 1.96*nwpnmr_0 if exp~=-1	
	#delimit ;
	scatter wpnmr_1 nwpnmr_1 lbw ubw lbnw ubnw exp, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon red blue) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Sh T i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon red blue) 
			msize(large large)			
			c(l l l l l l l l l) 
			cmissing(y y n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-.2(.1).2, labsize(medium))			
			legend(off)
			xtitle("Years Since Medicaid Implementation")
			ytitle("log Mortality Rate")
			title("{it: B. Post-Neonatal Infant Mortality}", size(medlarge) color(black))
			text(-.15 -0.5 "White DD Estimate = `wpnmrdd' (s.e. = `wpnmrddse')", j(left) size(small) box fcolor(white) margin(small))			
			text(.17 -0.5 "Nonwhite DD Estimate = `nwpnmrdd' (s.e. = `nwpnmrddse')", j(left) size(small) box fcolor(white) margin(small))			
			graphregion(fcolor(white) color(white) icolor(white)) 
			saving("$output/figureA2C3_pnmr.gph", replace);
	#delimit cr;
	
*Younger Children, 1-4
	cap drop lb* ub*
	gen lbw = wasmr14_1 - 1.96*wasmr14_0 if exp~=-1
	gen ubw = wasmr14_1 + 1.96*wasmr14_0 if exp~=-1
	
	gen lbnw = nwasmr14_1 - 1.96*nwasmr14_0 if exp~=-1
	gen ubnw = nwasmr14_1 + 1.96*nwasmr14_0 if exp~=-1	
	#delimit ;
	scatter wasmr14_1 nwasmr14_1 lbw ubw lbnw ubnw exp, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon red blue) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Sh T i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon red blue) 
			msize(large large)			
			c(l l l l l l l l l) 
			cmissing(y y n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-.4(.2).4, labsize(medium))			
			legend(off)
			xtitle("")
			ytitle("")
			title("{it: C. Younger Child Mortality, Ages 1-4}", size(medlarge) color(black))
			text(.3 1.3 "White DD Estimate = `wasmr14dd' (s.e. = `wasmr14ddse')", j(left) size(small) box fcolor(white) margin(small))			
			text(-.35 0 "Nonwhite DD Estimate = `nwasmr14dd' (s.e. = `nwasmr14ddse')", j(left) size(small) box fcolor(white) margin(small))			
			graphregion(fcolor(white) color(white) icolor(white)) 
			saving("$output/figureA2C3_asmr14.gph", replace);
	#delimit cr; 
	
	
**Older Children, 5-14
	cap drop lb* ub*
	gen lbw = wasmr514_1 - 1.96*wasmr514_0 if exp~=-1
	gen ubw = wasmr514_1 + 1.96*wasmr514_0 if exp~=-1
	
	gen lbnw = nwasmr514_1 - 1.96*nwasmr514_0 if exp~=-1
	gen ubnw = nwasmr514_1 + 1.96*nwasmr514_0 if exp~=-1	
	#delimit ;
	scatter wasmr514_1 nwasmr514_1 lbw ubw lbnw ubnw exp, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon red blue) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Sh T i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon red blue) 
			msize(large large)				
			c(l l l l l l l l l) 
			cmissing(y y n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-.3(.15).3, labsize(medium))			
			legend(order(1 2 3 5) rows(3) label(1 "White Children") label(2 "Nonwhite Children") label(3 "White 95% C.I.") label(5 "Nonwhite 95% C.I.") region(style(none)))
			xtitle("Years Since Medicaid Implementation")
			ytitle("")
			title("{it: D. Older Child Mortality, Ages 5-14}", size(medlarge) color(black))
			text(-.22 1.1 "White DD Estimate = `wasmr514dd' (s.e. = `wasmr514ddse')", j(left) size(small) box fcolor(white) margin(small))			
			text(0.3 .5 "Nonwhite DD Estimate = `nwasmr514dd' (s.e. = `nwasmr514ddse')", j(left) size(small) box fcolor(white) margin(small))			
			graphregion(fcolor(white) color(white) icolor(white)) 
			saving("$output/figureA2C3_asmr514.gph", replace);
	#delimit cr; 
	

#delimit ;
grc1leg
"$output/figureA2C3_nmr.gph" "$output/figureA2C3_asmr14.gph"
"$output/figureA2C3_pnmr.gph" "$output/figureA2C3_asmr514.gph",
legendfrom("$output/figureA2C3_asmr514.gph") col(2) row(1)
imargin(tiny) 
graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
;

graph display, 	xsize(8) ysize(6); 
graph export "$output/figureA2C3_asmr_nw_binary.emf", replace;
#delimit cr;

erase "$output/figureA2C3_nmr.gph" 
erase "$output/figureA2C3_asmr14.gph"
erase "$output/figureA2C3_pnmr.gph" 
erase "$output/figureA2C3_asmr514.gph"

/******************DISPLAY THE P-VALUES FROM F-TESTS OF THE PRE/POST COEFS AND A JOINT TEST OF THE DD ASSUMPTIONS***************/
di "NNMR, Pre: `nmrpre'"
di "NNMR, Post: `nmrpost'"
di "NNMR, DD Test: `nmrddtest'"

di "PNMR, Pre: `pnmrpre'"
di "PNMR, Post: `pnmrpost'"
di "PNMR, DD Test: `pnmrddtest'"


di "1-4, Pre: `_1pre'"
di "1-4, Post: `_1post'"
di "1-4, DD Test: `_1ddtest'"

di "5-14, Pre: `_2pre'"
di "5-14, Post: `_2post'"
di "5-14, DD Test: `_2ddtest'"


