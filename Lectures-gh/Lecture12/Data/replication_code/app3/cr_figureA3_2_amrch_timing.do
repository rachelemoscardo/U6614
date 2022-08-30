clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA3_2_amrch_timing.xls", clear  cells(A3:C61) first
replace VARIABLES = subinstr(VARIABLES,"_Texp_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach stat in dd ddse pre post ddtest wlstest{
	sum White if VARIABLES=="`stat'"
	local w`stat' : display %04.3f r(mean)
	
	sum Nonwhite if VARIABLES=="`stat'"
	local nw`stat' : display %04.3f r(mean)
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
for var White Nonwhite: ren X X_
reshape wide *ite_, i(exp) j(b)


graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
**WHITE/NONWHITE Kids
	cap drop lb* ub*
	local a = 1
	gen lbw = White_1 - 1.96*White_0 if exp~=-1
	gen ubw = White_1 + 1.96*White_0 if exp~=-1

	gen lbnw = Nonwhite_1 - 1.96*Nonwhite_0 if exp~=-1
	gen ubnw = Nonwhite_1 + 1.96*Nonwhite_0 if exp~=-1
	
	gen nwdd 	= `nwdd' if exp>=1 & exp<=9	
	gen wdd 	= `wdd' if exp>=1 & exp<=9
	
for var ubw lbw: replace X = . 
	#delimit ;
	twoway (scatter White_1 Nonwhite_1 lbw ubw ubnw lbnw exp, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon red blue) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Sh T i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon red blue) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(order(1 2) rows(1) label(1 "White Children") label(2 "Nonwhite Children") /*label(3 "White 95% C.I.") label(5 "Nonwhite 95% C.I.")*/ region(style(none)))
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("log Mortality Rate x 100", size(medium))
			title("", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(-.25 -7 "Nonwhite DD Estimate:  `nwdd' (s.e. = `nwddse')" "White DD Estimate:       `wdd' (s.e. = `wddse')", j(left) size(small) box fcolor(white) margin(small))
			)
			(pcarrowi .304 -3.8 .28 -1.1, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(.305 -7.1 "Year Before Medicaid", j(left) size(medium)));
			;
	#delimit cr;	
	
graph display, 	xsize(7.5) ysize(5.5) 
graph export "$output/figureA3_2_amrch_timing.emf", replace

