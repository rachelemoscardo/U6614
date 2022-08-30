clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2E1_bw.xls", clear  cells(A3:C65) first
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach stat in dd ddse pre post ddtest wlstest{
	sum lbwr if VARIABLES=="`stat'"
	local lbwr`stat' : display %03.2f r(mean)*100
	
	sum vlbwr if VARIABLES=="`stat'"
	local vlbwr`stat' : display %03.2f r(mean)*100
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
for var lbwr vlbwr: ren X X_
reshape wide *bwr_, i(exp) j(b)

for var *_0 *_1: replace X = X*100

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
**lbwr/vlbwr Kids
	cap drop lb* ub*
	local a = 1
	gen lblbw = lbwr_1 - 1.96*lbwr_0 if exp~=-1
	gen ublbw = lbwr_1 + 1.96*lbwr_0 if exp~=-1

	gen lbvlbw = vlbwr_1 - 1.96*vlbwr_0 if exp~=-1
	gen ubvlbw= vlbwr_1 + 1.96*vlbwr_0 if exp~=-1
	
	gen vlbwrdd 	= `vlbwrdd' if exp>=1 & exp<=9	
	gen lbwrdd 	= `lbwrdd' if exp>=1 & exp<=9
	

	#delimit ;
	twoway (scatter lbwr_1 vlbwr_1 lblbw ublbw ubvlbw lbvlbw vlbwrdd lbwrdd exp, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(forest_green dkorange forest_green forest_green dkorange dkorange orange green) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Sh T i i i i i i) 
			mcolor(forest_green dkorange forest_green forest_green dkorange dkorange red blue) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-3 0 3 5, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(order(1 2) rows(1) label(1 "Very Low Birth Weight (<1,500 g)") label(2 "Low Birth Weight (<2,500 g)") /*label(3 "lbwr 95% C.I.") label(5 "vlbwr 95% C.I.")*/ region(style(none)))
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("log Birth Weight Rate x 100", size(medium))
			title("", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(-3.2 -7 "VLBW DD Estimate: `vlbwrdd' (s.e. = `vlbwrddse')" , j(left) size(small) box fcolor(white) margin(small))
			text(3 4.2 "LBW DD Estimate: `lbwrdd' (s.e. = `lbwrddse')" , j(left) size(small) box fcolor(white) margin(small))			
			)
			(pcarrowi -3.2 -1.9 `vlbwrdd' .8, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))
			(pcarrowi 3 4.8 `lbwrdd' 4.8, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))
			(pcarrowi 4.4 -3.8 4 -1.1, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(4.5 -7.1 "Year Before Medicaid", j(left) size(medium)));
			;
	#delimit cr;	
	
graph display, 	xsize(7.5) ysize(5.5) 
graph export "$output/figureA2E1_bw.emf", replace

/******************DISPLAY THE P-VALUES FROM F-TESTS OF THE PRE/POST COEFS AND A JOINT TEST OF THE DD ASSUMPTIONS***************/
di "lbwr, Pre: `wpre'"
di "lbwr, Post: `wpost'"
di "lbwr, DD Test: `lbwrddtest'"
di "lbwr, WLS Test: `wwlstest'"
*di "lbwr, WLS DD Test: `wwlstestdd'"

di "vlbwr, Pre: `nwpre'"
di "vlbwr, Post: `nwpost'"
di "vlbwr, DD Test: `vlbwrddtest'"
di "vlbwr, WLS Test: `nwwlstest'"
*di "vlbwr, WLS DD Test: `nwwlstestdd'"
 
exit
