clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figure6.xls", clear  cells(A3:C65) first
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach stat in dd ddse {
	sum White if VARIABLES=="`stat'"
	local w`stat' : display %03.2f r(mean)*100
	
	sum Nonwhite if VARIABLES=="`stat'"
	local nw`stat' : display %03.2f r(mean)*100
}

foreach stat in pre post ddtest wlstest {
	sum White if VARIABLES=="`stat'"
	local w`stat' : display %03.2f r(mean)
	
	sum Nonwhite if VARIABLES=="`stat'"
	local nw`stat' : display %03.2f r(mean)
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

for var *_0 *_1: replace X = X*100

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
	cap drop lb* ub*
	local a = 1
	gen lbw = White_1 - 1.96*White_0 if exp~=-1
	gen ubw = White_1 + 1.96*White_0 if exp~=-1

	gen lbnw = Nonwhite_1 - 1.96*Nonwhite_0 if exp~=-1
	gen ubnw = Nonwhite_1 + 1.96*Nonwhite_0 if exp~=-1
	
	gen nwdd 	= `nwdd' if exp>=1 & exp<=9	
	gen wdd 	= `wdd' if exp>=1 & exp<=9
	
	#delimit ;
	twoway (scatter Nonwhite_1 ubnw lbnw nwdd  exp, 
			lpattern(solid dash dash solid solid) 
			lcolor(black black black gray gray) 
			lwidth(thick medium medium medium medium thick thick)
			msymbol(T i i i i i i) 
			mcolor(black black black gray gray) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-3(1)3, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("", size(medium))
			ytitle("log Mortality Rate x 100", size(medium))
			title("{it: A. Nonwhite Child Mortality}", size(medium) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(-2.5 -7 "Nonwhite DD Estimate: `nwdd' (s.e. = `nwddse')" , j(left) size(small) box fcolor(white) margin(small))
			)
			(pcarrowi -2.5 -1.7 `nwdd' .8, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))
			(pcarrowi 2 -3.8 1.5 -1.1, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(2 -7.5 "Year Before Medicaid", j(left) size(medium))
			saving("$output/nonwhite.gph", replace));
			;
	#delimit cr;	


	#delimit ;
	twoway (scatter White_1 ubw lbw  wdd exp, 
			lpattern(solid dash dash solid solid) 
			lcolor(black black black gray) 
			lwidth(thick medium medium medium medium thick thick)
			msymbol(Sh i i i i i i) 
			mcolor(black black black gray gray) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-5(5)5, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("log Mortality Rate x 100", size(medium))
			title("{it: B. White Child Mortality}", size(medium) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(-6 -7 "White DD Estimate: `wdd' (s.e. = `wddse')" , j(left) size(small) box fcolor(white) margin(small))				
			)
			(pcarrowi -6 -2.1 `wdd' 4.8, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))	
			(pcarrowi 9 -3.8 8 -1.1, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(9 -7.5 "Year Before Medicaid", j(left) size(medium))
			saving("$output/white.gph", replace));
			;
	#delimit cr;	
	
graph combine "$output/nonwhite.gph" "$output/white.gph", col(1) row(2) imargin(tiny)  graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 
graph display, 	xsize(5.8) ysize(7.975) 
graph export "$output/figure6_es_kids.emf", replace

erase "$output/nonwhite.gph" 
erase "$output/white.gph"

/******************DISPLAY THE P-VALUES FROM F-TESTS OF THE PRE/POST COEFS AND A JOINT TEST OF THE DD ASSUMPTIONS***************/
di "White, Pre: `wpre'"
di "White, Post: `wpost'"
di "White, DD Test: `wddtest'"
di "White, WLS Test: `wwlstest'"
*di "White, WLS DD Test: `wwlstestdd'"

di "Nonwhite, Pre: `nwpre'"
di "Nonwhite, Post: `nwpost'"
di "Nonwhite, DD Test: `nwddtest'"
di "Nonwhite, WLS Test: `nwwlstest'"
*di "Nonwhite, WLS DD Test: `nwwlstestdd'"
 
exit
