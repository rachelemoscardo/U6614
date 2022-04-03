clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2E3_hospbirths_dd.xls", clear  cells(A3:E12) first
drop in 1
quietly destring _all, replace ignore(",")
compress
**make locals out of the DD estimates and the F-test p-values for a textbox
sum w_all in 1
local dd : display %03.2f r(mean)*100
sum w_all in 2
local ddse : display %03.2f r(mean)*100
sum w_all in 8
local bsp : display %04.3f r(mean)

sum uw_south in 1
local osdd : display %03.2f r(mean)*100
sum uw_south in 2
local osddse : display %03.2f r(mean)*100
sum uw_south in 8
local osbsp : display %05.3f r(mean)


sum bw_all in 1
local bdd : display %05.3f r(mean)
sum bw_all in 2
local bddse : display %05.3f r(mean)
sum bw_all in 8
local bbsp : display %05.3f r(mean)

sum buw_south in 1
local bosdd : display %05.3f r(mean)
sum buw_south in 2
local bosddse : display %05.3f r(mean)
sum buw_south in 8
local bosbsp : display %05.3f r(mean)


/*****************************GET ES COEFS********************************/
*need a way to map the number of regressions to the letter code in excel columns
xmluse  "$output/figureA2E3_hospbirths.xls", clear  cells(A3:E54) first
drop in 1
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress

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
for var *w* : ren X X_
reshape wide *w*, i(exp) j(b)

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
**WHITE/NONWHITE Kids

	for var w_*: replace X = X*100
	
	cap drop lb* ub*
	gen lb = w_all_1 - 1.96*w_all_0 if exp~=-1
	gen ub = w_all_1 + 1.96*w_all_0 if exp~=-1
	
	gen lbos = w_south_1 - 1.96*w_south_0 if exp~=-1
	gen ubos = w_south_1 + 1.96*w_south_0 if exp~=-1	
	
	for var lb* ub*: replace X = . if abs(X)>1
	#delimit ;
	twoway (scatter w_all_1 w_south_1 ub lb ubos lbos exp, 
			lpattern(solid solid dot dot dash dash) 
			lcolor(blue red blue blue red red gray) 
			lwidth(thick thick medium medium medium mediumthick)
			msymbol(X S i i i i i) 
			mcolor(blue red blue blue red red) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("Nonwhite/White Difference in Hospital Share of Births", size(small))
			title("{it: A. Continuous Specification}", size(medlarge))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(-.7 4 "{bf:All States:} " "DD Estimate: `dd'   (s.e. = `ddse')" "Bootstrap {it:p}-value = `bsp'"  , j(left) size(small) box fcolor(white) margin(small))
			text(.8 -4 "{bf:South Only:} " "DD Estimate: `osdd'   (s.e. = `osddse')" "Bootstrap {it:p}-value = `osbsp'", j(left) size(small) box fcolor(white) margin(small))
			)
			(pcarrowi -.8 4.5 .1 4.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick) )
			(pcarrowi .8 -4 .37 2, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick) )

			/*			(pcarrowi -.11 -.3 `dd' .8, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))*/
			(pcarrowi -.7 -1.8 -.7 -1.1, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(-.7 -5.5 "Year Before Medicaid", j(left) size(medium)) 
			saving("$output/hb_continuous.gph", replace));
			
#delimit cr;

	
	cap drop lb* ub*
	gen lb = bw_all_1 - 1.96*bw_all_0 if exp~=-1
	gen ub = bw_all_1 + 1.96*bw_all_0 if exp~=-1
	
	gen lbos = buw_south_1 - 1.96*buw_south_0 if exp~=-1
	gen ubos = buw_south_1 + 1.96*buw_south_0 if exp~=-1	
	
*	for var lb* ub*: replace X = . if abs(X)>1
	#delimit ;
	twoway (scatter bw_all_1 buw_south_1 ub lb ubos lbos exp, 
			lpattern(solid solid dot dot dash dash) 
			lcolor(blue red blue blue red red gray) 
			lwidth(thick thick medium medium medium mediumthick)
			msymbol(X S i i i i i) 
			mcolor(blue red blue blue red red) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("Nonwhite/White Difference in Hospital Share of Births", size(small))
			title("{it: B. Binary Specification}", size(medlarge))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(-.07 4 "{bf:All States:} " "DD Estimate: `bdd'   (s.e. = `bddse')" "Bootstrap {it:p}-value = `bbsp'"  , j(left) size(small) box fcolor(white) margin(small))
			text(.08 -4 "{bf:South Only:} " "DD Estimate: `bosdd'   (s.e. = `bosddse')" "Bootstrap {it:p}-value = `bosbsp'", j(left) size(small) box fcolor(white) margin(small))
			)
			(pcarrowi -.08 4.5 .01 4.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick) )
			(pcarrowi .08 -4 .037 2, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick) )

			/*			(pcarrowi -.11 -.3 `dd' .8, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))*/
			(pcarrowi -.07 -1.8 -.07 -1.1, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(-.07 -5.5 "Year Before Medicaid", j(left) size(medium)) 
			saving("$output/hb_binary.gph", replace));
			
#delimit cr;

graph combine "$output/hb_continuous.gph" "$output/hb_binary.gph", row(2) col(1) graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
graph display, 	xsize(5.8) ysize(7.975) 
graph export "$output/figureA2E3_hospbirths.emf", replace

erase "$output/hb_continuous.gph" 
erase "$output/hb_binary.gph"

exit
