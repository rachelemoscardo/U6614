clear 
clear matrix
pause on

xmluse  "$output/figureA2C1_A_amr_1950_1988.xls", clear  cells(A3:C72) first
drop in 1
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress

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
for var *ite*: ren X X_
reshape wide *ite*_, i(exp) j(b)



graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
**WHITE/NONWHITE Kids
	cap drop lb* ub*
	local a = 1
	gen lbw = White_1 - 1.96*White_0 if exp~=-1
	gen ubw = White_1 + 1.96*White_0 if exp~=-1

	gen lbnw = Nonwhite_1 - 1.96*Nonwhite_0 if exp~=-1
	gen ubnw = Nonwhite_1 + 1.96*Nonwhite_0 if exp~=-1
	
for var *bw: replace X = . 	
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
			xlabel(-16(3)17, labsize(medium))
			ylabel(, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(order(1 2) rows(1) label(1 "White Children") label(2 "Nonwhite Children") region(style(none)))
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("log Mortality Rate", size(medium))
			title("{it:A. Mortality Effects}", size(medlarge) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			saving("$output/amr5088.gph", replace));
			;
	#delimit cr;
	
graph display, 	xsize(7.5) ysize(5.5) 




xmluse  "$output/figureA2C1_B_simelig.xls", clear  cells(A3:C31) first
drop in 1
replace VARIABLES = subinstr(VARIABLES,"CR_Iyear_","",.)	
quietly destring _all, replace ignore(",")
compress

cap destring VARIABLES, replace

sum medelig if VARIABLES=="b" 
local mb : display %04.3f r(mean)
sum medelig if VARIABLES=="se" 
local mbse : display %04.3f r(mean)

sum simelig if VARIABLES=="b" 
local sb : display %04.3f r(mean)
sum simelig if VARIABLES=="se" 
local sbse : display %04.3f r(mean)	

drop in 21/27

ren VARIABLES year
destring year, replace
gen b = year<.
replace year = year[_n-1] if year[_n]==.
reshape wide *elig*, i(year) j(b)

gen lbm = medelig1 - 1.96*medelig0
gen ubm = medelig1 + 1.96*medelig0

gen lbs = simelig1 - 1.96*simelig0
gen ubs = simelig1 + 1.96*simelig0

reg medelig1 year
predict pm
reg simelig1 year
predict ps


	#delimit ;
	twoway (scatter medelig1 simelig1 pm ps  year, 
			lpattern(solid solid solid solid dot dot dash dash solid solid) 
			lcolor(black gray black gray black black gray gray ) 
			lwidth(thick thick medthick medthick medium medium medium medium thick thick)
			msymbol(Oh S i i i i i i) 
			msize(medlarge medlarge)
			mcolor(black gray black gray black black gray gray) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xlabel(1979 1982 1985 1988, labsize(medium))
			ylabel(, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(order(1 2) rows(1) label(1 "Medicaid Elig.") label(2 "Simulated Medicaid Elig.") region(style(none)))
			xtitle("Year", size(medium))
			ytitle("corr(AFDC{superscript:*}, Medicaid Elig.)", size(medium))
			title("{it: B. AFDC{superscript:*} and Nonwhite Medicaid Eligiblity, 1979-1988}", size(medlarge) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(small)) 
			text(.92 1986 "Trends:" "    Elig.         `mb', s.e. = `mbse'"
						"    Sim. Elig. `sb', s.e. = `sbse'", j(left))
			saving("$output/simelig_check.gph", replace));
			;
	#delimit cr;

graph combine "$output/amr5088.gph" "$output/simelig_check.gph", col(1) row(2) imargin(tiny)  graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 
graph display, 	xsize(5.8) ysize(7.975) 
graph export "$output/figureA2C1_amr_1950_1988.emf", replace

erase "$output/amr5088.gph" 
erase "$output/simelig_check.gph"

exit


