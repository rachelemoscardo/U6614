clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
*need a way to map the number of regressions to the letter code in excel columns
xmluse  "$output/figure7.xls", clear  cells(A3:H54) first
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
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
for var *mr*: ren X X_
reshape wide *mr*_, i(exp) j(b)

for var *_0 *_1: replace X = X*100
graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
	cap drop ub* lb*
	
	#delimit ;
	twoway (scatter nmrx_1 pnmr_1 dmr_1  exp, 
			lpattern(solid solid solid dash dash dot dot solid solid) 
			lcolor(blue green red red red blue blue) 
			lwidth(medthick medthick thick medium medium medium medium thick thick)
			msymbol(Th i O i i i ) msize(medlarge med large)
			mcolor(blue green red red blue blue) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-2.5 0 2.5, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			xtitle("", size(medium))
			ytitle("log Mortality Rate x 100", size(medium))
			title("{it:A. Infant Mortality}", size(medium) color(black))
			legend(order(3 1 2) label(3 "First Day") label(1 "Days 2-27") label(2 "Post-Neonatal") rows(3) ring(0) bplace(sw) bmargin(large))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero))
			saving("$datatemp/a.gph", replace))
			;
	#delimit cr;	
	
graph display, 	xsize(7.5) ysize(5.5) 


graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
	cap drop ub* lb*

	#delimit ;
	twoway (scatter asmr59_1 asmr1014_1 asmr14_1 exp, 
			lpattern(solid solid solid dash dash dot dot solid solid) 
			lcolor(blue green red red red blue blue) 
			lwidth(medthick medthick thick medium medium medium medium thick thick)
			msymbol(Dh i S i i i ) msize(medlarge med large)
			mcolor(blue green red red blue blue) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xline(-1, lcolor(black)) 
			yline(0, lcolor(black)) 
			xlabel(-16(3)9, labsize(medium))
			ylabel(-5 -2.5 0 2.5, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			xtitle("Years since Medicaid Implementation", size(medium))
			ytitle("log Mortality Rate x 100", size(medium))
			title("{it:B. Child Mortality}", size(medium) color(black))
			legend(order(3 1 2) label(3 "Ages 1-4") label(1 "Ages 5-9") label(2 "Ages 10-14") rows(3) ring(0) bplace(sw) bmargin(large))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero))
			saving("$datatemp/b.gph", replace))			
			;
	#delimit cr;	

	
graph combine "$datatemp/a.gph" "$datatemp/b.gph", col(1) row(2) xcommon imargin(tiny)  graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 
graph display, 	xsize(5.8) ysize(7.975) 
graph export "$output/figure7_age_specific.emf", replace

erase "$datatemp/a.gph" 
erase "$datatemp/b.gph"	

exit
