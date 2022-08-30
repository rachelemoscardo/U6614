clear 
clear matrix
pause on

/*****************************ESTIMATES BY HOUR********************************/
xmluse  "$output/figure8.xls", clear  cells(A3:AZ12) first
replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress

drop in 4/8
drop in 1
quietly destring _all, replace ignore(",")
compress

replace VARIABLES = "1" in 1
replace VARIABLES = "0" in 2
ren VARIABLES stat
drop in 3
destring stat, replace


reshape long h d, i(stat) j(time)
reshape wide h d, i(time) j(stat)

for var h* d*: replace X = X*100

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
cap drop lb* ub*
local obs = _N
local obs = `obs'+1
set obs `obs'
for var time *1: replace X = 0 in `obs'
sort time

gen lbnw = h1  - 1.96*h0
gen ubnw = h1  + 1.96*h0
	
	#delimit ;
	twoway (scatter h1 ubnw lbnw time if time<=24, 
			lpattern(solid dash dash solid solid) 
			lcolor(black black black) 
			lwidth(thick medium medium medium medium medium medium thick thick)
			msymbol(O i i i i i i) 
			mcolor(black) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xlabel(0 1 3(3)24, labsize(medium))
			ylabel(, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("Died On or Before This Hour", size(medium))
			ytitle("Log Infant Mortality Rate x 100", size(medium))
			title("{it: A. Deaths by Hour}", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			saving("$datatemp/byhour", replace))
			;
	#delimit cr;	


drop *bnw
gen lbnw = d1  - 1.96*d0
gen ubnw = d1  + 1.96*d0
	
	#delimit ;
	twoway (scatter d1 ubnw lbnw time if time<=27 & time>0, 
			lpattern(solid dash dash solid solid) 
			lcolor(black black black) 
			lwidth(thick medium medium medium medium medium medium thick thick)
			msymbol(O i i i i i i) 
			mcolor(black) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xlabel(1 3(3)27, labsize(medium))
			ylabel(-3(1)0, nolab)
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("Died On or Before This Day", size(medium))
			ytitle("Log Infant Mortality Rate x 100", size(medium))
			title("{it: B. Deaths by Day}", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			saving("$datatemp/byday", replace))
			;
	#delimit cr;	
	
graph combine "$datatemp/byhour.gph" "$datatemp/byday.gph", col(2) row(1) ycommon imargin(tiny)  graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 
graph display, 	xsize(9.5) ysize(4.34) 
graph export "$output/figure8_lnimr_byhour_byday.emf", replace

erase "$datatemp/byhour.gph" 
erase "$datatemp/byday.gph"
exit
