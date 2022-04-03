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
*replace VARIABLES = subinstr(VARIABLES,"HC_Texp_","",.)	
quietly destring _all, replace ignore(",")
compress

replace VARIABLES = "1" in 1
replace VARIABLES = "0" in 2
ren VARIABLES stat
*drop in 3
destring stat, replace force
recode stat (.=2)

xpose, clear varname
drop v2
drop in 1
destring(_varname), ignore("h" "d") gen(time)
gen day = strmatch(_varname,"d*")
replace time = time+24 if day

label def time ///
1 "Hour 1" ///
2 "Hour 2" ///
3 "Hour 3" ///
4 "Hour 4" ///
5 "Hour 5" ///
6 "Hour 6" ///
7 "Hour 7" ///
8 "Hour 8" ///
9 "Hour 9" ///
10 "Hour 10" ///
11 "Hour 11" ///
12 "Hour 12" ///
13 "Hour 13" ///
14 "Hour 14" ///
15 "Hour 15" ///
16 "Hour 16" ///
17 "Hour 17" ///
18 "Hour 18" ///
19 "Hour 19" ///
20 "Hour 20" ///
21 "Hour 21" ///
22 "Hour 22" ///
23 "Hour 23" ///
24 "Hour 24" ///
25 "Day 1" ///
26 "Day 2" ///
27 "Day 3" ///
28 "Day 4" ///
29 "Day 5" ///
30 "Day 6" ///
31 "Day 7" ///
32 "Day 8" ///
33 "Day 9" ///
34 "Day 10" ///
35 "Day 11" ///
36 "Day 12" ///
37 "Day 13" ///
38 "Day 14" ///
39 "Day 15" ///
40 "Day 16" ///
41 "Day 17" ///
42 "Day 18" ///
43 "Day 19" ///
44 "Day 20" ///
45 "Day 21" ///
46 "Day 22" ///
47 "Day 23" ///
48 "Day 24" ///
49 "Day 25" ///
50 "Day 26" ///
51 "Day 27"

label val time time

ren v1 eff
ren v3 mdv


replace eff = eff*mdv
qui sum eff if time==51
local d = r(mean)
gen sh = (eff[_n]-eff[_n-1])/`d'
replace sh = eff/`d' in 1


qui sum mdv if time==51
local d = r(mean)
gen shmdv = (mdv[_n]-mdv[_n-1])/`d'
replace shmdv = mdv/`d' in 1

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
cap drop lb* ub*
local obs = _N
local obs = `obs'+1
set obs `obs'
sort time
	
	#delimit ;
	twoway (scatter sh time, yaxis(1)
			c(l l)
			lpattern(solid dash)  
			lcolor(blue red) 
			lwidth(medthick medthick)
			msymbol(i i i i i i i) 
			ytitle("Share of NMR Effect", size(medium) axis(1))			
			xline(25)
			yline(0)
			)
			(pcarrowi .175 13 .15 2 , yaxis(1) lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) text(.19 13 "Share of neonatal mortality effect" "due to deaths in each period", size(medsmall))
			)
			(scatter mdv time, yaxis(2)
			lpattern(solid) 
			lcolor(green) 
			lwidth(medthick)
			msymbol(Oh i i i i i i i) 
			mcolor(green) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			xlabel(1 6 12 18 25 35 45 51, labsize(small) valuelabel)
			ytitle("", axis(2))			
			xsize(7.5) ysize(5.5) 			
			/*legend(order(1 2) rows(1) label(1 "DD Estimate on Mortality Before This Hour") label(2 "95% C.I.") region(style(none)))*/
			legend(off)
			ytitle("Mortality Rate", axis(2) size(medium))
			xtitle(" " "Died at or before this time", size(medmsall))
			title("", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)))
			(pcarrowi 18 34 16.5 34, yaxis(2) lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) text(19 40 "Cumulative infant mortality rate", size(medsmall)))
			;
	#delimit cr;	
	

graph export "$output/figureA2F1_nmr_decomp.emf", replace

exit
