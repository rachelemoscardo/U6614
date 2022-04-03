use "$data/balance",clear
qui sum crate
if r(mean)<1{
	replace crate = crate*100
}
gen samp = ~inlist(stfips,23,33,50)

foreach y of numlist 1961 1958 1948{
	reg crate0 /*i.region i.ymcaid*/ crate if samp & year==`y' & ~white, robust
	predict fit`y' if e(sample), xb
	local b`y' : display %04.2f _b[crate]
	local se`y' : display %04.2f _se[crate]
	local i = `i' + 1
}
graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/

keep if (year==1948|year==1958|year==1961) 
xi: reg crate0 i.year*crate if samp & (year==1948|year==1958|year==1961) & ~white, robust
testparm _IyeaXcra_1961 _IyeaXcra_1958
local joint : display %04.2f r(p)
*NO South Dakota
xi: reg crate0 i.year*crate if samp & (year==1948|year==1958|year==1961) & ~white & stfips~=46, robust
testparm _IyeaXcra*
local joint_nosd : display %04.2f r(p)
*Robust Regression
xi: rreg crate0 i.year*crate if samp & (year==1948|year==1958|year==1961) & ~white
testparm _IyeaXcra*
local joint_rr : display %04.2f r(p)
	
twoway 	scatter crate0 crate if samp & ~white & year==1961, msymbol(O)  || ///
		scatter crate0 crate if samp & ~white & year==1958, msymbol(Sh)  || ///
		scatter crate0 crate if samp & ~white & year==1948, msymbol(X)  || ///
		line fit1961 crate if samp & ~white & year==1961, lcolor(navy)  || ///
		line fit1958 crate if samp & ~white & year==1958, lcolor(maroon) lpattern(dash) || ///
		line fit1948 crate if samp & ~white & year==1948, lcolor(forest_green) lpattern(dash_dot) ///
		legend(order(- "1961: " 1 - "1958:" 2 - "1948:" 3) rows(1) ///
		label(1	"") label(2	"") label(3	"") region(style(none))) ///
		ytitle("Nonwhite AFDC Rate in Medicaid Year" " ", size(medsmall)) xtitle("Nonwhite AFDC Rate Before Medicaid", size(medsmall)) ///
		title("{it: A. Nonwhite Women}", size(medlarge) color(black)) ///
		graphregion(fcolor(white) color(white) icolor(white)) ///
		text(10 25 "H{subscript:0}: Slopes are" "equal ({it:p}-value) = `joint'", j(left) size(medium) color(black)) || ///
		pcarrowi /*blunt end, (y,x)*/ 18 25 /*pointy end, (y,x)*/ 20 22, lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) text(17 25 "1961: `b1961', s.e. = `se1961'") || ///
		pcarrowi /*blunt end, (y,x)*/ 24 16 /*pointy end, (y,x)*/ 22 21, lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) text(25 17 "1958: `b1958', s.e. = `se1958'") || ///
		pcarrowi /*blunt end, (y,x)*/ 20 05 /*pointy end, (y,x)*/ 15 08 , lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) text(21 06 "1948: `b1948', s.e. = `se1948'")  ///
		saving("$output/afdc_persistence_nw.gph", replace) 

di "P-value that slopes are equal: `joint'"		
di "P-value that slopes are equal, excluding SD: `joint_nosd'"
di "P-value that slopes are equal, robust regression: `joint_rr'"


use "$data/balance",clear
qui sum crate
if r(mean)<1{
	replace crate = crate*100
}
gen samp = 1

foreach y of numlist 1961 1958 1948{
	reg crate0 /*i.region i.ymcaid*/ crate if samp & year==`y' & white, robust
	predict fit`y' if e(sample), xb
	local b`y' : display %04.2f _b[crate]
	local se`y' : display %04.2f _se[crate]
	local i = `i' + 1
}
graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/

keep if (year==1948|year==1958|year==1961) 
xi: reg crate0 i.year*crate if samp & (year==1948|year==1958|year==1961) & white, robust
testparm _IyeaXcra_1961 _IyeaXcra_1958
local joint : display %04.2f r(p)
*NO WV
xi: reg crate0 i.year*crate if samp & (year==1948|year==1958|year==1961) & white & stfips~=54, robust
testparm _IyeaXcra*
local joint_nowv : display %04.2f r(p)
*Robust Regression
xi: rreg crate0 i.year*crate if samp & (year==1948|year==1958|year==1961) & white
testparm _IyeaXcra*
local joint_rr : display %04.2f r(p)
	
twoway 	scatter crate0 crate if samp & white & year==1961, msymbol(O)  || ///
		scatter crate0 crate if samp & white & year==1958, msymbol(Sh)  || ///
		scatter crate0 crate if samp & white & year==1948, msymbol(X)  || ///
		line fit1961 crate if samp & white & year==1961, lcolor(navy)  || ///
		line fit1958 crate if samp & white & year==1958, lcolor(maroon) lpattern(dash) || ///
		line fit1948 crate if samp & white & year==1948, lcolor(forest_green) lpattern(dash_dot) ///
		legend(order(- "1961: " 1 - "1958:" 2 - "1948:" 3) rows(1) ///
		label(1	"") label(2	"") label(3	"") region(style(none))) ///
		ytitle("Nonwhite AFDC Rate in Medicaid Year" " ", size(medsmall)) xtitle("White AFDC Rate Before Medicaid", size(medsmall)) ///
		title("{it: B. White Women}", size(medlarge) color(black)) ///
		graphregion(fcolor(white) color(white) icolor(white)) ///
		text(04 01 "H{subscript:0}: Slopes are" "equal ({it:p}-value) = `joint'", j(left) size(medium) color(black)) || ///
		pcarrowi /*blunt end, (y,x)*/ 04.2 03.5 /*pointy end, (y,x)*/ 04 04, lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) text(04.4 03.2 "1961: `b1961', s.e. = `se1961'") || ///
		pcarrowi /*blunt end, (y,x)*/ 02.9 04.2 /*pointy end, (y,x)*/ 03.5 04, lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) text(02.7 04.3 "1958: `b1958', s.e. = `se1958'") || ///
		pcarrowi /*blunt end, (y,x)*/ 01.2 03.5 /*pointy end, (y,x)*/ 02.2 02.8 , lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) text(01 03.5 "1948: `b1948', s.e. = `se1948'")  ///
		saving("$output/afdc_persistence_w.gph", replace) 
		
di "P-value that slopes are equal: `joint'"
di "P-value that slopes are equal, excluding WV: `joint_nowv'"
di "P-value that slopes are equal, robust regression: `joint_rr'"

grc1leg "$output/afdc_persistence_nw.gph" "$output/afdc_persistence_w.gph", legendfrom("$output/afdc_persistence_w.gph") col(1) row(2) imargin(tiny) graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 
graph display, xsize(8.5) ysize(11)
graph export "$output/figureA2A3_afdc_persistence.wmf", replace

erase "$output/afdc_persistence_nw.gph" 
erase "$output/afdc_persistence_w.gph"
