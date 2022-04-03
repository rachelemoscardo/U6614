clear
clear matrix
pause on
set more off
cap log close

use "$data/mvp_mcaid", clear
*drop WV (see figure notes)
drop if stfips==54|stfips==4

replace rmvp_ch = . if stfips==18 & year==1969

preserve
collapse (mean) rmvp_ch [aw=ch_pop], by(exp)
save "$datatemp/testo", replace
restore

collapse (mean) rmvp_ch [aw=ch_pop], by(exp hc)
reshape wide rmvp_ch, i(exp) j(hc)
merge 1:1 exp using "$datatemp/testo"
drop if exp==-4|exp==7
graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/

	#delimit ;
	twoway (scatter rmvp_ch0 rmvp_ch1 rmvp_ch exp, 
			lpattern(dash dash solid solid) 
			lcolor(maroon maroon navy forest_green) 
			lwidth(medthick medthick thick medthick)
			msymbol(Sh S i O) 
			mcolor(maroon maroon navy forest_green) 
			c(l l l l l l l l l) 
			cmissing(y y y y n n) 
			xline(-1, lcolor(black)) 
			xlabel(-3(3)6, labsize(small))
			ylabel(0(.05).15, labsize(small))
			legend(off)
			text(.012 2.2 "Year Before Medicaid", size(medsmall) j(left))
			xtitle("Years Since Medicaid Implementation")
			ytitle("")
			title("", size(medium) color(black))
			xsize(6)
			ysize(5)
			text(.05 4 "Low-Eligibility States")
			text(.12 1 "High-Eligibility States")
			text(.09 4 "All States")
			graphregion(fcolor(white) color(white) icolor(white) margin(small)) )
			(pcarrowi .01 .8 0 -.9, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))
			;
	#delimit cr;

graph export "$output/figure3_mvp_levels.emf", replace
	

exit
