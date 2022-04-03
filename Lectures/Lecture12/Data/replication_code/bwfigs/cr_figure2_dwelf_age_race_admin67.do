use "$data/figure2", clear

sum afdc_w if age<18 [aw=pop_w]
gen mkw = r(mean) if age<18
local mkw : display %04.2f round(r(mean)*100)/100
sum afdc_nw if age<18 [aw=pop_nw]
gen mknw = r(mean) if age<18
local mknw : display %04.2f round(r(mean)*100)/100

sum afdc_w if age>=18 & age<65 [aw=pop_w]
gen maw = r(mean) if age>=18 & age<65
local maw : display %04.2f round(r(mean)*100)/100
sum afdc_nw if age>=18 & age<65 [aw=pop_nw]
gen manw = r(mean) if age>=18 & age<65
local manw : display %04.2f round(r(mean)*100)/100

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/


#delimit ;
	twoway (scatter afdc_nw afdc_w age if age<65, 
			lpattern(solid solid dash dash dash dash) 
			lcolor(black gray black black gray gray) 
			lwidth(medthick medthick medium medium medium medium )
			msymbol(Oh T i i i i) 
			mcolor(black gray ) 
			c(l l l l l l l l l) 
			cmissing(y y y y n n) 
			text(.2 24 "Nonwhite", size(medsmall))
			text(.06 5 "White", size(medsmall))
			legend(off)
			ylabel(, labsize(medsmall))
			xtitle("Age", size(medsmall))
			ytitle("Share in Families with Welfare Income", size(medsmall))
			title("", size(medsmall) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(small)) plotregion(margin(small))
			
			text(`mknw' 22 "Mean, Ages"  "0 to 18 = `mknw'" , j(left) size(medsmall) fcolor(white) margin(small))
			text(.055 55 "Mean, Over" "Age 18 = `manw'" , j(left) size(medsmall) fcolor(white) margin(small))
		
			text(.02 9 "Mean, Ages" "0 to 18 = `mkw'" , j(left) size(medsmall) fcolor(white) margin(small))
			text(.025 40 "Mean, Over" "Age 18 = `maw'" , j(left) size(medsmall) fcolor(white) margin(small))
			
			)
			(pcarrowi .2 20 0.2 14, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))
			(pcarrowi .055 5 .04 6, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick))
			;
#delimit cr;
graph export "$output/figure2_dwelf_age_race_admin67.emf", replace

exit
