use "$data/app2A_welfare1970_medicaid1976", clear
#delimit ;
	scatter dmcaid0 dmcaid1 age if age<65, 
			lpattern(solid solid dash dash dash dash) 
			lcolor(navy maroon navy navy maroon maroon) 
			lwidth(medthick medthick medium medium medium medium )
			msymbol(O Th i i i i) 
			mcolor(navy maroon ) 
			c(l l l l l l l l l) 
			cmissing(y y y y n n) 
			legend(order(1 2) rows(1) label(1 "Nonwhite") label(2 "White"))
			ylabel(0(.05).2)
			xtitle("Age")
			ytitle("Share That Used Medicaid")
			title("", size(medium) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(small)) plotregion(margin(small));
#delimit cr;

graph export "$output/figureA2A2_dmcaid_age_race_sie76.emf", replace

exit
