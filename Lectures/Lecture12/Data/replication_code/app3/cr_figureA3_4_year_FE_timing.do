xmluse  "$output/figureA3_3_year_FE_timing.xls", clear  cells(A3:E62) first
replace VARIABLES = subinstr(VARIABLES,"RXYyear_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress
ren VARIABLES year
gen b = year<.
replace year = year[_n-1] if year[_n]==.
reshape wide ES* DD*, i(year) j(b)


gen eslb = ES1 - 1.96*ES0 
gen esub = ES1 + 1.96*ES0

gen ddlb = DD1 - 1.96*DD0 
gen ddub = DD1 + 1.96*DD0 


	#delimit ;
	twoway (scatter ES1 DD1 year, 
			lpattern(solid solid dot dot dash dash) 
			lcolor(forest_green purple navy navy maroon maroon ) 
			lwidth(thick thick medium medium medium medium)
			msymbol(Oh X i i i i) 
			mcolor(forest_green purple navy navy maroon maroon ) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			yline(0, lcolor(black)) 
			xlabel(1950(5)1975 1979, labsize(medium))
			ylabel(0(-.3)-1.2, labsize(medium))
			legend(order(1 2) rows(2) label(1 "Event-Study") label(2 "Diff-in-Diff") region(style(none)) size(medium) ring(0) bplace(sw) bmargin(large))
			xtitle("Year", size(medlarge))
			ytitle("Year FE", size(medlarge))
			title("{it:A. Year FE in Timing-Only Models}", size(large) color(black))			
			graphregion(fcolor(white) color(white) icolor(white) margin(small)) 
			saving("$output/yearfe_a.gph", replace))
			;
	#delimit cr;	

	#delimit ;
	twoway (scatter ESHC1 DDHC1 year, 
			lpattern(solid solid dot dot dash dash) 
			lcolor(navy maroon navy navy maroon maroon ) 
			lwidth(thick thick medium medium medium medium)
			msymbol(T Sh i i i i) 
			mcolor(navy maroon navy navy maroon maroon ) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			yline(0, lcolor(black)) 
			xlabel(1950(5)1975 1979, labsize(medium))
			ylabel(0(-.3)-1.2, labsize(medium))
			legend(order(1 2) rows(2) label(1 "Event-Study") label(2 "Diff-in-Diff") region(style(none)) size(medium) ring(0) bplace(sw) bmargin(large))
			xtitle("Year", size(medlarge))
			ytitle("Year FE", size(medlarge))
			title("{it:B. Year FE in AFDC{superscript:*}{subscript:s} Models}", size(large) color(black))			
			graphregion(fcolor(white) color(white) icolor(white) margin(small)) 
			saving("$output/yearfe_b.gph", replace))
			;
	#delimit cr;		
graph combine "$output/yearfe_a.gph" "$output/yearfe_b.gph", rows(2) graphregion(fcolor(white) color(white) icolor(white) margin(small)) 
graph display, 	xsize(5.8) ysize(7.975) 
graph export "$output/figureA3_4_year_FE_timing.wmf", replace

erase "$output/yearfe_a.gph" 
erase "$output/yearfe_b.gph"
