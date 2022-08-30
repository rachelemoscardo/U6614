use "$data/figure1", replace
*set mvp to zero in 1950
replace chmvprate = 0 if year==1950

*drop the NHIS uninsurance observation that doesn't have the Medicaid question
*(see notes of the figure)
replace unins_nhis = . if year==1970

scatter unins_nhis 	unins_guess_shsue ///
		pub_nhis 	chmvprate chmcaidrate year, ///
	c(l n l l l l) cmissing(n n n n n n n n) msymbol(O x i Sh Th) msize(medium large medium medium medium) mcolor(blue blue green green green) ///
	lpattern(solid solid solid solid dash) lwidth(medium medium medium medium medium) lcolor(blue blue green green green) ///
	ylabel(0(.1).4, labsize(medium)) xlabel(1950 1960 1966 1980(10)2000 2012, labsize(medium)) ///
	ytitle("Share of Children Ages 0 to 19", size(medium) color(black)) ///
	xtitle("", size(medium) color(black)) ///
	title("", size(medlarge)  color(black)) ///
	legend(order( - "Uninsured: " 1 2 - " " - "Publicly Insured: " 3 4 5) rows(2) label(1 "NHIS") label(2 "SHSUE") label(3 "NHIS") label(4 "DHEW") label(5 "CMS") bmargin(zero) region(style(none))) ///
	graphregion(fcolor(white) color(white) icolor(white) margin(zero)) plotregion(margin(medsmall))

	graph export "$output/figure1_pub_unins.emf", replace	

	
exit
