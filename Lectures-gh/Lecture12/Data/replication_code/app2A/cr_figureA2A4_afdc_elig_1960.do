use "$data/balance", clear

reg crate elig if year==1960 & white
	local bw_uw : display %04.2f round(_b[elig]*100)/100
	local sew_uw : display %04.2f round(_se[elig]*100)/100
predict pw if e(sample)
rreg crate elig if year==1960  & white
	local bw_rr : display %04.2f round(_b[elig]*100)/100
	local sew_rr : display %04.2f round(_se[elig]*100)/100
reg crate elig if year==1960 & white [aw=crpop]
	local bw : display %04.2f round(_b[elig]*100)/100
	local sew : display %04.2f round(_se[elig]*100)/100
	local r2w : display %04.2f round(e(r2)*100)/100


reg crate elig if year==1960 & ~white
	local bnw_uw : display %04.2f round(_b[elig]*100)/100
	local senw_uw : display %04.2f round(_se[elig]*100)/100
predict pnw if e(sample)
rreg crate elig if year==1960 & ~white
	local bnw_rr : display %04.2f round(_b[elig]*100)/100
	local senw_rr : display %04.2f round(_se[elig]*100)/100
reg crate elig if year==1960 & ~white [aw=crpop]
	local bnw : display %04.2f round(_b[elig]*100)/100
	local senw : display %04.2f round(_se[elig]*100)/100
	local r2nw : display %05.3f round(e(r2)*1000)/1000


	#delimit ;
	twoway (scatter crate pw elig if white & year==1960 [aw=crpop], 
			lpattern(none solid) 
			lcolor(maroon maroon) 
			lwidth(thick )
			msymbol(Th i) 
			mcolor(maroon) 
			c(n l l l l l l l l) 
			cmissing(n n n n n n n) 
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("", size(medium))
			ytitle("1960 AFDC Participation", size(medium))
			title("{it:A. White Women}", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(.0325 .02 "Slope: `bw' (s.e. = `sew')" "R{superscript:2} = `r2w'" , j(left) size(medium) fcolor(white) margin(small))
			)
			(pcarrowi .03 .02 .02 .035 , lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			saving("$output/panel_white.gph", replace));
	#delimit cr;	


	
	#delimit ;
	twoway (scatter crate pnw elig if ~white & year==1960 [aw=crpop], 
			lpattern(none solid) 
			lcolor(forest_green forest_green) 
			lwidth(thick )
			msymbol(Oh i) 
			mcolor(forest_green) 
			c(n l l l l l l l l) 
			cmissing(n n n n n n n) 
			xsize(7.5) ysize(5.5) 			
			legend(off)
			xtitle("1960 AFDC Eligibility", size(medium))
			ytitle("1960 AFDC Participation", size(medium))
			title("{it:B. Nonwhite Women}", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(.23 .04 "Slope: `bnw' (s.e. = `senw')" "R{superscript:2} = `r2nw'", j(left) size(medium) fcolor(white) margin(small))
			)
			(pcarrowi .2 .02 .065 .05 , lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		saving("$output/panel_nonwhite.gph", replace));
	#delimit cr;	


	graph combine "$output/panel_white.gph" "$output/panel_nonwhite.gph", row(2) col(1) xsize(5.5) ysize(8) graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
	
	graph export "$output/figA2A4_afdc_elig.emf", replace
	
erase "$output/panel_white.gph" 
erase "$output/panel_nonwhite.gph"
	
exit	
