clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse  "$output/figureA2B1_pretrend_figs.xls", clear  cells(A3:S44) first
replace VARIABLES = subinstr(VARIABLES,"C_Iyear_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach var in amrch imr lbwr bpc im ih{
	foreach stat in mdv F p{
		sum w_`var' if VARIABLES=="`stat'"
		local w_`var'_`stat' : display %04.2f r(mean)
		
		sum nw_`var' if VARIABLES=="`stat'"
		local nw_`var'_`stat' : display %04.2f r(mean)
	}
}
	
drop if _n>=33
cap destring VARIABLES, replace
ren VARIABLES year
gen b = year<.
local obs = _N
forval i = 2/`obs'{
	local j = `i'-1
	replace year = year[`j'] in `i' if year[`i']==.
}

reshape wide nw* w*, i(year) j(b)

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
local _amrch "A. Child Mortality"
local _imr "B. Infant Mortality"
local _lbwr "C. Low Birth Weight"
local _bpc "D. Hosp. Beds per 1,000"
local _im "E. Medical Insurance per 1,000"
local _ih "F. Hospital Insurance per 1,000"

foreach var in amrch imr lbwr bpc im ih{
	cap drop lb* ub*
	local a = 1
*	gen lbw = w_`var'1 - 1.96*w_`var'0 if year~=-1
*	gen ubw = w_`var'1 + 1.96*w_`var'0 if year~=-1
	gen lbw = .
	gen ubw = .
	gen lbnw = nw_`var'1 - 1.96*nw_`var'0 if year~=-1
	gen ubnw = nw_`var'1 + 1.96*nw_`var'0 if year~=-1
*	qui sum lbnw
*	local m = r(min)
	
	cap drop wl 
	qui reg w_`var'1 year
	predict wl
	cap drop nwl 
	qui reg nw_`var'1 year
	predict nwl
	
	#delimit ;
	twoway (scatter w_`var'1 nw_`var'1 lbw ubw ubnw lbnw wl nwl year if year>=1950, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon blue red) 
			lwidth(thick thick medium medium medium medium medthick medthick)
			msymbol(Sh T i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon blue red)  
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			yline(0, lcolor(black))
			ylabel(, labsize(medium))
			xlabel(1950(5)1965, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(order(1 2 3 5) rows(3) label(1 "White") label(2 "Nonwhite") label(3 "White 95% C.I.") label(5 "Nonwhite 95% C.I.") region(style(none)) size(medium))
			xtitle("Year", size(medium))
			ytitle("", size(medium))
			title("{it:`_`var''}", size(medlarge) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(medsmall)))
			;
	#delimit cr;	
graph save "$output/balance_`var'.gph", replace
}

grc1leg "$output/balance_amrch.gph" "$output/balance_imr.gph" "$output/balance_lbwr.gph" "$output/balance_bpc.gph" "$output/balance_im.gph" "$output/balance_ih.gph", legendfrom("$output/balance_ih.gph") row(3) col(2) 			graphregion(fcolor(white) color(white) icolor(white) margin(medsmall))

graph display, xsize(8.5) ysize(11)
graph export "$output/figureA2B1_pretrend_figs.emf", replace

erase "$output/balance_amrch.gph" 
erase "$output/balance_imr.gph" 
erase "$output/balance_lbwr.gph" 
erase "$output/balance_bpc.gph" 
erase "$output/balance_im.gph" 
erase "$output/balance_ih.gph"

exit

