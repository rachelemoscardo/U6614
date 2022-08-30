clear 
clear matrix
pause on


/*****************************GET DD STATISTICS********************************/
xmluse "$output/figureA2B2_cra_balance_1950_1970.xls", clear  cells(A3:K18) first
replace VARIABLES = subinstr(VARIABLES,"C_Iyear_","",.)	
quietly destring _all, replace ignore(",")
drop in 1
compress

**make locals out of the DD estimates and the F-test p-values for a textbox
foreach var in dpov smh earn g12{
	foreach stat in mdv l lse{
		cap sum w_`var' if VARIABLES=="`stat'"
		cap local w_`var'_`stat' : display %04.2f r(mean)
		
		sum nw_`var' if VARIABLES=="`stat'"
		local nw_`var'_`stat' : display %04.2f r(mean)
	}
}

foreach var in  er gr{
	foreach stat in mdv{
		local w_`var'_`stat' = "      "
		
		sum nw_`var' if VARIABLES=="`stat'"
		local nw_`var'_`stat' : display %03.2f r(mean)
	}
	foreach stat in l lse{
		local w_`var'_`stat' = "     "
		
		sum nw_`var' if VARIABLES=="`stat'"
		local nw_`var'_`stat' : display %05.4f r(mean)
	}
}

local stat = "mdv"
local var  ="earn"
cap sum w_`var' if VARIABLES=="`stat'"
cap local w_`var'_`stat' : display %04.0f r(mean)

sum nw_`var' if VARIABLES=="`stat'"
local nw_`var'_`stat' : display %04.0f r(mean)


	
drop if _n>=7
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
local _dpov "A. Child Poverty"
local _smh "B. Living w/o Father"
local _earn "C. Earnings"
local _g12 "D. 12+ Years of Schooling"
local _er "E. Nonwhite/White Earnings Ratio"
local _gr "F. Nonwhite/White Schooling Ratio"

cap gen w_er1 = .
cap gen w_er0 = .
cap gen w_gr1 = .
cap gen w_gr0 = .

local ydpov = 8
local ysmh = 2.25
local yearn = 750 
local yg12 = 7.5
local yer = .025
local ygr = .0125

foreach var in dpov smh earn g12 er gr{
	cap drop lb* ub*
	local a = 1
	gen lbw = w_`var'1 - 1.96*w_`var'0 if year~=-1
	gen ubw = w_`var'1 + 1.96*w_`var'0 if year~=-1

	gen lbnw = nw_`var'1 - 1.96*nw_`var'0 if year~=-1
	gen ubnw = nw_`var'1 + 1.96*nw_`var'0 if year~=-1

	
	
	#delimit ;
	twoway (scatter w_`var'1 nw_`var'1 lbw ubw ubnw lbnw year if year>=1950, 
			lpattern(solid solid dot dot dash dash solid solid) 
			lcolor(midblue maroon midblue midblue maroon maroon red blue) 
			lwidth(thick thick medium medium medium medium thick thick)
			msymbol(Sh T i i i i i i) 
			mcolor(midblue maroon midblue midblue maroon maroon red blue) 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			ylabel(, labsize(medium))
			xlabel(1950(10)1970, labsize(medium))
			xsize(7.5) ysize(5.5) 			
			legend(order(1 2 3 5) rows(3) label(1 "White") label(2 "Nonwhite") label(3 "White 95% C.I.") label(5 "Nonwhite 95% C.I.") region(style(none)))
			xtitle("Year", size(medium))
			ytitle("", size(medium))
			title("{it:`_`var''}", size(medlarge) color(black))
			text( `y`var'' 1955 "           White   Nonwhite" "MDV:  `w_`var'_mdv'   `nw_`var'_mdv'" "Trend:  `w_`var'_l'    `nw_`var'_l'"  "             (`w_`var'_lse')   (`nw_`var'_lse')", size(small) j(left) box fcolor(white) margin(small))
			graphregion(fcolor(white) color(white) icolor(white) margin(medsmall)))
			;
	#delimit cr;	
graph save "$output/balance_`var'.gph", replace
}

grc1leg "$output/balance_dpov.gph" "$output/balance_smh.gph" "$output/balance_earn.gph" "$output/balance_g12.gph" "$output/balance_er.gph" "$output/balance_gr.gph", legendfrom("$output/balance_dpov.gph") row(3) col(2) 			graphregion(fcolor(white) color(white) icolor(white) margin(medsmall))

graph display, xsize(8.5) ysize(11)
graph export "$output/figureA2B2_cra_balance_1950_1970.emf", replace

erase "$output/balance_dpov.gph" 
erase "$output/balance_smh.gph" 
erase "$output/balance_earn.gph" 
erase "$output/balance_g12.gph" 
erase "$output/balance_er.gph" 
erase "$output/balance_gr.gph"

exit

