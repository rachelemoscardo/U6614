use "$datatemp/pbs_atet_gbnwkids_corr", clear
gen corr0 = 0
gen stat0 = .
local i = 1

*for adding the CI from figure IX
foreach var of varlist atet lci uci{
	sum `var' if _n==11
	local `var'0 : display %04.2f round(r(mean)*100)/100
	replace stat0 = ``var'0' in `i'
	local i = `i' +1
}

*for labelling the ATET/LCI/UCI
foreach var of varlist atet lci uci{
	sum `var' if _n==1
	local `var'1 : display %04.2f round(r(mean)*100)/100
}
local atet1 = `atet1'-.05
local lci1 = `lci1'-.05
local uci1 = `uci1'-.05

	#delimit ;
	twoway (scatter atet lci uci corr, 
			lpattern(solid dash dash) 
			lcolor(navy navy navy) 
			lwidth(thick medium medium)
			msymbol(i i i i i i) 
			mcolor() 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			ylabel(-1(.25)0)
			legend(off)
			xtitle("Correlation between ITT and First Stage", size(medium))
			ytitle("ATET", size(medium))
			title("{it:A. Nonwhite Children, 0-14}", size(medlarge) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero))
			text(`atet1' .8 "ATET: `atet0'", size(small))
			text(`lci1' .85 "Lower 95% C.I.", size(small))
			text(`uci1' .85 "Upper 95% C.I.", size(small))
			)
			(scatter stat0 corr0 if _n==1, 
			msymbol(O Oh Oh) 
			mcolor(navy navy navy) 
			msize(large large large)
			lcolor(navy)
			c(l l l)
			cmissing(y y y) 
			ylabel(-1(.25)0)
			legend(off)
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			)
			(scatter stat0 corr0 if _n>1, 
			msymbol(+) 
			mcolor(navy navy navy) 
			msize(large large large)
			mlwidth(thick)
			lcolor(navy)
			c(l l l)
			cmissing(y y y) 
			ylabel(-1(.25)0)
			legend(off)
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			)
			(pcarrowi -.68 -.5 `lci0' -.05, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(-.78 -.5 "C.I. with zero correlation:" "[`lci0',`uci0']"), j(center)
			)
			;
	#delimit cr;

graph save "$datatemp/atet_corr_kids", replace	


use "$datatemp/pbs_atet_gbnwnnmr_corr", clear
gen corr0 = 0
gen stat0 = .
local i = 1
foreach var of varlist atet lci uci{
	sum `var' if _n==11
	local `var'0 : display %04.2f round(r(mean)*100)/100
	replace stat0 = ``var'0' in `i'
	local i = `i' +1
}

*for labelling the ATET/LCI/UCI
foreach var of varlist atet lci uci{
	sum `var' if _n==1
	local `var'1 : display %04.2f round(r(mean)*100)/100
}
local atet1 = `atet1'-.05
local lci1 = `lci1'-.05
local uci1 = `uci1'-.05

	#delimit ;
	twoway (scatter atet lci uci corr, 
			lpattern(solid dash dash) 
			lcolor(navy navy navy) 
			lwidth(thick medium medium)
			msymbol(i i i i i i) 
			mcolor() 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			ylabel(-1(.25)0)
			legend(off)
			xtitle("Correlation between ITT and First Stage", size(medium))
			ytitle("", size(medium))
			title("{it:B. Nonwhite Neonates}", size(med large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(`atet1' .8 "ATET: `atet0'", size(small))
			text(`lci1' .85 "Lower 95% C.I.", size(small))
			text(`uci1' .85 "Upper 95% C.I.", size(small))
			)
			(scatter stat0 corr0 if _n==1, 
			msymbol(O Oh Oh) 
			mcolor(navy navy navy) 
			msize(large large large)
			lcolor(navy)
			c(l l l)
			cmissing(y y y) 
			ylabel(-1(.25)0)
			legend(off)
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			)
			(scatter stat0 corr0 if _n>1, 
			msymbol(+) 
			mcolor(navy navy navy) 
			msize(large large large)
			mlwidth(thick)
			lcolor(navy)
			c(l l l)
			cmissing(y y y) 
			ylabel(-1(.25)0)
			legend(off)
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			)
			(pcarrowi -.68 -.5 `lci0' -.05, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(-.78 -.5 "C.I. with zero correlation" "[`lci0',`uci0']"), j(center)
			)
			;
	#delimit cr;
graph save "$datatemp/atet_corr_nnmr", replace

			

use "$datatemp/pbs_atet_gbnw14_corr", clear
gen corr0 = 0
gen stat0 = .
local i = 1
foreach var of varlist atet lci uci{
	sum `var' if _n==11
	local `var'0 : display %04.2f round(r(mean)*100)/100
	replace stat0 = ``var'0' in `i'
	local i = `i' +1
}

*for labelling the ATET/LCI/UCI
foreach var of varlist atet lci uci{
	sum `var' if _n==1
	local `var'1 : display %04.2f round(r(mean)*100)/100
}
local atet1 = `atet1'-.05
local lci1 = `lci1'+.2
local uci1 = `uci1'-.06

	#delimit ;
	twoway (scatter atet lci uci corr, 
			lpattern(solid dash dash) 
			lcolor(navy navy navy) 
			lwidth(thick medium medium)
			msymbol(i i i i i i) 
			mcolor() 
			c(l l l l l l l l l) 
			cmissing(n n n n n n n) 
			ylabel(-1.5(.25)0)
			legend(off)
			xtitle("Correlation between ITT and First Stage", size(medium))
			ytitle("ATET", size(medium))
			title("{it:C. Nonwhite Children, 1-4}", size(medlarge) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			text(`atet1' .8 "ATET: `atet0'", size(small))
			text(`lci1' .85 "Lower 95% C.I.", size(small))
			text(`uci1' .85 "Upper 95% C.I.", size(small))
			)
			(scatter stat0 corr0 if _n==1, 
			msymbol(O Oh Oh) 
			mcolor(navy navy navy) 
			msize(large large large)
			lcolor(navy)
			c(l l l)
			cmissing(y y y) 
			ylabel(-1.5(.25)0)
			legend(off)
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			)
			(scatter stat0 corr0 if _n>1, 
			msymbol(+) 
			mcolor(navy navy navy) 
			msize(large large large)
			mlwidth(thick)
			lcolor(navy)
			c(l l l)
			cmissing(y y y) 
			ylabel(-1.5(.25)0)
			legend(off)
			graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
			)
			(pcarrowi -1.22 -.5 `lci0' -.05, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
			text(-1.35 -.5 "C.I. with zero correlation" "[`lci0',`uci0']"), j(center)
			)
			;
	#delimit cr;
graph save "$datatemp/atet_corr_14", replace

	
graph combine "$datatemp/atet_corr_kids.gph" "$datatemp/atet_corr_nnmr.gph" "$datatemp/atet_corr_14.gph", col(2) row(2) imargin(tiny) graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 
	
graph export "$output/figureA4_1_atet_corr.emf", replace
	
erase "$datatemp/atet_corr_kids.gph" 
erase "$datatemp/atet_corr_nnmr.gph" 
erase "$datatemp/atet_corr_14.gph"


	
