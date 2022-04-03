/*********************************************************************
*Create a figure of point estimates and bootstrap C.I.s for the ATETs*
*********************************************************************/
use "$datatemp/pbs_atet.dta", clear

*drop the SBE ATETs and reshuffle to match old code
drop in 4/6
gen ord = _n
recode ord (4=6) (5=4) (6=5)
replace ord = 7-ord
ren lci ci0
ren uci ci1
reshape long ci, i(ord) j(testo)
replace ci = ci*100
replace atet = atet*100

*create locals for labels, etc
local p6 "Goodman-Bacon (2016)" 
local p61 "Nonwhite Children 0-14"
local p5 "Goodman-Bacon (2016)"
local p51 "Nonwhite Children 1-4"
local p4 "Goodman-Bacon (2016)"
local p41 "Nonwhite Neonatal Infants"
local p3 "Currie and Gruber (1996b)/" 
local p3a "Dave et al. (2008)" 
local p31 "Children 1-14"
local p2 "Currie and Gruber (1996a)/" 
local p2a "Cutler and Gruber (1996)" 
local p21 "Infants"
local p1 "Wherry and Meyer (2015)/"
local p1a "     Card and Shore-Sheppard (2011)"
local p11 "Black Teens"

forval i = 1/6{
	sum atet if ord==`i'
	local m`i' : display %4.0f round(r(mean),1 )
}


twoway ///
(scatter ord ci 	if ord==1, c(l) msymbol(+) mcolor(gray) msize(large) lcolor(gray) text(1.25 `m1' "`m1'%") ) ///
(scatter ord atet 	if ord==1, c(l) msymbol(O) mcolor(gray) msize(medium) lcolor(gray) text(1 100 "`p1'" "`p1a'" "`p11'", size(small))) ///
(scatter ord ci 	if ord==2, c(l) msymbol(+) mcolor(gray) msize(large) lcolor(gray) text(2.25 `m2' "`m2'%") ) ///
(scatter ord atet 	if ord==2, c(l) msymbol(O) mcolor(gray) msize(medium) lcolor(gray) text(2 100 "`p2'" "`p2a'" "`p21'", size(small))) ///
(scatter ord ci 	if ord==3, c(l) msymbol(+) mcolor(gray) msize(large) lcolor(gray) text(3.25 `m3' "`m3'%") ) ///
(scatter ord atet 	if ord==3, c(l) msymbol(O) mcolor(gray) msize(medium) lcolor(gray) text(3 100 "`p3'" "`p3a'" "`p31'", size(small))) ///
(scatter ord ci 	if ord==4, c(l) msymbol(+) mcolor(blue) msize(large) lcolor(blue) text(4.25 `m4' "`m4'%") ) ///
(scatter ord atet 	if ord==4, c(l) msymbol(O) mcolor(blue) msize(medium) lcolor(blue) text(4 100 "`p4'" "`p41'", size(small))) ///
(scatter ord ci 	if ord==5, c(l) msymbol(+) mcolor(blue) msize(large) lcolor(blue) text(5.25 `m5' "`m5'%") ) ///
(scatter ord atet 	if ord==5, c(l) msymbol(O) mcolor(blue) msize(medium) lcolor(blue) text(5 100 "`p5'" "`p51'", size(small))) ///
(scatter ord ci 	if ord==6, c(l) msymbol(+) mcolor(blue) msize(large) lcolor(blue) text(6.25 `m6' "`m6'%") ) ///
(scatter ord atet 	if ord==6, c(l) msymbol(O) mcolor(blue) msize(medium) lcolor(blue) text(6 100 "`p6'" "`p61'", size(small)) ///
/*text(6.5 100 "Paper:", size(medium)) */ ///
graphregion(fcolor(white) color(white) icolor(white) margin(zero)) ///
legend(off) ///
ylabel(.8 6.5, labsize(zero) labcolor(white) tstyle(none) nogrid) ytitle("")  ///
xlabel(-500 "-500%" -400 "-400%" -300 "-300%" -200 "-200%" -100 "-100%" 0 "0%") xscale(range(-500 200)) xline(0, lcolor(gray) lpattern(dash)) xline(-100, lcolor(gray) lpattern(dash)) ///
xtitle("Percent Reduction in Mortality Among Medicaid Recipients                               ", size(medsmall) ) ///
 )

graph export "$output/figure10_atet.emf" , replace

exit

*title("Proportional Average Treatment Effect on the Treated") ///
*t1title("with 95% Parametric Bootstrap C.I.", size(medsmall)) ///
