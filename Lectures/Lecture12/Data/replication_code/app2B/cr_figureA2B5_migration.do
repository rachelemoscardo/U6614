use "$data/balance", clear

cap drop w
cap drop uw
reg pf crate0 [aw=p65], robust
local b: display %03.1f _b[crate0]
local bse: display %03.1f _se[crate0]
predict w
reg pf crate0, robust
local buw: display %03.1f  _b[crate0]
local buwse: display %03.1f  _se[crate0]
predict uw

#delimit ;
scatter pf uw w crate0 [aw=p65], 
msym(Oh i i) c(n l l) lwidth(i thick thick) 
ytitle("Net Population Flows") xtitle("AFDC{superscript:*}") 
legend(off) 
graphregion(fcolor(white) color(white) icolor(white) margin(zero)) 
text(-15000 5 "WLS: `b'  (s.e. = `bse')" "OLS: 	`buw'    (s.e. = `buwse')", j(left) size(small) box fcolor(white) margin(small));

graph display, 	xsize(7.5) ysize(5.5) ;
graph export "$output/figureA2B5_migration.emf", replace;

exit

