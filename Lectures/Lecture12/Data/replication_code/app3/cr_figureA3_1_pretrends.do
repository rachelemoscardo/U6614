
clear
clear matrix
set matsize 2000
cap log close
local r replace

use "$data/vs_mcaid" if inrange(year,1950,1979), clear

drop if inlist(stfips,2,4,15)

drop exp
gen exp = year - ymcaid
recode exp (-1000/-17=-17) (10/100=10)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X


gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

gen lnamrch = ln(amrch)


*nonwhite
xi: reg lnamrch i.ymcaid*year if year<=1965 & ~white [aw=popch], cluster(stfips)
testparm _IymcX*
local pnw : display %04.3f round(r(p),.001)

xi: reg lnamrch i.ymcaid*year if inrange(year,1959,1965) & ~white [aw=popch], cluster(stfips)
testparm _IymcX*
local pnw2 : display %07.5f round(r(p),.001)

xi i.ymcaid*year, noomit
reg lnamrch _I* if year<=1965 & ~white [aw=popch], cluster(stfips) nocon
predict pretrends if e(sample)
forval y = 1966/1970{
	local c`y'_nw : display %05.2f round(_b[_Iymcaid_`y']*100)/100
	local b`y'_nw : display %04.2f round(_b[_IymcXyea_`y']*100)/100
}

*white
xi: reg lnamrch i.ymcaid*year if year<=1965 & white [aw=popch], cluster(stfips)
testparm _IymcX*
local pw : display %04.3f round(r(p),.001)

xi i.ymcaid*year, noomit
reg lnamrch _I* if year<=1965 & white [aw=popch], cluster(stfips) nocon
predict pretrends_w if e(sample)
replace pretrends = pretrends_w if e(sample)
drop pretrends_w
forval y = 1966/1970{
	local c`y'_w : display %05.2f round(_b[_Iymcaid_`y']*100)/100
	local b`y'_w : display %04.2f round(_b[_IymcXyea_`y']*100)/100
}


collapse (mean) lnamrch pretrends [aw=popch], by(year ymcaid white)
reshape wide lnamrch pretrends, i(year ymcaid) j(white)
reshape wide lnamrch* pretrends*, i(year) j(ymcaid)

*right now this is set up to show trends up to the Medicaid year...which is right?
scatter lnamrch0* pretrends0* year if year<=1965, ///
c(l l l l l l l l l l ) lpattern(dash dash dash dash dash solid solid solid solid solid) lcolor(red blue green dkorange gray red blue green dkorange gray) /// 
mcolor(red blue green dkorange gray red blue green dkorange gray) msymbol(Oh Th Sh i Dh O T S X D) ///
xlabel(1950(5)1975 1979) ///
xtitle("") ytitle("log Age-Adjusted Child Mortality Rate" " ") ///
legend(order(- "Medicaid: " 1 2 3 4 5) row(1) label(1 "1966") label(2 "1967") label(3 "1968") label(4 "1969") label(5 "1970") region(style(none))) ///
graphregion(fcolor(white) color(white) icolor(white) margin(small)) ///
|| ///
scatter lnamrch0* year if year>1965, ///
c(l l l l l l l l l l ) lpattern(dash dash dash dash dash solid solid solid solid solid) lcolor(red blue green dkorange gray red blue green dkorange gray) /// 
mcolor(red blue green dkorange gray red blue green dkorange gray) msymbol(Oh Th Sh i Dh O T S X D) ///
xtitle("") ytitle("log Age-Adjusted Child Mortality Rate" " ") title("{it: B. Nonwhite Child Mortality}") ///
graphregion(fcolor(white) color(white) icolor(white) margin(small)) || ///
pcarrowi 5.65 1965 5.9 1963, lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) ///
text(5.6 1964 "H{subscript:0}: 1959-1965 linear trends are equal." "        {it:p}-value = `pnw'", j(left)) ///
saving("$output/pretrends_nw", replace)

scatter lnamrch1* pretrends1* year if year<=1965, ///
c(l l l l l l l l l l ) lpattern(dash dash dash dash dash solid solid solid solid solid) lcolor(red blue green dkorange gray red blue green dkorange gray) /// 
mcolor(red blue green dkorange gray red blue green dkorange gray) msymbol(Oh Th Sh i Dh O T S X D) ///
xlabel(1950(5)1975 1979) ///
xtitle("") ytitle("log Age-Adjusted Child Mortality Rate" " ") ///
legend(order(- "Medicaid: " 1 2 3 4 5) row(1) label(1 "1966") label(2 "1967") label(3 "1968") label(4 "1969") label(5 "1970") region(style(none))) ///
graphregion(fcolor(white) color(white) icolor(white) margin(small)) ///
|| ///
scatter lnamrch1* year if year>1965, ///
c(l l l l l l l l l l ) lpattern(dash dash dash dash dash solid solid solid solid solid) lcolor(red blue green dkorange gray red blue green dkorange gray) /// 
mcolor(red blue green dkorange gray red blue green dkorange gray) msymbol(Oh Th Sh i Dh O T S X D) ///
xtitle("") ytitle("log Age-Adjusted Child Mortality Rate" " ") title("{it: A. White Child Mortality}") ///
graphregion(fcolor(white) color(white) icolor(white) margin(small)) || ///
pcarrowi 5.05 1965 5.3 1963, lcolor(black) mcolor(black) lwidth(medium) mlwidth(medium) ///
text(5 1964 "H{subscript:0}: 1959-1965 linear trends are equal." "        {it:p}-value = `pw'", j(left)) ///
saving("$output/pretrends_w", replace)



grc1leg "$output/pretrends_w" "$output/pretrends_nw", col(1) row(2) imargin(tiny)  graphregion(fcolor(white) color(white) icolor(white) margin(tiny)),
graph display, xsize(7) ysize(10) 
graph export "$output/figureA3_1_pretrends.wmf", replace

erase "$output/pretrends_w.gph" 
erase "$output/pretrends_nw.gph"

di "NW test from 1959: `pnw2'"

