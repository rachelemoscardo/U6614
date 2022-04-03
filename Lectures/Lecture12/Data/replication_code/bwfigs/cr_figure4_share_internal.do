*read in from NBER and save a temp version to read in for each cleaning file
use "http://www.nber.org/mortality/1959/mort1959.dta", clear
gen icd			= real(substr(ucod,1,3))
gen shint = icd<800
gen ageunit		= real(substr(string(age),1,1)) if length(string(age))==3
recode ageunit	(.=0)
gen agenumber	= real(substr(string(age),-2,.))
drop if ageunit==1|ageunit==9
replace agenumber = 0 if ageunit>0
replace agenumber = age if agenumber==.
cap drop age
ren agenumber age
ren shint shint59
collapse (mean) shint, by(age)
save "$datatemp/shint", replace

use "http://www.nber.org/mortality/1965/mort1965.dta", clear
gen icd			= real(substr(ucod,1,3))
gen shint = icd<800
gen ageunit		= real(substr(string(age),1,1)) if length(string(age))==3
recode ageunit	(.=0)
gen agenumber	= real(substr(string(age),-2,.))
drop if ageunit==1|ageunit==9
replace agenumber = 0 if ageunit>0
replace agenumber = age if agenumber==.
cap drop age
ren agenumber age
collapse (mean) shint, by(age)
ren shint shint65
merge 1:1 age using "$datatemp/shint"
drop _merge
save "$datatemp/shint", replace

use "http://www.nber.org/mortality/1971/mort1971.dta", clear
gen icd			= real(substr(ucod,1,3))
gen shint = icd<800
gen ageunit		= real(substr(string(age),1,1)) if length(string(age))==3
recode ageunit	(.=0)
gen agenumber	= real(substr(string(age),-2,.))
drop if ageunit==1|ageunit==9
replace agenumber = 0 if ageunit>0
replace agenumber = age if agenumber==.
cap drop age
ren agenumber age
collapse (mean) shint, by(age)
ren shint shint71
merge 1:1 age using "$datatemp/shint"
save "$datatemp/shint", replace


use $datatemp/shint,clear
#delimit ;
scatter shint59 shint65 shint71 age if age<=65, 
			lpattern(solid solid solid dash dash dash dash) 
			lcolor(gray black gray ) 
			lwidth(medthick medthick medthick medium medium medium medium )
			msymbol(i Th O i i i i) 
			mcolor(gray black gray) 
			c(l l l l l l l l l) 
			cmissing(y y y y n n) 
			legend(rows(3) label(1 "1959") label(2 "1965") label(3 "1971") ring(0) bplace(se) bmargin(large))
			xtitle("Age", size(medlarge) )
			ytitle("", size(medlarge) )
			xlabel(, labsize(medlarge) )
			ylabel(.2(.2)1, labsize(medlarge) )
			yaxis(1)
			title("", size(large) color(black))
			graphregion(fcolor(white) color(white) icolor(white) margin(small)) plotregion(margin(small)) 
			;
#delimit cr;

graph export "$output/figure4_share_internal.emf", replace


