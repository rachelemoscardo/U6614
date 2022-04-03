use "$data/vs_mcaid" if inrange(year,1959,1979), clear
drop if inlist(stfips,2,4,15)

char exp[omit] -1
xi i.exp, pref(_T)
for var _T*: gen HCX = crate0*X

egen D 			= cut(exp), at(-8,-7,-1,0,1,5,10,100)
char D[omit] -1
xi i.D, pref(_D)
for var _D*: gen HCX = crate0*X

gen POST  		= exp>0 & exp<10
gen HCPOST		= crate0*POST

for var hmr1-hmr24 dmr1-dmr27: gen lnX = ln(X)

local X = "i.stfips i.year*i.region i.year*i.ymcaid pcinc hpc bpc HC_Texp_1 HC_Texp_9 HCPOST HC_Texp_19"
local r replace

/***********/
/**By Hour**/
/***********/
forval h = 1/24{
	*Mean DV
		qui sum hmr`h' if exp==-1 & hc [aw=births]
		local mdv = r(mean)
	*Run DD Model, store treatment effect
		xi: reg lnhmr`h' `X' [aw=births] if ~white, cluster(stfips)
	outreg2 using "$output/figure8.xls", `r' keep(HCPOST) noparen noaster ctitle("h`h'") addstat(mdv, `mdv')
	local r append
}

/***********/
/**By Day**/
/***********/
forval d = 1/27{
	*Mean DV
		qui sum dmr`d' if exp==-1 & hc [aw=births]
		local mdv = r(mean)
	*Run DD Model, store treatment effect
		xi: reg lndmr`d' `X' [aw=births] if ~white, cluster(stfips)
	outreg2 using "$output/figure8.xls", `r' keep(HCPOST) noparen noaster ctitle("d`d'") addstat(mdv, `mdv')
}



