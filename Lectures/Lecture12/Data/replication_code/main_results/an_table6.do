use "$data/nnfbs_6469_72", clear

drop if inlist(stfips,2,4,15)

*define covariates
local X "i.faminc#i.year i.plural i.subs i.age i.sex"
local X3 "i.stfips i.year#i.region i.year#i.ymcaid"

gen cov = subs | (~subs & ubc_afdc_1967)

local r replace
*Rerun LPMS jointly and outreg to their own sheet
local r replace
foreach dv in lbw prem datt{
	forval i = 3/3{
		/****NONWHITE****/	
		cap drop HCPOST
		cap drop HC0
		cap drop HC	
		cap drop t0
		gen HCPOST = crate0*post
		gen HC0 = crate0*(year==ymcaid)
		gen t0 = year==ymcaid
		gen HC = crate0	

		cap drop notcov_*
		cap drop cov_*
		for var HC post t0 HC0 HCPOST: gen notcov_X = X*(1-cov)
		for var HC post t0 HC0 HCPOST: gen cov_X = X*cov

		cap drop poor_*
		cap drop nonpoor_*
		for var notcov*: gen poor_X = D150poor*X
		for var notcov*: gen nonpoor_X = (1-D150poor)*X	
		
		for var cov*: gen poor_X = D150poor*X
		for var cov*: gen nonpoor_X = (1-D150poor)*X	
		
		reg `dv' `X`i'' `X' poor* nonpoor* [aw=wt], cluster(stfips)	
		
		*test covered vs. not covered for POOR
		test poor_notcov_HCPOST = poor_cov_HCPOST
		local cnc_poor_p = r(p)

		*test covered vs. not covered for NONPOOR	
		test nonpoor_notcov_HCPOST = nonpoor_cov_HCPOST
		local cnc_nonpoor_p = r(p)	
		
		*test poor vs. non poor for NON-COVERED
		test poor_notcov_HCPOST = nonpoor_notcov_HCPOST
		local pnp_nc_p = r(p)

		*test poor vs. non poor for COVERED	
		test poor_cov_HCPOST = nonpoor_cov_HCPOST
		local pnp_c_p = r(p)	

		*test poor COVERED vs. non poor for NON-COVERED	
		test poor_cov_HCPOST = nonpoor_notcov_HCPOST
		local pcnpnc_p = r(p)			
		
		sum `dv' if ~post & D150poor [aw=wt]
		local mdv = r(mean)
		outreg2 using "$output/table6.xls", `r' keep(*HCPOST) noparen noaster cttop("nw_sepX`i'_`dv'") addstat("Cov=NotCov|POOR", `cnc_poor_p', "Cov=NotCov|~POOR", `cnc_nonpoor_p', "Poor=NotPoor|~Cov", `pnp_nc_p', "Poor=NotPoor|Cov", `pnp_c_p', "Poor+Cov = NotPoor+NotCov", `pcnpnc_p', mdv, `mdv')
		local r append
	}
}




exit

