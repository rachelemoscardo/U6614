/**************************************************************
This code implements a parametric boostrap procedure to:
1. generate confidence intervals for the point estimate of the ATET
**************************************************************/


clear all
set more off
capture log close
log using "$output/pbs_atet", replace text
program drop _all


/****************************************************/
/*This program implements the parametric bootstrap	*/
/*once a one-line dataset of effects/parameters is	*/
/*in the memory.  It saves a file of size `B'		*/
/*with the bootstrap reps of ATET and appends		*/
/*the ATET and modified-percentile-method CIs to a 	*/
/*short file for graphing							*/
/****************************************************/
program define pbs
	*1. Step one was to get the parameters in and make globals out of them...done outside the loop
	*2. Define globals to loop through the bootstrap reps
	global paper = paper[1]
	drop paper
	foreach var of varlist _all{
		qui sum `var'
		global `var' = r(mean)
	}


	*********************	
	*ATET Point Estimate:
	*1. Adjusted ITT (only adjusted for Meyer and Wherry: 0.8 yrs of elig)
	*2. Adjusted FS (only adjusted for underreporting)
	*3. Poor/Overall Mortality Ratio from surveys times reported baseline overall mortality in paper
	*********************
	global ATET = ($itt/$adj)/($fs/$underreport)/(($dpov/$pov)*$mrreport)

	if "`1'"=="gbnwkids" | "`1'"=="gbnw14"{			
		global ATET = ($itt/$adj)/($fs/$underreport)/((($dpovch83/$povch83)/($dpovad83/$povad83))*($dpovad68/$povad68))
	}
	
	/*********************************************************************
					*****START THE BOOTSTRAP******
	**********************************************************************/
	clear
	/**NUMBER OF REPS**/
	local B 10000
	set obs `B'
	gen iter = _n
	set seed 55555

	**************************
	*Dataset of bootstrap reps
	*1. For the coefficient estimates: use normal draws because they are regression coefs and I can rely on the CLT
	gen itt = rnormal($itt,$ittse)
	gen fs  = rnormal($fs, $fsse)
	save "$datatemp/atet_`1'", replace


	*2. for the mortality ratios: use the uniform method based on survey sample sizes for POOR and OVERALL mortality rates (avoids impossible draws)
	*this needs to be broken up for papers where I CAN get an estimate of poor and overall mortaltiy 
	*and those where I can't (like child mort in the 60s)

	*baseline mortality bootstrap for papers with "direct" estimates
	if "`1'"~="gbnwkids" & "`1'"~="gbnw14"{
			clear
			set obs `B'
			gen iter = _n
			save "$datatemp/sim_mr", replace
			
			*now for every mortality rate necessary to calculate the ratio, simulate 10,000 draws from a uniform with survey mean and sample size
			foreach stub in dpov pov{
				clear
				local obs = max($`stub',`B')						/*make the dataset big enough to accommodate both the sample size and the `B' replications*/
				set obs `obs'
				gen iter = _n
				gen sim_`stub' = .
				forval i = 1/`B'{
					*draw the righ number of uniform draws with the specified mean
					*summarize the draw and use this iteration to at a new value of chmr
					qui{
						gen testo = uniform()<$`stub' if _n<=$`stub'n
						sum testo
						replace sim_`stub' = r(mean) in `i'
						drop testo
					}
				}
				keep in 1/`B'
				keep sim_`stub' iter
				merge 1:1 iter using "$datatemp/sim_mr"
				drop _merge
				save "$datatemp/sim_mr", replace
			}
			gen mradjust = ((sim_dpov/sim_pov)*$mrreport)
			save "$datatemp/sim_mr", replace			
	}
		*baseline mortality bootstrap for papers without "direct" estimates: ie child mort in the 1960s
		if "`1'"=="gbnwkids" | "`1'"=="gbnw14"{			
			clear
			set obs `B'
			gen iter = _n
			save "$datatemp/sim_mr", replace
			
			*now for every mortality rate necessary to calculate the ratio, simulate 10,000 draws from a uniform with survey mean and sample size
			foreach stub in dpovad83 povad83 dpovch83 povch83 dpovad68 povad68{
				clear
				local obs = max($`stub',`B')						/*make the dataset big enough to accommodate both the sample size and the `B' replications*/
				set obs `obs'
				gen iter = _n
				gen sim_`stub' = .
				forval i = 1/`B'{
					*draw the righ number of uniform draws with the specified mean
					*summarize the draw and use this iteration to at a new value of chmr
					qui{
						gen testo = uniform()<$`stub' if _n<=$`stub'n
						sum testo
						replace sim_`stub' = r(mean) in `i'
						drop testo
					}
				}
				keep in 1/`B'
				keep sim_`stub' iter
				merge 1:1 iter using "$datatemp/sim_mr"
				drop _merge
				save "$datatemp/sim_mr", replace
			}
			*MR adjustment here is different: ratio of the CHILD-to-ADULT poor/overall ratio in 83 (how much greater is 
			*the child mort disparity in 83?) times the ADULT poor/overall ratio in 68 (apply that factor the observed ratio in the right time period)
			gen mradjust = ((sim_dpovch83/sim_povch83)/(sim_dpovad83/sim_povad83))*(sim_dpovad68/sim_povad68)
			save "$datatemp/sim_mr", replace
		}
		
		
		
		

		
		
		
		
	*3. Combine the normal draws for ITT and FS with the draws for the mortality adjustment
		merge 1:1 iter using "$datatemp/atet_`1'"
		drop _merge

	gen ATET = (itt/$adj)/(fs/$underreport)/mradjust

	save "$datatemp/atet_`1'", replace


	/* There are a very small number of gigantic outliers but just plot the trimmed mean */
	summ ATET, det
	*define bounds of the kdensity plot
	global lower = r(p1)
	global upper = max(r(p99),0.05)

	summ ATET if ATET>=$lower & ATET<=$upper, det


	/***************************************************************************
	*Define naive confidence intervals -- HOW TO REDUCE THEIR SIZE?
	centile ATET, centile(2.5)
	global lci = r(c_1)
	centile ATET, centile(97.5)
	global uci =r(c_1)
	****************************************************************************/

	/***************************************************************************/
	*This program defined C.I.s based on moving a 95% interval up and down starting
	*from (0.025, 0.975) and going up to (0.05, 1).  The algorithm often CHOSE
	*this upper limit, which may be a problem
	*getci
	/***************************************************************************/

	/***************************************************************************/
	*This calculated the CIs by taking 95% on either side of the ATET
	*it lets the lower end move up a bit from the naive method but ensures
	*that I never have the extremes of the bootstrap draws as the top/bottom of the C.I.
	/***************************************************************************/
	centile ATET if ATET<=$ATET, centile(5)
	global lci = r(c_1)

	centile ATET if ATET>=$ATET, centile(95)
	global uci = r(c_1)
	 
	clear
	set obs 1
	gen atet = $ATET
	gen lci = $lci
	gen uci = $uci 
	gen paper = "$paper"
	append using "$datatemp/pbs_atet"
	save "$datatemp/pbs_atet", replace

end

*save a blank file to append the CIs to
clear	
save "$datatemp/pbs_atet", replace emptyok
foreach type in cginf cgkids mwteens sbead sbewad sbenwad gbnwnnmr gbnw14 gbnwkids{

	/*********************************************************************/
					*****GET THE PARAMETERS******
	/**********************************************************************/	
	*1. Read in spreadsheet of reduced form/ITT estimates, first stage estimates, and baseline mortality calculations
	import excel using "$data/atet_scaling_parameters.xlsx", clear firstr
	keep if type=="`type'"
	drop type

	/*********************************************************************/
					******DO THE BOOTSTRAP******
	/**********************************************************************/
	pbs `type'
}
	 
	
 exit
 
 
 
