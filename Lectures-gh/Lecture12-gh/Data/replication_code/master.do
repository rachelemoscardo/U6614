global dofile 		"C:\Users\goodmaj2\Documents\medicaid\jpe"
global data 		"C:\Users\goodmaj2\Documents\medicaid\jpe\data"
global datatemp 	"C:\Users\goodmaj2\Documents\medicaid\jpe\datatemp"
global output 		"C:\Users\goodmaj2\Documents\medicaid\jpe\output"


clear
clear mata
set matsize 2000

***************************
*Analysis Files, Main Paper
***************************
*Table 1: Balance table
do "$dofile/main_results/an_table1"			
*Table 2: First Stage
do "$dofile/main_results/an_table2"			
*Table 3: child mortality results by spec. ~10 hours to do 5,000 bootstrap draws
do "$dofile/main_results/an_table3" 			
*Table 4: infant mortalitly results by age at death. ~2  hours to do 5,000 bootstrap draws
do "$dofile/main_results/an_table4"			
*Table 5: other infant outcomes, Vital Stats. ~2  hours to do 5,000 bootstrap draws
do "$dofile/main_results/an_table5"			
*Table 6: other infant outcomes, NNFBS
do "$dofile/main_results/an_table6"			
*Table 7: child mortality by age and cause
do "$dofile/main_results/an_table7"			
*Table 8: Robustness and Falsification Tests
do "$dofile/main_results/an_table8"			


*figures 1-4 do not have analysis files
do "$dofile/main_results/an_figure5"			/*first stage ES*/
do "$dofile/main_results/an_figure6"			/*Reduced form ES*/
do "$dofile/main_results/an_figure7"			/*age specific reduced form ES*/
do "$dofile/main_results/an_figure8"			/*infant deaths by hour and day*/
do "$dofile/main_results/an_figure9"			/*other programs*/
do "$dofile/main_results/an_figure10"			/*ATET*/

do "$dofile/main_results/cr_figure1_pub_unins_1950_to_2012"
do "$dofile/main_results/cr_figure2_dwelf_age_race_admin67"
do "$dofile/main_results/cr_figure3_mvp_levels"
do "$dofile/main_results/cr_figure4_share_internal"
do "$dofile/main_results/cr_figure5_firststage_es_kids"
do "$dofile/main_results/cr_figure6_es_amrch"
do "$dofile/main_results/cr_figure7_age_specific"
do "$dofile/main_results/cr_figure8_dd_nnmr_byhour_byday"
do "$dofile/main_results/cr_figure9_otherprog"
do "$dofile/main_results/cr_figure10_atet"



******************
*Appendix Results*
*******************
*1: Data Appendix (no dofiles)

*2A: Eligibility and Utilization
do "$dofile/app2A/an_figureA2A5_prexp"

do "$dofile/app2A/cr_figureA2A1_dwelf_age_race_ipums70"
do "$dofile/app2A/cr_figureA2A2_dmcaid_age_race_sie76"
do "$dofile/app2A/cr_figureA2A3_afdc_persistence"
do "$dofile/app2A/cr_figureA2A4_afdc_elig_1960"
do "$dofile/app2A/cr_figureA2A5_prexp"


*2B: Additional Support for the Research Design
do "$dofile/app2B/an_figureA2B1_pretrend_figs"
do "$dofile/app2B/an_figureA2B2_cra_balance_1950_1970"
do "$dofile/app2B/an_figureA2B3_fs_add0"
do "$dofile/app2B/an_figureA2B4_fs_chrate"

do "$dofile/app2B/cr_figureA2B1_pretrend_figs"
do "$dofile/app2B/cr_figureA2B2_cra_balance_1950_1970"
do "$dofile/app2B/cr_figureA2B3_firststage_es_kids_add0"
do "$dofile/app2B/cr_figureA2B4_fs_chrate"
do "$dofile/app2B/cr_figureA2B5_migration"


*2C: Additional Mortality Event-Study Results
do "$dofile/app2C/an_figureA2C1_A_amr_1950_1988"
do "$dofile/app2C/an_figureA2C1_B_simelig"
do "$dofile/app2C/an_figureA2C2_amr_binary"
do "$dofile/app2C/an_figureA2C3_asmr_binary"
do "$dofile/app2C/an_figureA2C4_amr_nw_byspec"
do "$dofile/app2C/an_figureA2C5_chrate"
do "$dofile/app2C/an_figureA2C6_mmr"
do "$dofile/app2C/an_figureA2C7_int_ext"
do "$dofile/app2C/an_figureA2C8_tr_untr"
do "$dofile/app2C/an_figureA2C9_amr_w_byspec"
do "$dofile/app2C/an_figureA2C10_asmr_w"

do "$dofile/app2C/cr_figureA2C1_amr_1950_1988"
do "$dofile/app2C/cr_figureA2C2_amr_binary"
do "$dofile/app2C/cr_figureA2C3_asmr_binary"
do "$dofile/app2C/cr_figureA2C4_amr_nw_byspec"
do "$dofile/app2C/cr_figureA2C5_chrate"
do "$dofile/app2C/cr_figureA2C6_mmr"
do "$dofile/app2C/cr_figureA2C7_int_ext"
do "$dofile/app2C/cr_figureA2C8_tr_untr"
do "$dofile/app2C/cr_figureA2C9_amr_w_byspec"
do "$dofile/app2C/cr_figureA2C10_asmr_w"


*2D: Additional Mortality Difference-in-Difference Results
do "$dofile/app2D/an_tableA2D1_alt_samples"
do "$dofile/app2D/an_tableA2D2_white_amr_byspec"
do "$dofile/app2D/an_tableA2D3_white_byage"
do "$dofile/app2D/an_tableA2D4_nmr_byspec"
do "$dofile/app2D/an_tableA2D5_asmr14_byspec"
do "$dofile/app2D/an_tableA2D6_pnmr_control"


*2E: Additional Non-Mortality Event-Study Results
do "$dofile/app2E/an_figureA2E1_bw"
do "$dofile/app2E/an_figureA2E2_sr"
do "$dofile/app2E/an_figureA2E3_hospbirths"

do "$dofile/app2E/cr_figureA2E1_bw"
do "$dofile/app2E/cr_figureA2E2_sr"
do "$dofile/app2E/cr_figureA2E3_hospbirths"

*2F: Results Related to the Interpretation of Effects
do "$dofile/app2f/cr_figureA2F1_nmr_decomp"

*3: Estimates Using the Staggered Timing of Medicaid Implementation
do "$dofile/app3/an_figureA3_2_amrch_timing"
do "$dofile/app3/an_figureA3_3_fs_timing"
do "$dofile/app3/an_figureA3_4_year_FE_timing"
do "$dofile/app3/an_tableA3_1_asmr_timing_byspec"

do "$dofile/app3/cr_figureA3_1_pretrends"
do "$dofile/app3/cr_figureA3_2_amrch_timing"
do "$dofile/app3/cr_figureA3_3_fs_timing"
do "$dofile/app3/cr_figureA3_4_year_FE_timing"


*4: Rescaling Quasiexperimental Estimates
do "$dofile/app4/an_figureA4_1_atet_corr"

do "$dofile/app4/cr_figureA4_1_atet_corr"









