use "~/proj/mm-prep/ROOT/stata/structure_of_executives_input.dta", clear
gen op_hos=v2expathhs==2 if v2expathhs<.
gen op_hog=v2expathhg==2 if v2expathhs<.
gen opw=v2ex_hosw*op_hos+v2ex_hogw*op_hog
gen mil_hos=(v2expathhs==0 | v2expathhs==5) if v2expathhs<.
gen mil_hog=(v2expathhg==5 | v2expathhg==5) if v2expathhs<.
gen milw=v2ex_hosw*mil_hos+v2ex_hogw*mil_hog
gen mon_hos=(v2expathhs==3 | v2expathhs==4) if v2expathhs<.
gen mon_hog=(v2expathhg==3 | v2expathhg==4) if v2expathhs<.
gen monw=v2ex_hosw*mon_hos+v2ex_hogw*mon_hog
gen elec_hog=v2ex_elechog==1 if v2ex_elechos<.
gen elecexw=v2ex_hosw*v2ex_elechos+v2ex_hogw*elec_hog
gen conf_hos=v2exremhsp_ord> 2 if v2exremhsp<.
gen conf_hog=v2exremhog_ord> 2 & v2exremhog_ord<5 if v2exremhsp<.
gen confw=v2ex_hosw*conf_hos+v2ex_hogw*conf_hog
gen dual=1-v2exhoshog
gen op_dism_v70=v2exrmhsol_2
replace op_dism_v70=v2ex_hosw*op_dism_v70+v2ex_hogw*v2exrmhgnp_2 if dual==1 & v2exrmhgnp_2<.
gen mil_dism_v70=v2exrmhsol_4
replace mil_dism_v70=v2ex_hosw*mil_dism_v70+v2ex_hogw*v2exrmhgnp_4 if dual==1 & v2exrmhgnp_4<.
gen mon_dism_v70=v2exrmhsol_3
replace mon_dism_v70=v2ex_hosw*mon_dism_v70+v2ex_hogw*v2exrmhgnp_3 if dual==1 & v2exrmhgnp_3<.
gen leg_rule2=(v2exremhsp_ord)/3
replace leg_rule2=v2ex_hosw*leg_rule2+v2ex_hogw*v2exremhog_ord/3 if dual==1 & v2exremhog<.
egen op_both_v70=rowmean(opw op_dism_v70)
egen mil_both_v70=rowmean(milw mil_dism_v70)
egen mon_both_v70=rowmean(monw mon_dism_v70)

rename mon_both_v70 v2x_ex_hereditary
rename mil_both_v70 v2x_ex_military
rename op_both_v70  v2x_ex_party
rename elecexw v2x_ex_direlect
rename leg_rule2 v2x_ex_confidence

keep country_text_id year v2x_ex_hereditary v2x_ex_military v2x_ex_party v2x_ex_direlect v2x_ex_confidence

save "~/proj/mm-prep/ROOT/stata/structure_of_executives_output.dta", replace

