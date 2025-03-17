****** Authors: Marcus Tannenberg and Anna Lührmann
****** For the JoD article (V4)
****** This is the same as used in the JoD article (regime type
****** CI_new_osp_) but with the leg and ex election variables in the
****** DS
****** Set DS_VERSION paths in the use (top) and save (bottom) calls,

*use "~/proj/vdemds/ROOT/stata/regime_type_input.dta", clear
use "~/data/ds_construction/v15/stata/regime_type_input.dta", clear
*drop if year < 1900

************ PART 1. 4 category regime variable
*****  B.  Head of Excecutive in Multiparty Elections

** 0. Interpolate for non-election  years

* see loop  & following

*gen v2elmulpar_osp_ex=v2elmulpar_osp if v2eltype_6==1 |  v2eltype_7==1
*gen v2elmulpar_osp_leg=v2elmulpar_osp if v2eltype_0==1 |  v2eltype_1==1

** Interpolate the regular election variables

generate v2elfrfair_osp_imp=v2elfrfair_osp
by country_name (year), sort: replace  v2elfrfair_osp_imp = v2elfrfair_osp_imp[_n-1] if v2elfrfair_osp_imp==. & v2x_elecreg==1

generate v2elmulpar_osp_imp=v2elmulpar_osp
by country_name (year), sort: replace  v2elmulpar_osp_imp = v2elmulpar_osp_imp[_n-1] if v2elmulpar_osp_imp==. & v2x_elecreg==1

generate v2elfrfair_osp_codehigh68_imp= v2elfrfair_osp_codehigh
by country_name (year), sort: replace  v2elfrfair_osp_codehigh68_imp = v2elfrfair_osp_codehigh68_imp[_n-1] if v2elfrfair_osp_codehigh68_imp==. & v2x_elecreg==1
generate v2elmulpar_osp_codehigh68_imp= v2elmulpar_osp_codehigh
by country_name (year), sort: replace  v2elmulpar_osp_codehigh68_imp = v2elmulpar_osp_codehigh68_imp[_n-1] if v2elmulpar_osp_codehigh68_imp==. & v2x_elecreg==1
generate v2elfrfair_osp_codelow68_imp= v2elfrfair_osp_codelow
by country_name (year), sort: replace  v2elfrfair_osp_codelow68_imp = v2elfrfair_osp_codelow68_imp[_n-1] if v2elfrfair_osp_codelow68_imp==. & v2x_elecreg==1
generate v2elmulpar_osp_codelow68_imp= v2elmulpar_osp_codelow
by country_name (year), sort: replace  v2elmulpar_osp_codelow68_imp = v2elmulpar_osp_codelow68_imp[_n-1] if v2elmulpar_osp_codelow68_imp==. & v2x_elecreg==1


/*gen v2elmulpar_osp_codehigh68_ex=v2elmulpar_osp_codehigh68 if v2elmulpar_osp_ex!=.
gen v2elmulpar_osp_codehigh68_leg=v2elmulpar_osp_codehigh68 if v2elmulpar_osp_leg!=.
gen v2elmulpar_osp_codelow68_ex=v2elmulpar_osp_codelow68 if v2elmulpar_osp_ex!=.
gen v2elmulpar_osp_codelow68_leg=v2elmulpar_osp_codelow68 if v2elmulpar_osp_leg!=.*/


** Interpolate the excecutive election variables

generate v2elmulpar_osp_ex_imp= v2elmulpar_osp_ex
by country_name (year), sort: replace  v2elmulpar_osp_ex_imp = v2elmulpar_osp_ex_imp[_n-1] if v2elmulpar_osp_ex_imp==. & v2xex_elecreg==1
generate v2elmulpar_osp_codehigh68_ex_imp= v2elmulpar_osp_codehigh_ex
by country_name (year), sort: replace  v2elmulpar_osp_codehigh68_ex_imp = v2elmulpar_osp_codehigh68_ex_imp[_n-1] if v2elmulpar_osp_codehigh68_ex_imp==. & v2xex_elecreg==1
generate v2elmulpar_osp_codelow68_ex_imp= v2elmulpar_osp_codelow_ex
by country_name (year), sort: replace  v2elmulpar_osp_codelow68_ex_imp = v2elmulpar_osp_codelow68_ex_imp[_n-1] if v2elmulpar_osp_codelow68_ex_imp==. & v2xex_elecreg==1

** Interpolate the legislative election variables

generate v2elmulpar_osp_leg_imp= v2elmulpar_osp_leg
by country_name (year), sort: replace  v2elmulpar_osp_leg_imp = v2elmulpar_osp_leg_imp[_n-1] if v2elmulpar_osp_leg_imp==. & v2xlg_elecreg==1

generate v2elmulpar_osp_codelow68_leg_imp= v2elmulpar_osp_codelow_leg
by country_name (year), sort: replace  v2elmulpar_osp_codelow68_leg_imp = v2elmulpar_osp_codelow68_leg_imp[_n-1] if v2elmulpar_osp_codelow68_leg_imp==. & v2xlg_elecreg==1
generate v2elmulpar_osp_codehigh68_leg_i= v2elmulpar_osp_codehigh_leg
by country_name (year), sort: replace  v2elmulpar_osp_codehigh68_leg_i = v2elmulpar_osp_codehigh68_leg_i[_n-1] if v2elmulpar_osp_codehigh68_leg_i==. & v2xlg_elecreg==1


* 0. Interpolate from create script
*rename v2ex_hosw_rec v2ex_hosw

** 1. Indicator if both elections are mulitparty

generate LegExMultiparty=0 if v2ex_hosw!=.
replace  LegExMultiparty=1 if v2xlg_elecreg==1  & v2xex_elecreg==1 & v2elmulpar_osp_ex_imp>1 & v2elmulpar_osp_ex_imp!=.  & v2elmulpar_osp_leg_imp>1 & v2elmulpar_osp_leg_imp!=.
 * 2. HoS elected in Multiparty Elections?
generate HoSmultiparty=0 if v2x_elecreg!=.
replace HoSmultiparty=1 if v2expathhs==7  & v2xex_elecreg==1 & v2elmulpar_osp_ex_imp>1 & v2elmulpar_osp_ex_imp!=.
* if HoS is directly elected, elections are multiparty
replace HoSmultiparty=1 if v2expathhs==6  & v2elmulpar_osp_leg_imp>1 & v2elmulpar_osp_leg_imp!=. & v2xlg_elecreg==1
*(if HoS is elected through parliament, parliamentary elections were multiparty
replace HoSmultiparty=1 if v2ex_legconhos==1  & v2xlg_elecreg==1 & v2elmulpar_osp_ex_imp>1 & v2elmulpar_osp_ex_imp!=.
* If parliamentary approval is needed in appointment of HoS, parliamentary elections are mulitparty


** 3. HoG elected in Multiparty Elections?

generate HoGmultiparty=0 if v2x_elecreg!=.
. replace HoGmultiparty=1 if v2expathhg==8 & v2elmulpar_osp_ex_imp>1 & v2elmulpar_osp_ex_imp!=. & v2xex_elecreg==1
 * if HoG is directly elected, elections are multiparty
 replace HoGmultiparty=1 if v2expathhg==7 & v2elmulpar_osp_leg_imp>1 & v2elmulpar_osp_leg_imp!=. & v2xlg_elecreg==1
* if HoG is indirectly elected, leg  elections are multiparty
 replace HoGmultiparty=1 if v2expathhg==6 &  HoSmultiparty==1
* if HoG appointed by HoS, HoS election multiparty
 replace HoGmultiparty=1 if v2exaphogp==1 & v2elmulpar_osp_leg_imp>1 & v2elmulpar_osp_leg_imp!=. & v2elmulpar_osp_imp!=.
* If parliamentary approval is needed in appointment of HoG, parliamentary elections are mulitparty


** 4. HoE elected in Multiparty Elections?

generate HoEmultiparty=0 if v2ex_hosw!=.
replace HoEmultiparty=HoSmultiparty if v2ex_hosw<=1 & v2ex_hosw > 0.5
* if HoS is more powerful its score is taken
replace HoEmultiparty=HoGmultiparty if v2ex_hosw<=0.5
* ”chief executive” is equal to the HOG if v2ex_hosw=0 or v2x_hogw=.5, otherwise (if v2ex_hosw=1) the ”chief executive” is the HOS. (from JT)

replace HoEmultiparty=1 if LegExMultiparty==1


***** D. Regime Types (v2x_regime)

*gen e_v2x_polyarchy_4C=0 if v2x_polyarchy!=.
*replace e_v2x_polyarchy_4C=.67 if v2x_polyarchy>0.5 & v2elfrfair_osp>2 & v2elmulpar_osp>2 & v2x_polyarchy!=. & v2elfrfair_osp!=. & v2elmulpar_osp!=.

gen poly_4C=0 if v2x_polyarchy!=.
replace poly_4C=1 if v2x_polyarchy>0.5 & v2elfrfair_osp_imp>2 & v2elmulpar_osp_imp>2 & v2x_polyarchy!=. & v2elfrfair_osp_imp!=. &v2elmulpar_osp_imp!=.

* Marcus: Updated liberal thresholds
gen liberal_5C = .
replace liberal_5C = 1 if v2x_liberal !=. & v2x_liberal > 0.8 & v2clacjstm_osp > 3 & v2clacjstw_osp > 3 & v2cltrnslw_osp > 3


gen v2x_regime = .
*create
replace v2x_regime=3 if poly_4C==1 & liberal_5C==1

replace v2x_regime=2 if poly_4C==1 & liberal_5C!=1

replace v2x_regime=1 if poly_4C==0 & HoEmultiparty==1 & v2elmulpar_osp_leg_imp > 1

replace v2x_regime=0 if poly_4C==0 & (HoEmultiparty==0 | v2elmulpar_osp_leg_imp < 1)

label define v2x_regime_label 0 "Closed Autocracy" 1 "Electoral Autocracy" 2 "Electoral Democracy" 3 "Liberal Democracy"
label values v2x_regime v2x_regime_label

/*gen LibDem=1 if v2x_regime== 3
gen ElecDem=1 if v2x_regime== 2
gen ElecAut=1 if v2x_regime== 1
gen CloAut=1 if v2x_regime== 0*/

**** count
/*by year, sort: egen totlibdem=total(LibDem)
by year, sort: egen totelecdem=total(ElecDem)
by year, sort: egen totelecaut=total(ElecAut)
by year, sort: egen totcloaut=total(CloAut)*/


******************* PART 2: adding confidence intervals and grey zone categories
gen v2x_regime_amb=.
replace v2x_regime_amb=9 if v2x_regime== 3
replace v2x_regime_amb=6 if v2x_regime== 2
replace v2x_regime_amb=3 if v2x_regime== 1
replace v2x_regime_amb=0 if v2x_regime== 0

***** creating 4, 7 and 8
* B.  Head of Excecutive in Multiparty Elections

 ***** D. Regime Types


*Category 4: higher bounds on polyarchy and elections go above threshold but not the point estimate
gen poly_4C_codehigh68=1 if v2x_polyarchy_codehigh>0.5 & v2elfrfair_osp_codehigh68_imp>2 & v2elmulpar_osp_codehigh68_imp>2 & poly_4C!=1

*Category 5: Countries that are falling on one of polyarchy dimension but not on the point estimate
gen poly_4C_codelow68=1 if (v2x_polyarchy_codelow<0.5 | v2elfrfair_osp_codelow68_imp<2 | v2elmulpar_osp_codelow68_imp<2) & poly_4C==1

*Category 7: Countries where higher bound on liberal is above cut-offs but not the point estimates
gen liby_5C_codehigh68=1 if v2x_liberal_codehigh>0.8 & v2clacjstm_osp_codehigh > 3 & v2clacjstw_osp_codehigh > 3 & v2cltrnslw_osp_codehigh > 3 & liberal_5C!=1

*Category 8: Countries where codelow is below the threshold but not the point estimate
gen liby_5C_codelow68=1 if (v2clacjstm_osp_codelow <3 | v2clacjstw_osp_codelow <3 | v2cltrnslw_osp_codelow <3 | v2x_liberal_codelow<0.8) & liberal_5C==1


* Between Libdem and elec dem
*codehigh is above the threshold
replace v2x_regime_amb=7 if liby_5C_codehigh68==1 & v2x_liberal!=. & v2x_regime== 2
*codelow is below the threshold
replace v2x_regime_amb=8 if liby_5C_codelow68==1 & v2x_liberal!=. & v2x_regime== 3

* Between elec dem and elec aut
*codehigh is above the threshold elec aut upper
replace v2x_regime_amb=4 if poly_4C_codehigh68==1 & v2x_regime ==1
*codelow is below the threshold elec dem lower
replace v2x_regime_amb=5 if poly_4C_codelow68==1 & v2x_regime ==2


******* creating 1 and 2 (closed aut upper and elec aut lower)
***** Upper bounds are higher than threshold
** 0. Impute for missing years

** 1. Indicator if both elections are mulitparty
generate LegExMultiparty_high=0 if v2ex_hosw!=.
replace  LegExMultiparty_high=1 if (v2elmulpar_osp_codehigh68_ex_imp>1 & v2elmulpar_osp_codehigh68_ex_imp!=.) & (v2elmulpar_osp_codehigh68_leg_i!=0 & v2elmulpar_osp_codehigh68_leg_i!=.)

* 2. HoS elected in Multiparty Elections?
generate HoSmultiparty_high=0 if v2x_elecreg!=.
replace HoSmultiparty_high=1 if v2expathhs==7 &  (v2elmulpar_osp_codehigh68_ex_imp>1 & v2elmulpar_osp_codehigh68_ex_imp!=.) & v2xex_elecreg==1
* if HoS is directly elected, elections are multiparty
replace HoSmultiparty_high=1 if v2expathhs==6 &  (v2elmulpar_osp_codehigh68_leg_i>1 & v2elmulpar_osp_codehigh68_leg_i!=.) & v2xlg_elecreg==1
*(if HoS is elected through parliament, parliamentary elections were multiparty
replace HoSmultiparty_high=1 if v2ex_legconhos==1  & v2xlg_elecreg==1  &   (v2elmulpar_osp_codehigh68_leg_i>1 & v2elmulpar_osp_codehigh68_leg_i!=.)
* If parliamentary approval is needed in appointment of HoS, parliamentary elections are mulitparty


** 3. HoG elected in Multiparty Elections?

generate HoGmultiparty_high=0 if v2x_elecreg!=.
. replace HoGmultiparty_high=1 if v2expathhg==8 &  (v2elmulpar_osp_codehigh68_ex_imp>1 & v2elmulpar_osp_codehigh68_ex_imp!=.) & v2xex_elecreg==1
 * if HoG is directly elected, elections are multiparty
 replace HoGmultiparty_high=1 if v2expathhg==7 & (v2elmulpar_osp_codehigh68_leg_i>1 & v2elmulpar_osp_codehigh68_leg_i!=.) & v2xlg_elecreg==1
* if HoG is indirectly elected, leg  elections are multiparty
 replace HoGmultiparty_high=1 if v2expathhg==6 &  HoSmultiparty_high==1
* if HoG appointed by HoS, HoS election multiparty
 replace HoGmultiparty_high=1 if v2exaphogp==1  & v2xlg_elecreg==1 & (v2elmulpar_osp_codehigh68_leg_i>1 & v2elmulpar_osp_codehigh68_leg_i!=.)
* If parliamentary approval is needed in appointment of HoG, parliamentary elections are mulitparty

** 4. HoE elected in Multiparty Elections?

generate HoEmultiparty_high=0 if v2ex_hosw!=.
replace HoEmultiparty_high=HoSmultiparty_high if v2ex_hosw==1
* if HoS is more powerful its score is taken
replace HoEmultiparty_high=HoGmultiparty_high if v2ex_hosw==0 | v2ex_hosw==0.5
* ”chief executive” is equal to the HOG if v2ex_hosw=0 or v2x_hogw=.5, otherwise (if v2ex_hosw=1) the ”chief executive” is the HOS. (from JT)

replace HoEmultiparty_high=1 if LegExMultiparty_high==1


*Between ElecDem and ElecAut
*The upper bound goes above the threshold
replace v2x_regime_amb=1 if poly_4C==0 & HoEmultiparty_high==1 & (HoEmultiparty!=1 | v2elmulpar_osp_leg_imp < 1) & v2elmulpar_osp_codehigh68_leg_i > 1

************* Lower bounds are lower than threshold


** 1. Indicator if both elections are multiparty
generate LegExMultiparty_low=0 if v2ex_hosw!=.

*@ANNA - line below should be "LegExMultiparty_low=1 if v2elmulpar_osp_codelow68_ex_imp<1 | v2elmulpar_osp_codelow68_leg_imp<1" right?
* YES! there mus have been a mistake but then it should read if (v2elmulpar_osp_codelow68_ex_imp<1 | v2elmulpar_osp_codelow68_leg_imp<1) & v2elmulpar_osp_ex_imp<1 & v2elmulpar_osp_leg_imp<1"
replace  LegExMultiparty_low=1 if (v2elmulpar_osp_codelow68_ex_imp<1 | v2elmulpar_osp_codelow68_leg_imp<1) & v2elmulpar_osp_ex_imp<1 & v2elmulpar_osp_leg_imp<1


* 2. HoS elected in Multiparty Elections?
generate HoSmultiparty_low=0 if v2x_elecreg!=.
** Why do we have v2xex_elecreg==1? Anna please check!
replace HoSmultiparty_low=1 if v2expathhs==7 &  v2elmulpar_osp_codelow68_ex_imp<1 & v2xex_elecreg==1 & v2elmulpar_osp_codelow68_ex_imp!=.
* if HoS is directly elected, elections are multiparty
replace HoSmultiparty_low=1 if v2expathhs==6 &  v2elmulpar_osp_codelow68_leg_imp<1 & v2xlg_elecreg==1 & v2elmulpar_osp_codelow68_leg_imp!=.
*(if HoS is elected through parliament, parliamentary elections were multiparty
replace HoSmultiparty_low=1 if v2ex_legconhos==1  & v2xlg_elecreg==1  &  v2elmulpar_osp_codelow68_leg_imp==0
* If parliamentary approval is needed in appointment of HoS, parliamentary elections are mulitparty


** 3. HoG elected in Multiparty Elections?

generate HoGmultiparty_low=0 if v2x_elecreg!=.
. replace HoGmultiparty_low=1 if v2expathhg==8 &  v2elmulpar_osp_codelow68_ex_imp<1 & v2xex_elecreg==1 & v2elmulpar_osp_codelow68_ex_imp!=.
 * if HoG is directly elected, elections are multiparty
 replace HoGmultiparty_low=1 if v2expathhg==7 & v2elmulpar_osp_codelow68_leg_imp<1 & v2xlg_elecreg==1 & v2elmulpar_osp_codelow68_leg_imp!=.
* if HoG is indirectly elected, leg  elections are multiparty
 replace HoGmultiparty_low=1 if v2expathhg==6 &  HoSmultiparty_low==1
* if HoG appointed by HoS, HoS election multiparty
 replace HoGmultiparty_low=1 if v2exaphogp==1  & v2xlg_elecreg==1 & v2elmulpar_osp_codelow68_leg_imp==0
* If parliamentary approval is needed in appointment of HoG, parliamentary elections are mulitparty

** 4. HoE elected in Multiparty Elections?

generate HoEmultiparty_low=0 if v2ex_hosw!=.
replace HoEmultiparty_low=HoSmultiparty_low if v2ex_hosw==1
* if HoS is more powerful its score is taken
replace HoEmultiparty_low=HoGmultiparty_low if v2ex_hosw==0 | v2ex_hosw==0.5
* ”chief executive” is equal to the HOG if v2ex_hosw=0 or v2x_hogw=.5, otherwise (if v2ex_hosw=1) the ”chief executive” is the HOS. (from JT)

replace HoEmultiparty_low=1 if LegExMultiparty_low==1

*Between ElecAut and Closed Aut
*The lower bound goes below the threshold

*@ANNA - Or it should work to add the "v2elmulpar_osp_codelow68_leg_imp<1" here but with a | operator, similar to what I did with the threshold on line 221
* AL: I think it should work if replace v2x_regime_amb=2 if poly_4C==0 & ((HoEmultiparty_low==1 & HoEmultiparty==1)| (v2elmulpar_osp_codelow68_leg_imp<=1 & v2elmulpar_osp_leg_imp>1))
*old: replace v2x_regime_amb=2 if poly_4C==0 & HoEmultiparty_low==1 & HoEmultiparty==1
replace v2x_regime_amb=2 if v2x_regime == 1 & poly_4C==0 & ((HoEmultiparty_low==1 & HoEmultiparty==1)| (v2elmulpar_osp_codelow68_leg_imp<=1 & v2elmulpar_osp_leg_imp>1))


label define v2x_regime_amb_labels 0 "Closed Autocracy" 1 "Closed Autocracy Upper Bound" 2 "Electoral Autocracy Lower Bound" ///
3 "Electoral Autocracy" 4 "Elec Auto Upper Bound" 5 "Electoral Democracy Lower Bound" 6 "Electoral Democracy" 7 "Electoral Democracy Upper bound" ///
8 "Liberal Democracy Lower Bound" 9 "Liberal Democracy"
*define regime*/

label values v2x_regime_amb v2x_regime_amb_labels

keep country_text_id year v2x_regime v2x_regime_amb

save "~/data/ds_construction/v15/stata/regime_type_output.dta", replace
*save "~/proj/vdemds/ROOT/stata/regime_type_output.dta", replace
