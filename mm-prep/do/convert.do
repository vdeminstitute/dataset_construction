* Apply value labels and save as stata version 13

* CY-Core
use "~/proj/mm-prep/ROOT/dataset/STATA_temp/V-Dem-CY-Core-v11.dta", clear
do ~/proj/mm-prep/do/value_labels.do, nostop
saveold "~/proj/mm-prep/ROOT/dataset/V-Dem-CY-Core/V-Dem-CY-Core-v11.dta", version(13) replace

* CY-Full+Others
use "~/proj/mm-prep/ROOT/dataset/STATA_temp/V-Dem-CY-Full+Others-v11.dta", clear
do ~/proj/mm-prep/do/value_labels.do, nostop
saveold "~/proj/mm-prep/ROOT/dataset/V-Dem-CY-Full+Others/V-Dem-CY-Full+Others-v11.dta", version(13) replace

* CD-Full
use "~/proj/mm-prep/ROOT/dataset/STATA_temp/V-Dem-CD-v11.dta", clear
do ~/proj/mm-prep/do/value_labels.do, nostop
saveold "~/proj/mm-prep/ROOT/dataset/V-Dem-CD/V-Dem-CD-v11.dta", version(13) replace
