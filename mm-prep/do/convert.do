
* CY-Core
use "~/proj/mm-prep/ROOT/dataset_stata/V-Dem-CY-Core-v12.dta", clear
do ~/proj/mm-prep/do/value_labels.do, nostop
saveold "~/proj/mm-prep/ROOT/dataset/V-Dem-CY-Core/V-Dem-CY-Core-v12.dta", version(13) replace

* CY-Full+Others
use "~/proj/mm-prep/ROOT/dataset_stata/V-Dem-CY-Full+Others-v12.dta", clear
do ~/proj/mm-prep/do/value_labels.do, nostop
saveold "~/proj/mm-prep/ROOT/dataset/V-Dem-CY-Full+Others/V-Dem-CY-Full+Others-v12.dta", version(13) replace

* CD-Full
use "~/proj/mm-prep/ROOT/dataset_stata/V-Dem-CD-v12.dta", clear
do ~/proj/mm-prep/do/value_labels.do, nostop
saveold "~/proj/mm-prep/ROOT/dataset/V-Dem-CD/V-Dem-CD-v12.dta", version(13) replace
