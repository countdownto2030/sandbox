
* Save subset of India and Nigeria
* India - 2019
use "/Users/EWilson/Desktop/Countdown/Data/India/2019/IABR7DFL.DTA", clear
keep caseid midx v001 v005 v008 v015 b3 midx m3a m3b m3c m3d m3e m3f m3h m3i m15 m61 v025 v012 v106 v501 v224 v024 v023 v190 m63 m67 
*m50 m51
save "/Users/EWilson/Desktop/DAC/Delivery/Data/PartialIndiaNigeria/IABR7DFLsub.DTA", replace

* India - 2015
use "/Users/EWilson/Desktop/Countdown/Data/India/2015/IABR72FL.DTA", clear
keep caseid midx v001 v005 v008 v015 b3 midx m3a m3b m3c m3d m3e m3f m3h m3i m15 m61 v025 v012 v106 v501 v224 v024 v023 v190 m50 m51
*m63 m67
save "/Users/EWilson/Desktop/DAC/Delivery/Data/PartialIndiaNigeria/IABR72FLsub.DTA", replace

* Nigeria
use "/Users/EWilson/Desktop/Countdown/Data/Nigeria/2018/NGBR7BFL.DTA", clear
keep caseid midx v001 v005 v008 v015 b3 midx m3a m3b m3c m3d m3e m3f m3h m3i m15 m61 m63 v025 v012 v106 v501 v224 v024 v023 v190
save "/Users/EWilson/Desktop/DAC/Delivery/Data/PartialIndiaNigeria/NGBR7BFLsub.DTA", replace

* Look at Chad
use "/Users/EWilson/Desktop/Countdown/Data/Chad/2019/wm.dta", clear
numlabel, add
ta MN19A
ta MN19B

* Compare to Bangladesh
use "/Users/EWilson/Desktop/Countdown/Data/Bangladesh/2019/wm.dta", clear
numlabel, add
ta MN19A
ta MN19B




* Look at Kenya
use "/Users/EWilson/Desktop/Countdown/Data/Kenya/2014/KEBR71FL.DTA", clear
numlabel, add

* MICS *************************************************************************************
use "/Users/EWilson/Desktop/Countdown/Data/Guyana/2019/wm.DTA", clear
numlabel, add

gen id = string(HH1) + " " + string(HH2) + " " + string(WM4)
duplicates report id
br id WDOBFC WDOBLC

gen agemo = WDOI - WDOBLC
gen ageyr = (WDOI/12 + 1900) - (WDOBLC/12 +1900)


br id WDOBFC WDOBLC agemo ageyr
sort agemo



use "/Users/EWilson/Desktop/Countdown/Data/Bangladesh/2019/wm.DTA", clear
numlabel, add
ta PN5
ta PN17



* DHS *************************************************************************************
use "/Users/EWilson/Desktop/Countdown/Data/Ethiopia/2019/ETBR81FL_old.DTA", clear
*br v008 b3
numlabel, add
ta m15, m

use "/Users/EWilson/Desktop/Countdown/Data/Gambia/2019/GMBR81FL.DTA", clear
numlabel, add
ta m15, m

* Senegal 2017
set maxvar 6000
use "/Users/EWilson/Desktop/Countdown/Data/Senegal/2017/SNIR7ZFL.DTA"
numlabel, add

* sba - NA for country-specific optoins
tab m3a_1
tab m3b_1
tab m3c_1

* ideliv
tab m15_1


use "/Users/EWilson/Desktop/Countdown/Data/Senegal/2018/SNIR81FL.DTA", clear
numlabel, add

tab m3a_1
tab m3b_1
tab m3c_1


*tab m14
*tab m13
*tab m3a
*tab m3b
*tab m3c
*tab m15
*tab m61
*tab m63
*tab m75
*tab v426
*tab m34
*** inferential analysis variables *********************************************************
tab v190                             // 1. wealth quintile 
tab v025                             // 2. residence
tab v012                             // 3. age group
tab v106                             // 4. education level : no education, primary, secondary, higher
tab v501                             // 5. marital status (married or in union)
tab bidx                             // 6. parity - merge to get 0
tab bidx                             // 7. first time mother
tab v225                             // 8. wanted most recent birth

tab v501
tab v364
tab v225
tab v3a08d
tab v3a08e
tab v3a08f

gen agemo = v008 - b3

gen pncwm = 0 if agemo < 24 & midx == 1
replace pncwm = 1 if agemo < 24 & midx == 1 & m63 >= 100 & m63 <= 202
replace pncwm = NA if agemo <24 & midx == 1 & m63 >= 998
tab pncwm

/*
gen met_need = 0 if inlist(v501,1,2) & v364 == 1 // 9. met need for modern contraception
replace met_need = 0 if inlist(v501,1,2) & inlist(v225,2,3) // unmet need
replace met_need = 1 if inlist(v501,1,2) & v364 == 1        // met need
replace met_need = . if v3a08d==1 | v3a08e==1 | v3a08f==1 //menopausal,infecund,amenorrheic	
tab met_need // 3000/3561	

gen met_need = 0 if inlist(v501,1,2) & inlist(v364,1,2)      // 9. all contraception
replace met_need = 0 if inlist(v501,1,2) & inlist(v225,2,3)  // unmet need
replace met_need = 1 if inlist(v501,1,2) & v364 == 1         // met need
replace met_need = . if v3a08d==1 | v3a08e==1 | v3a08f==1    //menopausal,infecund,amenorrheic	
tab met_need						 
*/
tab v626 v364

tab v225 v364
tab m10 m11

tab v401 // m17                      // 10. c-section
gen age_diff = v730 - v012           // 11. age difference between woman and partner
*replace age_diff = . if v730 == 99
*replace age_diff = . if v012 == 99
browse caseid v730 v012 age_diff
tab v744a	                         // 12. attitude towards domestic violence
tab v744b
tab v744c
tab v744d
tab v744e
tab v511                             // 13. age at first marriage





* SBA **************************************************************************************
* Y m3a - doctor
* Y m3b - nurse
* Y m3c - midwife


* IDELIV ***********************************************************************************
* DHS:
* N 11. respondent's home
* N 12. other home 
* N 13. other home (imputed)
* Y 21. central hospital / Gov't hospital
* Y 22. provincial hospital / CC Govt health professional / Central maternity
* Y 23. municipal hospital / CC Govt health professional / CHR maternity
* Y 24. health center/post / HD maternity / district hospital / rural hosptital
* Y 25. maternity / CSI / healthcenter / urban municipal clinic
* N 26. other public sector / Health cabin / dispensary
* N 27. Other public sector, except Tz & Zim where Y 27. clinic (government/parastatal)-Tz / rural health center-Zim
* Y 31. private hospital/clinic /  referal/spec. hospital(religious/ 
* Y 32. medical center / mission hospital / private doctor / religious institution-Niger
* Y 33. medical post / mission health center / hospital (religious/voluntary)
* Y 34. blm / health center (religious/voluntary)
* N 35. dispensary (religious/voluntary)
* N 36. other private sector
* Y 41. NGO hospital/clinic / Health facility (imputed) / specialized hospital (private)-Tz / mission hospital - Zim
* Y 42. hospital (private) 
* Y 43. health center (private)
* N 44. dispensary (private)
* Y 45. clinic (private)
* N 95. Abroad
* N 96. other 

