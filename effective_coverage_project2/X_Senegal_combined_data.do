



* SENEGAL 2017
use "/Users/EWilson/Desktop/Countdown/Data/Senegal/2017/SNBR7ZFL.DTA", clear
keep caseid v001 v002 v003 v005 v008 v012 v015 v023 v024 v025 v106 v190 v224 v501 midx b3 m3a m3b m3c m3d m3e m3f m3g m3h m3i m3j m3k m3l m3m m3n m15 m61 m63 m67
duplicates report caseid
drop caseid
gen caseid = "1_" + string(v001) + "  " + string(v002) + "  " + string(v003)
label variable caseid "case identification"
gen v023_new = "1_" + string(v023)
drop v023
rename v023_new v023
label variable v023 "stratification used in sample design"
duplicates report caseid
save "/Users/EWilson/Desktop/Countdown/Data/Senegal/2017/SNBR7ZFL_append.DTA", replace



* SENEGAL 2018
use "/Users/EWilson/Desktop/Countdown/Data/Senegal/2018/SNBR81FL.DTA", clear
keep caseid v001 v002 v003 v005 v008 v012 v015 v023 v024 v025 v106 v190 v224 v501 midx b3 m3a m3b m3c m3d m3e m3f m3g m3h m3i m3j m3k m3l m3m m3n m15 m61 m63 m67
duplicates report caseid
drop caseid
gen caseid = "2_" + string(v001) + "  " + string(v002) + "  " + string(v003)
label variable caseid "case identification"
gen v023_new = "2_" + string(v023)
drop v023
rename v023_new v023
label variable v023 "stratification used in sample design"
duplicates report caseid
save "/Users/EWilson/Desktop/Countdown/Data/Senegal/2018/SNBR81FL_append.DTA", replace



* SENEGAL 2019
use "/Users/EWilson/Desktop/Countdown/Data/Senegal/2019/SNBR8BFL.DTA", clear
keep caseid v001 v002 v003 v005 v008 v012 v015 v023 v024 v025 v106 v190 v224 v501 midx b3 m3a m3b m3c m3d m3e m3f m3g m3h m3i m3j m3k m3l m3m m3n m15 m61 m63 m67
duplicates report caseid
drop caseid
gen caseid = "3_" + string(v001) + "  " + string(v002) + "  " + string(v003)
label variable caseid "case identification"
gen v023_new = "3_" + string(v023)
drop v023
rename v023_new v023
label variable v023 "stratification used in sample design"
duplicates report caseid
save "/Users/EWilson/Desktop/Countdown/Data/Senegal/2019/SNBR8BFL_append.DTA", replace



* PUT THREE TOGETHER
use "/Users/EWilson/Desktop/Countdown/Data/Senegal/2017/SNBR7ZFL_append.DTA", clear

append using "/Users/EWilson/Desktop/Countdown/Data/Senegal/2018/SNBR81FL_append.DTA"

append using "/Users/EWilson/Desktop/Countdown/Data/Senegal/2019/SNBR8BFL_append.DTA"


save "/Users/EWilson/Desktop/Countdown/Data/Senegal/combined/continuous2017_2018_2019.DTA", replace



