clear
set memory 50m
set more off

use railway

***************************************************************************************************
* Table 2
su  gr tgq lakes po

***************************************************************************************************
* Table 3, Column 1
ge month2=week/4
replace month2=month2+0.9
replace month2=int(month2)
quietly tab month2, generate(monthdu)
drop month2

* following lines just to define DM
ge year=1880 if week<53
replace year=1881 if week>52 & week<105
replace year=1882 if week>104 & week<157
replace year=1883 if week>156 & week<209
replace year=1884 if week>208 & week<261
replace year=1885 if week>260 & week<313
replace year=1886 if week>312

sort year month week
by year: gen countweek=_n

ge DM1=1 if _n>=28 & _n<=166
replace DM1=0 if DM1==.
ge DM2=1 if _n>=167 & _n<=181
replace DM2=0 if DM2==.
ge DM3=1 if _n>=182 & _n<=323
replace DM3=0 if DM3==.
ge DM4=1 if _n>=324
replace DM4=0 if DM4==.

drop countweek year week month

* now define seasonal dummies
ge month1 =  monthdu1+ monthdu14+ monthdu27+ monthdu40+ monthdu53+ monthdu66+ monthdu79
ge month2 =  monthdu2+ monthdu15+ monthdu28+ monthdu41+ monthdu54+ monthdu67+ monthdu80
ge month3 =  monthdu3+ monthdu16+ monthdu29+ monthdu42+ monthdu55+ monthdu68+ monthdu81
ge month4 =  monthdu4+ monthdu17+ monthdu30+ monthdu43+ monthdu56+ monthdu69+ monthdu82
ge month5 =  monthdu5+ monthdu18+ monthdu31+ monthdu44+ monthdu57+ monthdu70
ge month6 =  monthdu6+ monthdu19+ monthdu32+ monthdu45+ monthdu58+ monthdu71
ge month7 =  monthdu7+ monthdu20+ monthdu33+ monthdu46+ monthdu59+ monthdu72
ge month8 =  monthdu8+ monthdu21+ monthdu34+ monthdu47+ monthdu60+ monthdu73
ge month9 =  monthdu9+ monthdu22+ monthdu35+ monthdu48+ monthdu61+ monthdu74
ge month10= monthdu10+ monthdu23+ monthdu36+ monthdu49+ monthdu62+ monthdu75
ge month11= monthdu11+ monthdu24+ monthdu37+ monthdu50+ monthdu63+ monthdu76
ge month12= monthdu12+ monthdu25+ monthdu38+ monthdu51+ monthdu64+ monthdu77
drop  monthdu1- monthdu82

* now take logs
ge logtgq=log(tgq)
ge loggr=log(gr)
drop  gr tgq

