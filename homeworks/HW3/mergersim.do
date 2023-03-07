net from http://www.bjornerstedt.org/stata/mergersim
net install mergersim

global dir "C:/Users/chv7bg/Documents/empirical-io"
cd "$dir/homeworks/HW3"

use "airline1997dataMERGERSIM.dta", clear
// use "airline1997data.dta", clear

egen prodID = group(tkcarrier T100nonstop)
egen marketID = group(market)
egen firmID = group(tkcarrier)
egen carrierGroup = group(carrierType)
gen inMarket = 1

xtset prodID marketID

// initialize market share variables
mergersim init, nests(inMarket carrierGroup) ///
	price(MdW_oneway_itinfare_ticket) /// 
	quantity(totalpassengers) marketsize(marketsize) firm(firmID)

// estimate demand
xtreg M_ls MdW_oneway_itinfare_ticket M_lsjh M_lshg nDest ///
	marketdistanceticket, fe

// simulate a merger between AA and US
mergersim simulate if market == "ORDMIA", seller(2) buyer(26) detail


// simulate a merger between DL and US
mergersim simulate if market == "ORDMIA", seller(5) buyer(26) detail
