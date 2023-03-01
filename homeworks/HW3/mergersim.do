net from http://www.bjornerstedt.org/stata/mergersim
net install mergersim

global dir "C:/Users/chv7bg/Documents/empirical-io"
cd "$dir/homeworks/HW3"

use "airline1997dataMERGERSIM.dta", clear

egen firmID = group(tkcarrier)
egen marketID = group(market)

xtset marketID

mergersim init, nests(carrierType) price(MdW_oneway_itinfare_ticket) /// 
	quantity(totalpassengers) marketsize(marketsize) firm(firmID)

