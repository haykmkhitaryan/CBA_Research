clear

use "/Users/anushikqamalyan/Desktop/CBA Research/Data_Freight.dta"

sum 

 corr  freight_cum rgdp_gr ind_gr agr_gr cons_gr serv_gr

describe
**Creating Time series environment**
gen date = tq(2000q1) + _n-1
format %tq date
list date in 1/4
tsset date


*Seasonality 
egen quarter = seq(), to(4)
tab quarter, gen(q)
reg rgdp_gr q2-q4
test q2=q3=q4=0
drop q1-q4

gen recent_period=0
replace recent_period=1 if date >= tq(2015q4)
 gen freight_recent=freight*recent_period

** Checking stationarity

foreach var of varlist _all {
dfuller `var'
dfuller `var', trend
dfuller `var', drift
}


tsline reer
tsline rrusgdp_gr
pperron reer
pperron rrusgdp_gr /* evidence of unit root present for REER and Real Russian GDP growth*/

gen dif_reer=d1.reer

twoway (tsline rgdp_gr) (tsline freight_cum) 




 *empty matrix for Akaike written
matrix AIC = J(2,4,0)
 * empty matrix for Schwarts written 
 matrix SIC = J(2,4,0)
 set more off
 forvalues i= 1(1)2 {
 	forvalues j = 1(1)4 {
arima rgdp_gr l(0/1)freight_cum rrusgdp_gr l(0 1).dif_reer, ar(1/`i') ma(1/`j') 
arimafit
matrix AIC[`i',`j'] = r(aic)  
matrix SIC [`i',`j'] = r(sic)
}
}

matrix list AIC
matrix list SIC

*Total GDP
arima rgdp_gr l(0/1)freight_cum rrusgdp_gr l(0 1).dif_reer, ar(1/2) ma(1/2) 

estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
estimates store m1, title(Model 1)

arima rgdp_gr l(0/1)freight_cum rrusgdp_gr l(0 1).dif_reer freight_recent, ar(1/2) ma(1/2) 

estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit     
estimates store int1, title(Interaction Model Total GDP)

*Industry

 *empty matrix for Akaike written
matrix AIC = J(2,4,0)
 * empty matrix for Schwarts written 
 matrix SIC = J(2,4,0)
 set more off
 forvalues i= 1(1)2 {
 	forvalues j = 1(1)4 {
arima ind_gr l(1 5)freight_cum rrusgdp_gr l(0/1)ind_price, ar(1/`i') ma(1/`j') 
arimafit
matrix AIC[`i',`j'] = r(aic)  
matrix SIC [`i',`j'] = r(sic)
}
}

matrix list AIC
matrix list SIC

arima ind_gr l(1 5)freight_cum rrusgdp_gr l(0/1)ind_price , ar(1/2) ma(1/2) 


estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
estimates store m2, title(Model 2)
 
 arima ind_gr l(1 5)freight_cum rrusgdp_gr l(0/1)ind_price freight_recent, ar(1/2) ma(1/2) 
estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
	 estimates store int2, title(Model Int Industry)
	 
** in long term people from other sectors switch to industry

** Agriculture 

 matrix SIC = J(2,4,0)
 set more off
 forvalues i= 1(1)2 {
 	forvalues j = 1(1)4 {
arima agr_gr l(0/1)freight_cum rrusgdp_gr l(0/1)agr_price, ar(1/`i') ma(1/`j') 
arimafit
matrix AIC[`i',`j'] = r(aic)  
matrix SIC [`i',`j'] = r(sic)
}
}

matrix list AIC
matrix list SIC


arima agr_gr l(0/1)freight_cum rrusgdp_gr agr_price , ar(1/2) ma(1/2) 

estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
estimates store m3, title(Model 3)

arima agr_gr l(0 1)freight_cum rrusgdp_gr agr_price freight_recent, ar(1/2) ma(1/2) 

estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
estimates store int3, title(Interaction Model Agriculture)


** Construction

 matrix SIC = J(2,4,0)
 set more off
 forvalues i= 1(1)2 {
 	forvalues j = 1(1)4 {
arima cons_gr l(0/1)freight_cum rrusgdp_gr l(0/1)cons_price, ar(1/`i') ma(1/`j') 
arimafit
matrix AIC[`i',`j'] = r(aic)  
matrix SIC [`i',`j'] = r(sic)
}
}

matrix list AIC
matrix list SIC

arima cons_gr l(0/1)freight_cum rrusgdp_gr l(0/1)cons_price, ar(1/2) ma(1/1) 
estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
estimates store m4, title(Model 4)


arima cons_gr l(0/1)freight_cum rrusgdp_gr l(0/1)cons_price freight_recent, ar(1/2) ma(1/1)  iterate(100)

estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
estimates store int4, title(Interaction Model Consturction)

** Services 

 matrix SIC = J(2,4,0)
 set more off
 forvalues i= 1(1)2 {
 	forvalues j = 1(1)4 {
arima serv_gr l(0/3)freight_cum rrusgdp_gr l(0/1)dif_reer freight_recent, ar(1/`i') ma(1/`j') 
arimafit
matrix AIC[`i',`j'] = r(aic)  
matrix SIC [`i',`j'] = r(sic)
}
}

matrix list AIC
matrix list SIC

arima serv_gr l(0/1)freight_cum rrusgdp_gr l(0/1)dif_reer, ar(1/2) ma(1/2) 

estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
estimates store m5, title(Model 5)

arima serv_gr l(0/1)freight_cum rrusgdp_gr l(0/1)dif_reer freight_recent, ar(1/2) ma(1/2) 

estout, cells("b(fmt(3)) p( fmt(2))")
     arimafit   
estimates store int5, title(Interaction Model Services)

predict res_serv, res
swilk res_serv /*Fail to reject normality*/ 
sfrancia res_serv /*Fail to reject */
hist res_serv
