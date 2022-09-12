use "C:\Users\pkath\OneDrive\Рабочий стол\Курсовая 2022\grp_weather.dta", clear
gen vrp_pc=vrp/pop
gen lnvrp_pc=log(vrp_pc)

gen lnA_pc=log(vrp_pc*A/100)
gen lnB_pc=log(vrp_pc*B/100)
gen lnC_pc=log(vrp_pc*C/100)
gen lnDE_pc=log(vrp_pc*DE/100)
gen lnF_pc=log(vrp_pc*F/100)
gen lnG_pc=log(vrp_pc*G/100)
gen lnI_pc=log(vrp_pc*I/100)
gen lnHJ_pc=log(vrp_pc*HJ/100)
gen lnK_pc=log(vrp_pc*K/100)
gen lnL_pc=log(vrp_pc*L/100)
gen lnON_pc=log(vrp_pc*ON/100)
gen lnPM_pc=log(vrp_pc*PM/100)
gen lnQ_pc=log(vrp_pc*Q/100)
gen lnSR_pc=log(vrp_pc*SR/100)


histogram vrp_pc
histogram lnvrp_pc

sort code year
xtset code year

xtdes
xtsum

*xtline temp_wa_1 temp_wa_2 temp_wa_3 if code==14
*xtline lnvrp if code==14
*xtline unempl_rate if code==14

correlate lnvrp_pc temp_wa_1 temp_wa_2 temp_wa_3 prec_wa_1 prec_wa_2 prec_wa_3 wrkfrc unempl_rate if code==14
correlate temp_wa_1 temp_wa_2 temp_wa_3 temp_wa_4 temp_wa_5 temp_wa_6 temp_wa_7 temp_wa_8 temp_wa_9 temp_wa_10 temp_wa_11 temp_wa_12 prec_wa_1 prec_wa_2 prec_wa_3 prec_wa_4 prec_wa_5 prec_wa_6 prec_wa_7 prec_wa_8 prec_wa_9 prec_wa_10 prec_wa_11 prec_wa_12


use "C:\Users\pkath\OneDrive\Рабочий стол\Курсовая 2022\grp_weather.dta", clear

foreach var of varlist lnvrp_pc lnA_pc-lnSR_pc{
forvalues i=1/12{
reghdfe `var' temp_wa_`i' prec_wa_`i' wrkfrc unempl_rate, absorb(code year)  vce(robust)
est store `var'`i'
}
}




//таблица с регионами и их долями ВРП

//проверить связь между сектором А и другими значимыми секторами.
//таблица сектора и их связи с месяцами

use "C:\Users\pkath\OneDrive\Рабочий стол\Курсовая 2022\grp_weather.dta", clear

forvalues i=1/12{
gen eff_temp_`i'=0
gen eff_prec_`i'=0

egen sd_temp_`i'=sd(temp_wa_`i'), by(code)
egen sd_prec_`i'=sd(prec_wa_`i'), by(code)

}

foreach var of varlist lnA_pc-lnSR_pc{

gen eff_temp_`var'=0
gen eff_prec_`var'=0

reghdfe `var' temp_wa_1-temp_wa_12 prec_wa_1-prec_wa_12 wrkfrc unempl_rate, absorb(code year) vce(robust) 
forvalues i=1/12{

gen z_t=abs(_b[temp_wa_`i']/_se[temp_wa_`i'])
gen d_z_t = 0
replace d_z_t = 1 if  z_t >= 1.645

gen z_p=abs(_b[prec_wa_`i']/_se[prec_wa_`i'])
gen d_z_p = 0
replace d_z_p = 1 if  z_p >= 1.645

*насколько % изменится отрасль А в январе для каждого региона при изменении на среднемесячное отклонение
gen temp_`var'_`i'=_b[temp_wa_`i']*d_z_t*sd_temp_`i'
gen prec_`var'_`i'=_b[prec_wa_`i']*d_z_p*sd_prec_`i'
*эффект погоды для отрасли каждого региона
replace eff_temp_`var'= eff_temp_`var'+ temp_`var'_`i'
replace eff_prec_`var'= eff_prec_`var'+ prec_`var'_`i'

replace eff_temp_`i'=eff_temp_`i'+temp_`var'_`i'
replace eff_prec_`i'=eff_prec_`i'+prec_`var'_`i'

drop z_t d_z_t z_p d_z_p temp_`var'_`i' prec_`var'_`i'
}
}

forvalues i=1/12{
drop sd_temp_`i' sd_prec_`i' 

}
gen chng_temp=0
gen chng_prec=0
gen chng = 0

foreach var of varlist A-SR{
replace chng_temp = chng_temp + eff_temp_ln`var'_pc*`var'
replace chng_prec = chng_prec + eff_prec_ln`var'_pc*`var'
}
replace chng = chng + abs(chng_prec) + abs(chng_temp)
save "C:\Users\pkath\OneDrive\Рабочий стол\Курсовая 2022\grp_weather.dta", replace
*ПЕРЕПУТАНЫ АЙДИ
drop if year!=2019
spmap chng if id!=12 using rucoord, id(id) fcolor(RdYlGn)
spmap A if id!=12 using rucoord, id(id) fcolor(RdYlGn)

gen abs_chng_temp = abs(chng_temp) 
spmap abs_chng_temp if id!=12 using rucoord, id(id) fcolor(Reds)
gen abs_chng_prec = abs(chng_prec)
spmap abs_chng_prec if id!=12 using rucoord, id(id) fcolor(YlGnBu) 
*АНАЛИЗ ЗАВИСИМОСТИ ОТРАСЛЕЙ ДРУГ ОТ ДРУГА




*Вдруг понадобится

egen sd_temp=sd(temp_wa_1-temp_wa_12), by(code)
egen sd_prec=sd(prec_wa_1-prec_wa_12), by(code)


foreach var of varlist lnA_pc-lnSR_pc{

gen eff_temp_`var'=0
gen eff_prec_`var'=0

reghdfe `var' temp_wa_1-temp_wa_12 prec_wa_1-prec_wa_12 wrkfrc unempl_rate, absorb(code year) vce(robust) 
forvalues i=1/1{

di _b[temp_wa_`i']/_se[temp_wa_`i']
gen pv_t=exp(abs(_b[temp_wa_`i']/_se[temp_wa_`i'])*(-0.717)-0.416*((abs(_b[temp_wa_`i']/_se[temp_wa_`i'])^2)))
gen d_pv_t = 0
replace d_pv_t = 1 if  pv_t <= 0.1

gen pv_p=exp(abs(_b[prec_wa_`i']/_se[prec_wa_`i'])*(-0.717)-0.416*((abs(_b[prec_wa_`i']/_se[prec_wa_`i'])^2)))
gen d_pv_p = 0
replace d_pv_p = 1 if  pv_p <= 0.1

gen temp_`var'_`i'=_b[temp_wa_`i']*d_pv_t 
gen prec_`var'_`i'=_b[prec_wa_`i']*d_pv_p 

replace eff_temp_`var'_`i'= eff_temp_`var'_`i'+ temp_`var'_`i'
replace eff_prec_`var'_`i'= eff_prec_`var'_`i'+ prec_`var'_`i'

drop pv_t d_pv_t pv_p d_pv_p temp_`var'_`i' prec_`var'_`i'

}
}


foreach var of varlist temp_lnA_pc_1-prec_lnSR_pc_12{
if missing(`var'[1]) {
drop `var'
 }
 }
 
 foreach var of varlist lnA_pc-lnSR_pc{
	list prec_`var'_1 prec_`var'_2  prec_`var'_3 prec_`var'_4  prec_`var'_5 prec_`var'_6  prec_`var'_7 prec_`var'_8  prec_`var'_9 prec_`var'_10  prec_`var'_11 prec_`var'_12  if year==2019 & code==14
}



