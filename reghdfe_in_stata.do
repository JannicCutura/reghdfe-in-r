********************************************************************************
*
* This code demonsrates equivalence of R and Stata's reghdfe method
*
********************************************************************************
cls
webuse nlswork, clear
xtset idcode year
gen cons = 1

* model 1
reg ln_w grade age ttl_exp tenure not_smsa south

* model 2
reghdfe ln_w grade age ttl_exp tenure not_smsa south, abs(idcode) 

* model 3
reghdfe ln_w grade age ttl_exp tenure not_smsa south, abs(idcode)  cluster(idcode)

* model 4
reghdfe ln_w grade age ttl_exp tenure not_smsa south, abs(year)  cluster(idcode)
xtreg ln_w grade age ttl_exp tenure not_smsa south i.year, re cluster(idcode)

* model 5
reghdfe ln_w  age ttl_exp tenure not_smsa south, abs(idcode year)  cluster(idcode)

* model 6
reghdfe ln_w  age ttl_exp tenure not_smsa south, abs(idcode year)  cluster(idcode year)

* model 7
reghdfe ln_w grade age ttl_exp tenure not_smsa south, abs(idcode year)  cluster(idcode wks_work)

