# clear work space
rm(list=ls()) # delete global environment
cat("\014")   # clear screen
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')   
path= getwd() # allow dynamic paths for usage on multiple machines

#install.packages('plm')
library(plm)
require(lmtest) 
library(DescTools)
library(tidyr)
library(dplyr)
library(jtools)
library(foreign)   
#library(heaven)
library(readstata13)
library(lfe)
df = read.dta13("http://www.stata-press.com/data/r14/nlswork.dta")
df$cons= 1




# simple linear regression
model1 =lm(ln_wage ~   grade + age + tenure + ttl_exp + not_smsa  +south, data=df)
summary(model1)   



effect_plot(model1, pred = grade, interval = TRUE, plot.points = TRUE,
            x.label = "grade",
            y.label = "Ln(wage)",
            pred.labels = NULL,
            main.title = "the effect of grades on wages",)


# same as above but with lfe library
model1_lfe = felm(ln_wage ~    grade + age + ttl_exp + tenure+ not_smsa  +south
                  | 0 | 0 | 0 ,data = df, exactDOF=TRUE)
summary(model1_lfe) 




model2 = plm(ln_wage ~  grade +  age + ttl_exp + tenure + not_smsa + south,
             data=df,
             index=c('idcode', 'year'), model = "within")
summary(model2)  ## id fixed effects but no clustering


model2_lfe = felm(ln_wage ~ cons+  grade +   age + ttl_exp + tenure+ not_smsa  +south
                  | idcode | 0 | 0 ,data = df, exactDOF=TRUE)
summary(model2_lfe) 




model3 = plm( ln_wage ~   age + ttl_exp + tenure+  not_smsa  +south, data=df, 
              index=c('idcode', 'year'), model = "within", effect='individual')  # effect='individual' is the default, so could have dropped this
#summary(model3) #  <- this gives unclustered errors
coeftest(model3, vcov=vcovHC(model3,type="HC0",cluster="group"))[1:5,1:4]  #<- this gives clustered errors


model3_lfe = felm(ln_wage ~   age + ttl_exp + tenure+  not_smsa  +south
                  | idcode | 0 | idcode ,data = df, exactDOF=TRUE)
summary(model3_lfe) ## somewhat differrent...



model4 = plm( ln_wage ~  grade + age + ttl_exp + tenure+  not_smsa  +south ,
              data=df, index=c('idcode', 'year'), model = "within", effect="time")  # time fixed effects
#summary(model4)  #  <- this gives unclustered errors
coeftest(model4, vcov=vcovHC(model4,type="HC0",cluster="group"))[1:6,1:4] # <- this gives clustered errors

model4_lfe = felm(ln_wage ~ grade +  age + ttl_exp + tenure+  not_smsa  +south
                  | year | 0 | idcode ,data = df, exactDOF=TRUE)
summary(model4_lfe) 


model5 = plm( ln_wage ~    age + ttl_exp + tenure+  not_smsa  + south  
                , data=df, index=c('idcode', 'year'), model="within", effect ="twoways")  # time and id fixed effects
coeftest(model5, vcov=vcovHC(model5,type="HC0",cluster="group")) # <- this gives clustered errors

model5_lfe = felm(ln_wage ~   age + ttl_exp + tenure+  not_smsa  +south
                  | idcode + year | 0 | idcode ,data = df, exactDOF=FALSE)
summary(model5_lfe) ## somewhat differrent...



model6_lfe = felm(ln_wage ~   age + ttl_exp + tenure+  not_smsa  +south
                  | idcode + year | 0 | idcode + year,data = df, exactDOF=TRUE)
summary(model6_lfe) ## somewhat differrent...



model7 = felm(ln_wage ~  age + ttl_exp + tenure+  not_smsa  + south
              | idcode + year | 0 | idcode +wks_work, 
              data = df, exactDOF=TRUE)
summary(model7)





coeftest(model5, cluster.vcov(model5, ~ idcode))








vcov_firm <- cluster.vcov(model5, df$idcode)
coeftest(model5, vcov_firm)

summary(model5)
se <- function(object) tail(sqrt(diag(object)), 1)
se(vcovHC(model5))
se(cluster.vcov(model5,df$idcode))


model5 = felm(ln_wage ~    age + ttl_exp + tenure+  not_smsa  + south
              | idcode + year | 0 | idcode , 
              data = df, exactDOF=TRUE)
summary(model5)

model6 = felm(ln_wage ~   grade + age + ttl_exp + tenure+  not_smsa  + south
              | idcode + year | 0 | idcode +wks_work, 
              data = df, cmethod = 'cgm2',exactDOF=TRUE)
summary(model6)

