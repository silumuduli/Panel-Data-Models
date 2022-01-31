
paneldata=function(data,pname,ptime){
if (!require(pacman)) install.packages("pacman")
pacman::p_load(openxlsx,readxl,plm)
colnames(data)[which(colnames(data)==pname)]="Panel"
colnames(data)[which(colnames(data)==ptime)]="Time"
colnames(data)=gsub(" ", "", colnames(data))
colnames(data)=gsub("\r", "", colnames(data))
colnames(data)=gsub("\n", "", colnames(data))
pdata= plm::pdata.frame(data, index = c("Panel", "Time"), drop.index = TRUE)
return(pdata)
}




pvardata=function(data,pname,ptime){
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(openxlsx,readxl,plm)
  colnames(data)[which(colnames(data)==pname)]="id"
  colnames(data)[which(colnames(data)==ptime)]="year"
  colnames(data)=gsub(" ", "", colnames(data))
  colnames(data)=gsub("\r", "", colnames(data))
  colnames(data)=gsub("\n", "", colnames(data))
  return(as.data.frame(data))
}



# Panel Unit Root Test
punitroot=function(x,lag){
l=plm::purtest(x, test = "levinlin", lags = lag, exo = "trend")
p=plm::purtest(x, test = "ips", lags = lag, exo = "trend")
punitdata=t(data.frame(l$statistic$p.value,p$statistic$p.value))
colnames(punitdata)=c("p-value")
rownames(punitdata)=c("Levin-Lin-Chu (LLC) test","Im-Pesaran-Shin (IPS) test")
return(round(punitdata,5))
}

## Optimal Model: pooled, fixed, or random

optimal_pmodel=function(formula,data,p,effect="towway"){
  ols_m=plm(formula, data = data)
  fixed_m=plm(formula, data = data,model = "within", effect = effect)
  random_m=plm(formula, data = data,model = "random",effect = effect)
  ooo=pFtest(fixed_m, ols_m)
  kkk=phtest(fixed_m, random_m)
  o=ooo$p.value
  k=kkk$p.value
  if(o<p & k <p){
    print(screenreg(fixed_m,stars = c(0.01, 0.05, 0.1),custom.model.names = c("Fixed Effect Model")))
  return(fixed_m)
    }
  if(o<p & k >p){
    print(screenreg(random_m,stars = c(0.01, 0.05, 0.1),custom.model.names = c("Random Effect Model"))) 
    return(random_m)
    }
  if(o>p & k<p){
    print(screenreg(ols_m,stars = c(0.01, 0.05, 0.1),custom.model.names = c("OLS Model")))
    return(ols_m)
    }
  if(o>p & k>p){
    print(screenreg(ols_m,stars = c(0.01, 0.05, 0.1),custom.model.names = c("OLS Model"))) 
    return(ols_m)
    }
}







