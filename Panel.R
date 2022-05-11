
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


optimal_panelmodel=function(ols, fixed, random,p=0.05){
  ols_m=ols
  fixed_m=fixed
  random_m=random
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




## Optimal Model: pooled, fixed, or random

optimal_pmodel=function(formula,data,p,effect="twoways"){
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


## Dynamic Panel Reg
pgmmreg=function(model){
 if (!require(pacman)) install.packages("pacman")
pacman::p_load(openxlsx,readxl,plm,texreg)
ss=summary(model, robust =TRUE)
coefficient.names <- rownames(ss$coefficients)  # extract coef names
coefficients <- ss$coefficients[,1]  # extract coefficient values
standard.errors <- ss$coefficients[,2]  # extract standard errors
significance <- ss$coefficients[,4]  #extract p-values
n<-  nobs(model) # extract log likelihood
ar1 <- ss$m1$p.value  # extract AIC
ar2 <- ss$m2$p.value  # extract BIC
sargan <- ss$sargan$p.value  # extract number of observations
gof <- c(n, ar1, ar2,sargan)  # create a vector of GOF statistics
gof.names <- c("Observations", "AR(1) test p-value", "AR(2) test p-value", "Sargan test p-value")  # names of GOFs
decimal.places <- c(FALSE, TRUE, TRUE, TRUE)  # last one is a count variable

tr <- texreg::createTexreg(coef.names = coefficient.names,
                   coef = coefficients,
                   se = standard.errors,
                   pvalues = significance,
                   gof.names = gof.names,
                   gof = gof,
                   gof.decimal = decimal.places)
 return(tr)
 }


### MATCHING
#x=c("A", "B", "C", "D","Z")
#y=c(1,4,6,3,2)

#w=c("A", "B", "C", "D","Z","B", "B", "C", "D","C", "D")
matching=function(x,y,w){
z=1:length(w)
for (i in 1:length(w)) {
  z[i]=y[match(w,x)[i]]
}
return(z)
}


### Panel Plot
#x=pdata$Liquidity_Creation  #Variable
#cat=pdata$year  #Time Variable
#xlab="xlab"  # xlabel
#ylab="ylab"  # ylabel
#fun=function(x){u=quantile(x,0.75);m=mean(x);l=quantile(x,0.25);c(u,m,l)}  #summary statistics measure

pplot=function(x,cat, xlab,ylab,fun=fun){
ag_d <- as.data.frame(aggregate(x, by=list(cat), FUN=fun))
head(ag_d)


plot=ggplot(ag_d, aes(x=Group.1))+ geom_ribbon(aes(ymin = ag_d$x[,3] , ymax =ag_d$x[,1] ),linetype = 5, alpha= 0.8, fill = "grey85") 
plot=plot+geom_line(aes(y =ag_d$x[,2] ), color="firebrick", size=0.5)+theme_bw()+  xlab(xlab) + ylab(ylab)
plot  
}

#pplot(x,cat, xlab,ylab,fun=fun)


### Panel Plot wof Different Category over time
#x=pdata$Liquidity_Creation
#cat=pdata$year
#cat2=pdata$Ownership
#xlab="xlab"
#ylab="ylab"
#fun=function(x){m=median(x);m}

pcatplot=function(x,cat, cat2, xlab,ylab, fun){
ag_d <- as.data.frame(aggregate(x, list(cat, cat2), FUN=fun))
head(ag_d)

p=ggplot(ag_d, aes(x =Group.1, y = x, col=Group.2)) + geom_line() +theme_bw()+  xlab(xlab) + ylab(ylab)+guides(fill = guide_legend(title.position = "bottom",title = ""))
p=p+theme(legend.position="bottom",legend.title = element_blank())
p
}

#pcatplot(x,cat, cat2, xlab,ylab, fun)


#### Panel Secondary Axis Time plot

#x1=pdata$MonetaryPolicy
#x2=pdata$Costoffunds
#cat=pdata$year
#xlab="xlab"
#ylab="ylab"
#ylab2="ylab2"
#fun1=function(x){m=median(x);m}
#fun2=function(x){m=sd(x);m}

psecplot=function(x1,x2,cat,xlab,ylab,ylab2,fun1, fun2){
ag_d1 <- as.data.frame(aggregate(x1, list(cat), FUN=fun1))
ag_d2<- as.data.frame(aggregate(x2, list(cat), FUN=fun2))
ag_d=merge(ag_d1, ag_d2, by="Group.1")

k=mean(mean(ag_d$x.x,na.rm = TRUE)/mean(ag_d$x.y,na.rm = TRUE),median(ag_d$x.x,na.rm = TRUE)/median(ag_d$x.y,na.rm = TRUE))
p <- ggplot(ag_d, aes(x = Group.1))+theme_bw()
p <- p + geom_line(aes(y = x.x, colour =ylab), size=1)
p <- p + geom_line(aes(y = x.y*k, colour = ylab2), size=1)
p <- p + scale_y_continuous(sec.axis = sec_axis(~./k, name = ylab2))
p <- p + scale_colour_manual(values =brewer.pal(n = 8, name = "Dark2"))
p <- p + labs(y = ylab,
              x = xlab,
              colour = "")
p <- p + theme(legend.position = c(0.9, 0.85))
p
}


#psecplot(x1,x2,cat,xlab,ylab,ylab2,fun1,fun2)



##### Generate Variable using Panel Saummary Statistics
#x=pdata$Liquidity_Creation
#by=pdata$Year
#fun=function(x){m=median(x);m}
#data=pdata
pgen=function(data,x,by,fun){
  matching=function(x,y,w){
    z=1:length(w)
    for (i in 1:length(w)) {
      z[i]=y[match(w,x)[i]]
    }
    return(z)
  }
ag_d <- as.data.frame(aggregate(x, list(by), FUN=fun))
gn=matching(ag_d$Group.1,ag_d$x ,by)
return(gn)
}



##################################################################
# Panel xtile in R
pxtile=function(x,by=y,n){
df=data.frame(x=x,by=by)
m=length(df$x)
xtl=1:m
fun=function(x){quantile(x, probs=1:n/n, na.rm=T)}
dff=aggregate(df$x, by=list(df$by), FUN=fun)
colnames(dff)[1]="by"
dfff=merge(df, dff, by="by")
for (i in which(is.na(dfff$x.x))){
xtl[i]=NA
}
for (i in which(!is.na(dfff$x.x))){ 
xtl[i]=min(which(as.vector(dfff[,3][i,])>=dfff$x.x[i]))
}
return(xtl)
}


# An Example
#df <- data.frame(team=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A','B', 'B', 'B', 'B', 'B', 'B', 'B', 'B','C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'),wins=c(2, 4, 4, 5, 7, 9, NA, 13, 15, 15, 14, 13,11, 9, 9, 8, 8, 16, 19, NA, 24, 20, 19, 18))
#pxtile(x=df$wins,by=df$team,n=3)




data("Produc", package = "plm")
formula= gsp ~ lag(gsp)+pcap + pc + emp + unemp
pooling <- plm(formula,data = Produc, index = c("state","year"), model ="pooling")
fixed <- plm(formula,data = Produc, index = c("state","year"), model ="within")
diff<- pgmm(gsp ~ lag(gsp)+pcap + pc + emp + unemp|lag(gsp,2:99),data = Produc, index = c("state","year"), model ="twosteps",effect="twoways")

#pooling
#fixed
#diff

# If two steps difference GMM lagged coeffciient is near to fixed effect model, then system gmm is prefrred.



dpmodelselect=function(pooling, fixed, diff){
  a=abs(fixed$coefficients[1]-diff$coefficients[[2]][1])
  b=abs(pooling$coefficients[2]-diff$coefficients[[2]][1])
if (a<b){
print("System GMM")
}
if(a >=b){
print("Difference GMM")
}
}

dpmodelselect(pooling, fixed, diff)


diff1 <- pgmm(gsp ~ lag(gsp)+pcap + pc + emp + unemp|lag(gsp,2:99),data = Produc, index = c("state","year"), model ="onestep",effect="twoways")
diff2<- pgmm(gsp ~ lag(gsp)+pcap + pc + emp + unemp|lag(gsp,2:99),data = Produc, index = c("state","year"), model ="twosteps",effect="twoways")
system_gmm1<- pgmm(gsp ~ lag(gsp)+pcap + pc + emp + unemp|lag(gsp,2:3),data = Produc, index = c("state","year"), model ="onestep",effect="twoways", transformation ="ld")
system_gmm2<- pgmm(gsp ~ lag(gsp)+pcap + pc + emp + unemp|lag(gsp,2:3),data = Produc, index = c("state","year"), model ="twosteps",effect="twoways", transformation ="ld")



