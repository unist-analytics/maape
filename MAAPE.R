
##set up the working directory
#Choose "MAAPE.R"
file = file.choose()
WD = substr(file, 1, nchar(file)-nchar("MAAPE.R"))
setwd(WD)

##Output images will be generated here
dir.create("output data")

############ Figure 1 ###############
library(Mcomp)
data(M3)
library(fma)

postscript("./output data/Figure 1.eps",width = 8.0, height = 3.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(c(1:36),productC,ylab="Unit sold",type="b",xlab="Month",lwd=3)
abline(v=24.5,lty=2,lwd=3)
dev.off() 


############ Figure 3 ###############
library(fields)
library(akima)


x=seq(0.1,10,0.1)
y=sort(x, decreasing = TRUE)
abc=function(a,b){
abs((a-b)/(a+b)*2)
}

AAPE=function(a,b){
atan(abs(a-b)/a)
}

SAPE=function(a,b){
abs((a-b)/(a+b)*2)
}

APE=function(a,b){
abs(a-b)/a #MAPE
}


##Fig 3 - (a)

postscript("./output data/Figure 3 - (a).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
z=outer(x, y, FUN=APE)
Z=c(z)
X=rep(x,length(y))
Y=rep(y,each=length(x))
akima.li <- interp(X,Y,Z, duplicate="median")
image.plot(akima.li,add=FALSE,xlab="A", ylab="F")
dev.off()



##Fig 3 - (b)

postscript("./output data/Figure 3 - (b).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
dat_diag=cbind(x,y,diag(z))
plot(dat_diag[,1],dat_diag[,3],xlab="",ylab="APE",type="l",axes=FALSE)
axis(2, pretty(round(dat_diag[,3],2),3))
axis(1, at=0:10,labels=c(10:0),line=1)
mtext("F",1,line=1,at=-0.2)
axis(1, at=0:10,labels=c(0:10),line=3)
mtext("A",1,line=3,at=-0.2)
dev.off() 


##Fig 3 -(c)

postscript("./output data/Figure 3 - (c).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
z=outer(x, y, FUN=AAPE)
Z=c(z)
X=rep(x,length(y))
Y=rep(y,each=length(x))
akima.li <- interp(X,Y,Z, duplicate="median")
image.plot(akima.li,add=FALSE,xlab="A", ylab="F")
dev.off() 


##Fig 3 - (d)
postscript("./output data/Figure 3 - (d).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
dat_diag=cbind(x,y,diag(z))
plot(dat_diag[,1],dat_diag[,3],xlab="",ylab="AAPE",type="l",axes=FALSE)
axis(2, pretty(round(dat_diag[,3],2),3))
axis(1, at=0:10,labels=c(10:0),line=1)
mtext("F",1,line=1,at=-0.2)
axis(1, at=0:10,labels=c(0:10),line=3)
mtext("A",1,line=3,at=-0.2)
dev.off() 



############ Figure 4 ###############


##Fig 4 - (a)

z=outer(x, y, FUN=APE)
Z=c(z)
X=rep(x,length(y))
Y=rep(y,each=length(x))

postscript("./output data/Figure 4 - (a).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
dat_diag=cbind(x,y,diag(z))
dat_diag2= dat_diag[9.2>y,]
plot(dat_diag2[,1],dat_diag2[,3],xlab="",ylab="APE",type="l",axes=FALSE)
axis(2, pretty(round(dat_diag2[,3],2),3))
axis(1, at=1:10,labels=c(10:1),line=1)
mtext("F",1,line=1,at=-0.2)
axis(1, at=1:10,labels=c(1:10),line=3)
mtext("A",1,line=3,at=-0.2)
dev.off() 


##Fig 4 - (b)

z=outer(x, y, FUN=AAPE)
Z=c(z)
X=rep(x,length(y))
Y=rep(y,each=length(x))

postscript("./output data/Figure 4 - (b).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
dat_diag=cbind(x,y,diag(z))
dat_diag3= dat_diag[9*x>y,]
plot(dat_diag3[,1],dat_diag3[,3],xlab="",ylab="AAPE",type="l",axes=FALSE)
axis(2, pretty(round(dat_diag3[,3],2),10))
axis(1, at=1:10,labels=c(10:1),line=1)
mtext("F",1,line=1,at=0.8)
axis(1, at=1:10,labels=c(1:10),line=3)
mtext("A",1,line=3,at=0.8)
dev.off() 


############ Figure 5 ###############

x=seq(0,6,0.1)
y=atan(x)


x2=seq(1.5,6,0.1)
y2=pi/2-1/x2

postscript("./output data/Figure 5.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(x,y,type="l",ylab="y",lwd=2,ylim=c(0,2))
lines(x,x,lwd=2,lty=2)
lines(x2,y2,col=2,lwd=2)
legend(4, 0.5, c("y=arctan(x)", "y=x", "y=pi/2-1/x"), col = c(1, 1, 2),
       box.col  ="white",lwd=c(2,2,2),lty = c(1, 2, 1),  merge = T, bg = "gray90")
dev.off()


############ Figure 6 & 7 ###############

AL<<-0
AH<<-5

findlossoptimal<-function(loss,rg,sep=0.1){
  tmp<-c()
  for(i in seq(rg[1],rg[2],sep)){
    tmp<-c(tmp,mean(loss(i)))
  }
  tb=cbind(seq(rg[1],rg[2],sep),tmp)
  return(list(tb=tb,op=as.numeric(tb[tb[,2]==min(tb[,2]),1])))
}

APE=function(A,P,rp=0.01){
  A[A==0]=rp
  abs(1-P/A)
}

AAPE=function(A,P){
  tmp<-cbind(A,P)
  tmp[rowSums(tmp)==0,]=0.1
  out=atan(abs(1-tmp[,2]/tmp[,1]))
  return(out)
}

Aloss1NB=function(p){
  rng=c(0:(50*m))
  fx<-dnbinom(rng,r,mu=m)
  sum(APE(rng,p,rp=0.01)*(fx))
}

Aloss1UD=function(p){
  rng=c(0:5)
  fx<-rep(1/6,6)
  sum(APE(rng,p,rp=0.01)*(fx))
}

Aloss2UD=function(p){
  rng=c(0:5)
  fx<-rep(1/6,6)
  max(sum(APE(rng[rng<p],p,rp=0.01)*fx[rng<p]),sum(APE(rng[rng>=p],p,rp=0.01)*fx[rng>=p])) 
}

Aloss2NB=function(p){
  rng=c(0:(50*m))
  fx<-dnbinom(rng,r,mu=m)
  max(sum(APE(rng[rng<p],p,rp=0.01)*fx[rng<p]),sum(APE(rng[rng>=p],p,rp=0.01)*fx[rng>=p])) 
}

SE1NB=function(p){
  rng=c(0:(50*m))
  fx<-dnbinom(rng,r,mu=m)
  sum(SE(rng,p)*(fx))
}

AAloss1UD=function(p){
  rng=c(0:5)
  fx<-rep(1/6,6)
  sum(AAPE(rng,p)*(fx))
}

AAloss2UD=function(p){
  rng=c(0:5)
  fx<-rep(1/6,6)
  if(p==0) return(sum(AAPE(rng[rng>=p],p)*fx[rng>=p]))
  if(p>0) return(max(sum(AAPE(rng[rng<p],p)*fx[rng<p]),sum(AAPE(rng[rng>=p],p)*fx[rng>=p])))
}

AAloss1NB=function(p){
  rng=c(0:(50*m))
  fx<-dnbinom(rng,r,mu=m)
  sum(AAPE(rng,p)*(fx))
}

AAloss2NB=function(p){
  rng=c(0:(50*m))
  fx<-dnbinom(rng,r,mu=m)
  if(p==0) return(sum(AAPE(rng[rng>=p],p)*fx[rng>=p]))
  if(p>0) return(max(sum(AAPE(rng[rng<p],p)*fx[rng<p]),sum(AAPE(rng[rng>=p],p)*fx[rng>=p])))
  
}

out=c()
for(pp in seq(0.001,0.999,0.001)){
  out<-c(out,abs(log(0.15)/log(pp)*(1-pp)/pp-2.5))
}

p=seq(0.001,0.999,0.001)[out==min(out)]
r<<-log(0.15)/log(p)
m<<-r*(1-p)/p

tmp<-findlossoptimal(AAloss1NB,c(0,5))

AD=function(A,P){
  abs(A-P)
}

AD1GP<-function(p){
  rng=c(0:(50*m))
  lambda=thm*m
  fx<-as.numeric(lapply(rng,GP,lambda))
  sum(AD(rng,p)*(fx))
}

AD1NB=function(p){
  rng=c(0:(50*m))
  fx<-dnbinom(rng,r,mu=m)
  sum(AD(rng,p)*(fx))
}

AUD1=findlossoptimal(Aloss1UD,c(0,5),0.01)$op 
AUD2=findlossoptimal(Aloss2UD,c(0,5),0.01)$op 
AAUD1=findlossoptimal(AAloss1UD,c(0,5),0.01)$op 
AAUD2=max(findlossoptimal(AAloss2UD,c(0,5),0.01)$op )

ADNB<- findlossoptimal(AD1NB,c(0,5),0.01)$op
ANB1=findlossoptimal(Aloss1NB,c(0,5),0.01)$op 
ANB2=findlossoptimal(Aloss2NB,c(0,5),0.01)$op 
AANB1=findlossoptimal(AAloss1NB,c(0,5),0.01)$op 
AANB2=max(findlossoptimal(AAloss2NB,c(0,5),0.01)$op )


###Figure 6-A
postscript("./output data/Figure 6 - (a).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),AUD1),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AAUD1),col=2,lwd=3,lty=2)
abline(v=2.5,col=1,lty=3,lwd=3);
dev.off() 


###Figure 6-B
postscript("./output data/Figure 6 - (b).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),AUD2),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AAUD2),col=2,lwd=3,lty=2)
abline(v=2.5,col=1,lty=3,lwd=3);
dev.off() 


###Fiugre 7-A
postscript("./output data/Figure 7 - (a).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),ANB1),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AANB1),col=2,lwd=3,lty=2)
abline(v=ADNB,col=1,lty=3,lwd=2)
abline(v=2.5,col=2,lty=1,lwd=1)
dev.off() 


###Figure 7-B
postscript("./output data/Figure 7 - (b).eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),ANB2),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AANB2),col=2,lwd=3,lty=2)
abline(v=ADNB,col=1,lty=3,lwd=2)
abline(v=2.5,col=2,lty=1,lwd=1)
dev.off() 


############ Figure 8 ###############

productC[1:24]

F=mean(productC[1:24])

mean(SAPE(productC[1:24],F))
mean(SAPE(productC[25:36],F))

F2=productC[25:36]+rnorm(12,0,0.1)

mean(AAPE(productC[1:24],F))
mean(AAPE(productC[25:36],F))
mean(SAPE(productC[25:36],F))
mean(APE(productC[c(28,29,32,34)],F))

mean(AAPE(productC[25:36],F2))
mean(SAPE(productC[25:36],F2))
mean(APE(productC[25:36],F2))

mean(AAPE(productC[25:36],F2))
mean(SAPE(productC[25:36],F2))


mean(APE(productC[c(28,29,32,34)],F))
mean(APE(productC[c(28,29,32,34)],c(7,1.1,0.9,1.1)))

mean(AAPE(productC[c(28,29,32,34)],F))
mean(AAPE(productC[c(28,29,32,34)],c(7,1.1,0.9,1.1)))

mean(AAPE(productC[25:36],F))
mean(AAPE(productC[25:36],c(0.1,-0.1,0.1,6,1.1,0.1,-0.1,0.9,0.1,1.1,-0.1,0.1)))

mean(APE(productC[c(28,29,32,34)],F))
mean(APE(productC[c(28,29,32,34)],c(6,1.1,0.9,1.1)))


postscript("./output data/Figure 8.eps",width = 5.0, height = 5.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(c(25:36),productC[25:36],type="l",xlab="Month", ylab="Sales",lwd=3, ylim=c(-0.5,7.5))
abline(h=F,col=4,lwd=3,lty=2)
lines(c(25:36),c(0.1,-0.1,0.1,7,1.1,0.1,-0.1,0.9,0.1,1.1,-0.1,0.1),col=2,lwd=3,lty=3)
legend(31.5, 7, c("Actual", "Forecast 1", "Forecast 2"), col = c(1,4,2),
lty = c(1,2,3), lwd=c(3,3,3),cex=0.9,merge = T,box.col="white")
dev.off() 


############ Figure 9 ###############

dat=read.csv("./All833.csv")
sku833=unique(dat$Sku)
weekdays=sort(unique(dat$WeekInDayFmt))

dat833=read.csv("./dat833.csv")

all_store=unique(dat833$Store)
dat2=c()
for(ss in all_store){
  skusinstore=unique(dat833[dat833$Store==ss,]$Sku)
  dd1=subset(dat833,(Store==ss))
  for(skus in skusinstore){
    dd=subset(dd1,(Sku==skus))  
    rr=rep(0,105)
    rr[match(dd$WeekInDayFmt,weekdays)]=dd$Sales
    if(sum(rr)>0) dat2=rbind(dat2,c(ss,skus,rr))
  }
}

outsample_compare=function(chs,x,x10){
  if(chs==1){
    fit.ets=ets(x)
    fcst=forecast(fit.ets,h=10)$mean
  }
  if(chs==2){
    fit.holt <- HoltWinters(x,gamma=FALSE,beta=FALSE)
    fcst=forecast(fit.holt,h=10)$mean
  } 
  if(chs==3){
    fit.arima <- forecast(auto.arima(x),h=10)
    fcst=forecast(fit.arima,h=10)$mean
  } 
  if(chs==4){
    fit.croston=croston(x, h=10, alpha=0.5)
    fcst=fit.croston[1]$mean
  } 
  
  ff1=APEforReal(as.numeric(x10),as.numeric(fcst))
  ff2=AAPEforReal(as.numeric(x10),as.numeric(fcst))
  denom=mean(abs(x[2:length(x)]-x[1:(length(x)-1)])) 
  ff3=ASE(as.numeric(x10),as.numeric(fcst),denom)
  return( c(mean(ff1[ff1<10000]),mean(ff2),mean(ff3)))
  
}


result1=c()
result2=c()
result3=c()
result4=c()
result5=c()

rows<-c(2,4,8,6)
demand<-dat2[rows,-c(1:2)]


##Figure 9 summation

postscript("./output data/Figure 9 - Summation.eps",width = 12.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[1,],type="l",xlab="Weeks",ylab="Sales",cex.lab=1.4,ylim=c(0,110),lwd=2,col=1)
points(demand[1,],pch=1,col=1)
for(j in 2:4){
  lines(demand[j,],type="l",lwd=2,col=j)
  points(demand[j,],pch=j,col=j)
}
legend(95,90, c("SKU A","SKU B","SKU C","SKU D"), col = 1:4, pch = c(1,2,3,4))
dev.off() 


##Figure 9-a
postscript("./output data/Figure 9 - skuA.eps",width = 12.0, height = 4.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[1,],type="l",xlab="Weeks",ylab="Sales of SKU D",cex.lab=1.4,lwd=2,col=1)
points(demand[1,],pch=1,col=1)
dev.off() 


##Figure 9-b
postscript("./output data/Figure 9 - skuB.eps",width = 12.0, height = 4.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[2,],type="l",xlab="Weeks",ylab="Sales of SKU A",cex.lab=1.4,lwd=2,col=1)
points(demand[2,],pch=1,col=1)
dev.off() 


##Figure 9-c
postscript("./output data/Figure 9 - skuC.eps",width = 12.0, height = 4.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[3,],type="l",xlab="Weeks",ylab="Sales of SKU B",cex.lab=1.4,lwd=2,col=1)
points(demand[3,],pch=1,col=1)
dev.off() 


##Figure 9-d
postscript("./output data/Figure 9 - skuD.eps",width = 12.0, height = 4.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[4,],type="l",xlab="Weeks",ylab="Sales of SKU C",cex.lab=1.4,lwd=2,col=1)
points(demand[4,],pch=1,col=1)
dev.off() 


############ Figure 10 ###############

SAPE=function(a,b){
  re=abs((a-b)/(a+b)*2)
  re[(a==0)&(b==0)]=0
  return(re)
}

sim<-function(v){
  result=c()
  n=10
  true=rnbinom(n,2,mu=1)
  for(k in 1:100){
    
    actual1=true+rnorm(n,0,0.01)
    actual2=true+rnorm(n,0,0.04)
    
    if(0){
      plot(true,ylim=c(-0.5,2.5))
      lines(actual1,col=2)
      lines(actual2,col=4)
    }
    
    
    d1=mean(APE(true,actual1,rp=v),na.rm=T)
    d2=mean(APE(true,actual2,rp=v),na.rm=T)
    
    d3=mean(AAPE(true,actual1),na.rm=T)
    d4=mean(AAPE(true,actual2),na.rm=T)
    
    d5=mean(SAPE(true,actual1),na.rm=T)
    d6=mean(SAPE(true,actual2),na.rm=T)
    
    
    result=rbind(result,c(d1<=d2,d3<=d4,d5<=d6))
    #if(is.na(d5<=d6)) print(cbind(true,actual1,actual2))
    
  }
  return(colSums(result))
}

pp<-c()
for(vv in seq(1:1000)){
  pp<-rbind(pp,sim(0.01))
}
simple<-data.frame(1-pp[,1:2]/100)
names(simple)<-c("MAPE","MAAPE")

postscript("./output data/Figure 10.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
boxplot(as.data.frame(simple),xlab="",ylab="Error rate",cex.lab=1.5)
dev.off() 

