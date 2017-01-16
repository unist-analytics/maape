
library(plyr)
library(forecast)
setwd("/Users/sungilkim/Dropbox/Sungil_MAAPE_(submitted)/code")

##
fitted_compare=function(a,MEASURE){
  
  fit.ets=ets(a)
  fit.holt <- HoltWinters(a,gamma=FALSE,beta=FALSE) ## Exponential Smoothing
  fit.arima <- forecast(auto.arima(a),h=10)
  fit.croston=croston(a, h=10, alpha=0.5)
  
  fitted.ets=fit.ets$fitted
  fitted.holt=fit.holt$fitted[,1]
  fitted.arima=fit.arima$fitted
  fitted.croston=fit.croston$fitted
  
  aa1=MEASURE(a,fitted.ets)
  aa2=MEASURE(a[-c(1)],fitted.holt)
  aa3=MEASURE(a,fitted.arima)
  aa4=MEASURE(a,fitted.croston)
  
  aa1=aa1[!is.na(aa1)];aa2=aa2[!is.na(aa2)];aa3=aa3[!is.na(aa3)];aa4=aa4[!is.na(aa4)]
  return(c(mean(aa1[aa1<10000]),mean(aa2[aa2<10000]),mean(aa3[aa3<10000]),mean(aa4[aa4<10000])))
}

fcst_compare=function(chs,x,x10){
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
    fit.croston=croston(a, h=10, alpha=0.5)
    fcst=fit.croston[1]$mean
  } 
  
  ff1=APE(as.numeric(x10),as.numeric(fcst))
  ff2=AAPE(as.numeric(x10),as.numeric(fcst))
  return( c(mean(ff1[ff1<10000]),mean(ff2)))
  
}

AAPE=function(a,b){
  re=atan(abs(a-b)/a) #MAAPE    
  re[(a==0)&(b==0)]=0
  return(re)
}

MSE=function(a,b){
  re=(a-b)^2 
  return(re)
}


SAPE=function(a,b){
  abs((a-b)/(a+b)*2)
}

APE=function(a,b){
  re=abs(a-b)/a #MAPE    
  re[(a==0)&(b==0)]=0
  return(re)
}

##
dat=read.csv("All833.csv")
sku833=unique(dat$Sku)
weekdays=sort(unique(dat$WeekInDayFmt))
dat1=dat[,c("Store","Sku","WeekInDayFmt","ActualSales")]

dat2=c()
for(ss in c(2,502)){
  for(skus in unique(dat1[dat1$Store==ss,]$Sku)){
    dd=subset(dat1,(Store==ss)&(Sku==skus))  
    rr=rep(0,105)
    rr[match(dd$WeekInDayFmt,weekdays)]=dd$ActualSales
    if(sum(rr)>0) dat2=rbind(dat2,c(ss,skus,rr))
  }
}


## distance matrix

a=as.ts(dat2[1,c(3:95)])
b=as.ts(dat2[2,c(3:95)])

result_mape=c()
for(i in 1:nrow(dat2)){
  print(i)
  x=as.ts(dat2[i,c(3:95)])
  x10=as.ts(dat2[i,c(96:105)])
  min_value=fitted_compare(x,APE)
  choice=order(min_value)[1]
  
  result_mape=rbind(result_mape,c(min_value,choice,fcst_compare(choice,x,x10),round(sum(x==0)/length(x),2)))
}

result_maape=c()
for(i in 1:nrow(dat2)){
  x=as.ts(dat2[i,c(3:95)])
  x10=as.ts(dat2[i,c(96:105)])
  min_value=fitted_compare(x,AAPE)
  choice=order(min_value)[1]
  
  result_maape=rbind(result_maape,c(min_value,choice,fcst_compare(choice,x,x10)))
}

par(mfrow=c(1,1))
plot(result_mape[,6],type="l",lwd=2)
lines(result_maape[,6],col=2)

plot(result_mape[result_mape[,7]>result_maape[,7],7],type="l",lwd=2)
lines(result_maape[result_mape[,7]>result_maape[,7],7],col=2)




############################## whole data


dat833=read.csv("dat833.csv")

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


result_mape=c()
for(i in 1501:nrow(dat2)){
  print(i)
  x=as.ts(dat2[i,c(3:95)])
  x10=as.ts(dat2[i,c(96:105)])
  min_value=fitted_compare(x,APE)
  choice=order(min_value)[1]
  
  result_mape=rbind(result_mape,c(dat2[i,c(1:2)],min_value,choice,fcst_compare(choice,x,x10),round(sum(x==0)/length(x),2)))
}

result_maape=c()
for(i in 1501:nrow(dat2)){
  print(i)
  x=as.ts(dat2[i,c(3:95)])
  x10=as.ts(dat2[i,c(96:105)])
  min_value=fitted_compare(x,AAPE)
  choice=order(min_value)[1]
  
  result_maape=rbind(result_maape,c(dat2[i,c(1:2)],min_value,choice,fcst_compare(choice,x,x10),round(sum(x==0)/length(x),2)))
}

#write.csv(result_mape, file = "result_mape833.csv", row.names = FALSE)
#write.csv(result_maape, file = "result_maape833.csv", row.names = FALSE)

head(result_mape)

## visualization

par(mfrow=c(1,1))
plot(result_mape[,6],type="l",lwd=2)
lines(result_maape[,6],col=2)

plot(result_mape[,7],type="l",lwd=2)
lines(result_maape[,7],col=2)

sum(result_mape[300:400,6]<result_maape[300:400,6],na.rm=T)
sum(result_mape[300:400,6]>result_maape[300:400,6],na.rm=T)


sum(result_mape[,8]<result_maape[,8],na.rm=T)
sum(result_mape[,8]>result_maape[,8],na.rm=T)
sum(result_mape[,9]<result_maape[,9],na.rm=T)
sum(result_mape[,9]>result_maape[,9],na.rm=T)

sum(result_mape[,8]<result_maape[,8],na.rm=T)
sum(result_mape[,8]>result_maape[,8],na.rm=T)

test=data.frame(cbind(result_mape[,c(1,8,9)],result_maape[,c(8,9)] ))
names(test)=c('Store','mape_mape','mape_maape','maape_mape','maape_maape')
test2=ddply(test,.(Store),ff)
ff=function(dd){
  c(sum(dd[,2]<dd[,4],na.rm=T),
    sum(dd[,2]>dd[,4],na.rm=T),
    sum(dd[,3]<dd[,5],na.rm=T),
    sum(dd[,3]>dd[,5],na.rm=T)
  )
}

## exclude stores
excludestore=test2[test2[,2]>test2[,3],1]
[1]  14  15  28  29  32  46  48  56  63  68  72  73  76  81  88  90  95  96  98 101 105
[22] 107 111 112 113 114 123 128 129 133 139 142 175


stores102=setdiff(all_store, excludestore)

result_mape2=result_mape[result_mape[,1]%in%stores102,]
result_maape2=result_maape[result_maape[,1]%in%stores102,]


sum(result_mape2[,8]<result_maape2[,8],na.rm=T)
sum(result_mape2[,8]>result_maape2[,8],na.rm=T)
sum(result_mape2[,9]<result_maape2[,9],na.rm=T)
sum(result_mape2[,9]>result_maape2[,9],na.rm=T)

test1=data.frame(cbind(result_mape2[,c(1,7,8,9,10)],result_maape2[,c(7,8,9,10)] ))
names(test1)=c('Store','mape_choice','mape_mape','mape_maape','mape_pct'
              ,'maape_choice','maape_mape','maape_maape','maape_pct')


postscript("../figs/comparebyMAPE.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")

sorted_test1=test1[order(test1$mape_mape,decreasing =T),]
plot(sorted_test1$mape_mape,type="b",xlab="",ylab="MAPE",cex=1.5,xlim=c(0,1270))
points(sorted_test1$maape_mape,col=2)
legend(970,5.5, c("MAPE", "MAAPE"), col = 1:2,
       pch = 1, cex = 1.0)
dev.off() 



postscript("../figs/comparebyMAAPE.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")

sorted_test1a=test1[order(test1$mape_maape,decreasing =T),]
plot(sorted_test1a$mape_maape,type="b",xlab="",ylab="MAAPE",cex=1.5)
points(sorted_test1a$maape_maape,col=2)
legend(1100,1.4, c("MAPE", "MAAPE"), col = 1:2,
       pch = 1, cex = 1.0)
dev.off() 





ff2=function(dd){
  c(item_count=nrow(dd),
    zero_pct2=round(mean(dd$maape_pct),2),
    mape_m1=sum(dd$mape_choice==1),
    mape_m2=sum(dd$mape_choice==2),
    mape_m3=sum(dd$mape_choice==3),
    mape_m4=sum(dd$mape_choice==4),
    maape_m1=sum(dd$maape_choice==1),
    maape_m2=sum(dd$maape_choice==2),
    maape_m3=sum(dd$maape_choice==3),
    maape_m4=sum(dd$maape_choice==4),
    mape_mape1=sum(dd[,3]<dd[,7],na.rm=T), #mape_mape is better than maape_mape
    mape_maape1=sum(dd[,3]>dd[,7],na.rm=T),#maape_mape is better than mape_mape
    maape_mape1=sum(dd[,4]<dd[,8],na.rm=T),#mape_maape is better than maape_maape
    maape_maape1=sum(dd[,4]>dd[,8],na.rm=T)#maape_maape is better than mape_maape
  )
}
test2=ddply(test1,.(Store),ff2)
sum(test2$maape_m1)+
sum(test2$maape_m2)+
sum(test2$maape_m3)+
sum(test2$maape_m4)


dat_store2=dat2[dat2[,1]==2,]
par(mfrow=c(1,1))
plot(dat_store2[1,3:107],type="l",lwd=2,xlab="Weeks",ylab="Sales",ylim=c(0,100))
for(k in 2:16){
  lines(dat_store2[k,3:107],lwd=2)
}
write.csv(dat_store2,'dat_store2.csv')


plot(result_mape[result_mape[,8]<result_maape[,8],8],type="l",lwd=2)
lines(result_maape[result_mape[,8]<result_maape[,8],8],col=2)



cbind(result_mape[result_mape[,8]<result_maape[,8],],result_maape[result_mape[,8]<result_maape[,8],])

## Average intermittent interval
dataset=dat2[dat2[,1]%in%stores102,-c(1,2)]
aaa=idclass(t(dataset))
mean(aaa$p,na.rm=T)

##############################################











































































fit.croston=croston(a, h=10, alpha=0.5)


dat2=ddply(dat1,.(Store,Sku,WeekInDayFmt),ff)

x=seq(0.1,3,0.05)
y=sort(x, decreasing = TRUE)
abc=function(a,b){
#atan(abs(a-b)/a)
#(a-b)^2  # MSE
#abs(a-b)/a #MAPE
abs((a-b)/(a+b)*2)
}



aa=APE(a,b)
AAPE(a,b)

dat_aape=matrix(0,nrow(dat2)-1,nrow(dat2))
for(i in 1:nrow(dat_aape)){
  for (j in (i+1):ncol(dat_aape)){
    dat_aape[i,j]=mean(AAPE(dat2[i,-c(1:2)],dat2[j,-c(1:2)]))
  }
}

##
result=c()
for(i in 1:(nrow(dat2)-1)){
  for (j in (i+1):nrow(dat2)){
    result=rbind(result,c(dat2[i,c(1:2)],dat2[j,c(1:2)],mean(AAPE(dat2[i,-c(1:2)],dat2[j,-c(1:2)]))))
  }
}

result2=cbind(result,as.numeric(result[,1]==result[,3]))
result3=cbind(result,as.numeric(result[,2]==result[,4]))
result2=result2[order(result2[,5]),]
result3=result3[order(result3[,5]),]
par(mfrow=c(2,2))
plot(result2[,5],result2[,6])
plot(result3[,5],result3[,6])
plot(result3[,6])
###
result=c()
for(i in 1:(nrow(dat2)-1)){
  for (j in (i+1):nrow(dat2)){
    aa=APE(dat2[i,-c(1:2)],dat2[j,-c(1:2)])
    result=rbind(result,c(dat2[i,c(1:2)],dat2[j,c(1:2)],mean(aa[aa<10000])))
  }
}

result4=cbind(result,as.numeric(result[,1]==result[,3]))
result5=cbind(result,as.numeric(result[,2]==result[,4]))
result4=result4[order(result4[,5]),]
result5=result5[order(result5[,5]),]
plot(result4[,5],result4[,6])
plot(result5[,5],result5[,6])
plot(result5[,6])


##


postscript("AAPE_1.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
z=outer(x, y, FUN=AAPE)
Z=c(z)
X=rep(x,length(y))
Y=rep(y,each=length(x))
akima.li <- interp(X,Y,Z, duplicate="median")
image.plot(akima.li,add=FALSE,xlab="A", ylab="F")
dev.off() 

postscript("AAPE_2.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
#dat=cbind(X,Y,Z)
dat_diag=cbind(x,y,diag(z))
#dat_diag=dat[(dat[,1]+dat[,2]==(max(x)+min(x))),]
plot(dat_diag[,1],dat_diag[,3],xlab="",ylab="AAPE",type="l",axes=FALSE)
axis(2, pretty(round(dat_diag[,3],2),3))
axis(1, at=0:3,labels=c(3:0),line=1)
mtext("F",1,line=1,at=-0.2)

#axis(1, pretty(dat_diag[,1],10),line=3)
axis(1, at=0:3,labels=c(0:3),line=3)
mtext("A",1,line=3,at=-0.2)

dev.off() 

dat_diag2= dat_diag[2*x>y,]
plot(dat_diag2[,1],dat_diag2[,3],xlab="",ylab="APE",type="l",axes=F)
axis(2, pretty(round(dat_diag2[,3],2),10))
axis(1, at=1:10,labels=c(10:1),line=1)
mtext("F",1,line=1,at=0.2)

axis(1, pretty(dat_diag2[,1],10),line=3)
mtext("A",1,line=3,at=0.2)

############ Figure 4 ###############
## APE
z=outer(x, y, FUN=APE)
Z=c(z)
X=rep(x,length(y))
Y=rep(y,each=length(x))
akima.li <- interp(X,Y,Z, duplicate="median")
image.plot(akima.li,add=FALSE,xlab="A", ylab="F")


postscript("APE.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
	dat_diag=cbind(x,y,diag(z))
	plot(dat_diag[,1],dat_diag[,3],xlab="",ylab="APE",type="l",axes=FALSE)
	axis(2, pretty(round(dat_diag[,3],2),3))
	axis(1, at=1:3,labels=c(3:1),line=1)
	mtext("F",1,line=1,at=0.8)
	axis(1, at=0:3,labels=c(0:3),line=3)
	mtext("A",1,line=3,at=0.8)
dev.off() 

## AAPE
z=outer(x, y, FUN=AAPE)
Z=c(z)
X=rep(x,length(y))
Y=rep(y,each=length(x))
akima.li <- interp(X,Y,Z, duplicate="median")
image.plot(akima.li,add=FALSE,xlab="A", ylab="F")


postscript("AAPE.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
	dat_diag=cbind(x,y,diag(z))
	plot(dat_diag[,1],dat_diag[,3],xlab="",ylab="AAPE",type="l",axes=FALSE)
	axis(2, pretty(round(dat_diag[,3],2),10))
	axis(1, at=1:3,labels=c(3:1),line=1)
	mtext("F",1,line=1,at=0.8)
	axis(1, at=1:3,labels=c(1:3),line=3)
	mtext("A",1,line=3,at=0.8)
dev.off() 


############## M3 data ###############



library(Mcomp)
data(M3)
plot(M3$N0647)
library(fma)
#postscript("lubricant.eps",width = 8.0, height = 3.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(c(1:36),productC,ylab="Unit sold",type="b",xlab="Month",lwd=3)
abline(v=24.5,lty=2,lwd=3)
#dev.off() 

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


postscript("lubricant_plot.eps",width = 5.0, height = 5.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(c(25:36),productC[25:36],type="l",xlab="Month", ylab="Sales",lwd=3, ylim=c(-0.5,7.5))
abline(h=F,col=4,lwd=3,lty=2)
lines(c(25:36),c(0.1,-0.1,0.1,7,1.1,0.1,-0.1,0.9,0.1,1.1,-0.1,0.1),col=2,lwd=3,lty=3)
legend(31.5, 7, c("Actual", "Forecast 1", "Forecast 2"), col = c(1,4,2),
lty = c(1,2,3), lwd=c(3,3,3),cex=0.9,merge = T,box.col="white")
dev.off() 

library(xtable)
DD=data.frame(cbind(c(productC),APE(productC,F),AAPE(productC,F)))

xtable(DD)

###################################

x=seq(0,6,0.1)
y=atan(x)


x2=seq(1.5,6,0.1)
y2=pi/2-1/x2

postscript("arctan.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")

plot(x,y,type="l",ylab="y",lwd=2,ylim=c(0,2))
lines(x,x,lwd=2,lty=2)
lines(x2,y2,col=2,lwd=2)

legend(4, 0.5, c("y=arctan(x)", "y=x", "y=pi/2-1/x"), col = c(1, 1, 2),
       box.col  ="white",lwd=c(2,2,2),lty = c(1, 2, 1),  merge = T, bg = "gray90")

dev.off()


