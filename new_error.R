library(fields)
library(akima)
setwd("C:\\Users\\sungil.kim\\Dropbox\\Research_idea\\figs")

x=seq(0.1,3,0.05)
y=sort(x, decreasing = TRUE)
abc=function(a,b){
#atan(abs(a-b)/a)
#(a-b)^2  # MSE
#abs(a-b)/a #MAPE
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


