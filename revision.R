AL<<-1
AH<<-5

################################## Derived Optimal solutions 
P1=exp((log(AH)+log(AL))/2)
P2=(AH-AL)/(log(AH)-log(AL))
P3=2*AH*AL/(AL+AH)
PM=(AL+AH)/2

P1;P2;P3;PM
################################### APE
SE=function(A,P){
		(A-P)^2
}


APE=function(A,P){
		abs(1-P/A)
}

x=seq(AL,AH,0.05)

loss1=function(p,x){
	sum(APE(x,p))
}

loss2=function(p,x){
	max(sum(APE(x[x<p],p)),sum(APE(x[x>=p],p)))
}

loss3=function(p,x){
	max(APE(x,p))
}

y1=c();y2=c();y3=c()
for(i in seq(AL,AH,0.05)){
	y1=c(y1,loss1(i,x))
	y2=c(y2,loss2(i,x))
	y3=c(y3,loss3(i,x))
}

plot(seq(AL,AH,0.05),y1,type="l",lwd=3,ylim=c(0,80))
lines(seq(AL,AH,0.05),y2,col=2,lwd=3)
lines(seq(AL,AH,0.05),y3*15,col=4,lwd=3)


############################ AAPE


#it should be here
findlossoptimal<-function(loss,rg,sep=0.1){
  tmp<-c()
  for(i in seq(rg[1],rg[2],sep)){
    tmp<-c(tmp,mean(loss(i)))
  }
  tb=cbind(seq(rg[1],rg[2],sep),tmp)
  return(list(tb=tb,op=as.numeric(tb[tb[,2]==min(tb[,2]),1])))
}

Aloss1=function(p,x){
	sum(AAPE(x,p))
}

Aloss2=function(p,x){
	max(sum(AAPE(x[x<p],p)),sum(AAPE(x[x>=p],p)))
}

Aloss3=function(p,x){
	max(AAPE(x,p))
}


##추가함
AAPE=function(a,b){
  re=atan(abs(a-b)/a) #MAAPE    
  re[(a==0)&(b==0)]=0
  return(re)
}
##여기까지

Ay1=c();Ay2=c();Ay3=c()
for(i in seq(AL,AH,0.05)){
	Ay1=c(Ay1,Aloss1(i,x))
	Ay2=c(Ay2,Aloss2(i,x))
	Ay3=c(Ay3,Aloss3(i,x))
}

plot(seq(AL,AH,0.05),Ay1,type="l",lwd=3,ylim=c(0,80))
lines(seq(AL,AH,0.05),Ay2,col=2,lwd=3)
lines(seq(AL,AH,0.05),Ay3*15,col=4,lwd=3)
####################### find solution using optim
SEloss1=function(p){
  x=seq(AL,AH,0.01)
  sum(SE(x,p))
}

AAloss1=function(p){
	x=seq(AL,AH,0.01)
	sum(AAPE(x,p))
}

AAloss2=function(p){
	x=seq(AL,AH,0.01)
  if(p==0) return(sum(AAPE(x[x>=p],p)))
  if(p>0) return(max(sum(AAPE(x[x<p],p)),sum(AAPE(x[x>=p],p))))
	
}

AAloss3=function(p){
	x=seq(AL,AH,0.01)
	max(AAPE(x,p))
}

Aloss1=function(p){
  x=seq(AL,AH,0.01)
  sum(APE(x,p,0.01))
}

Aloss2=function(p){
  x=seq(AL,AH,0.01)
  if(p==0) return(sum(APE(x[x>=p],p,0.01)))
  if(p>0) return(max(sum(APE(x[x<p],p,0.01)),sum(APE(x[x>=p],p,0.01))))
  
}

####################### Figure 1 for AAPE
AL=0
AH=5

P1=exp((log(AH)+log(AL))/2)
P2=(AH-AL)/(log(AH)-log(AL))
P3=2*AH*AL/(AL+AH)
PM=(AL+AH)/2



optimize(SEloss1,c(AL,AH))$minimum
P1A=optimize(AAloss1,c(AL,AH))$minimum
P2A=optimize(AAloss2,c(AL,AH))$minimum
P3A=optimize(AAloss3,c(AL,AH))$minimum

AL=0
P1A=findlossoptimal(AAloss1,c(0,5),0.01)$op
P2A=findlossoptimal(AAloss2,c(0,5),0.01)$op
P3A=findlossoptimal(AAloss3,c(0,5),0.01)$op



##Error occurs here 
##Error in APE(x, p, 0.01) : unused argument (0.01) 
##4.
##APE(x, p, 0.01) 
##3.
##loss(i) 
##2.
##mean(loss(i)) 
##1.
##findlossoptimal(Aloss1, c(0, 5), 0.01)
##result -> p1=p2 = 0
##however below, p1 = 0.1, and p2 = 0.64

P1=findlossoptimal(Aloss1,c(0,5),0.01)$op
P2=findlossoptimal(Aloss2,c(0,5),0.01)$op


postscript("F1_1.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(AL,AH,0.05),APE(seq(AL,AH,0.05),PM),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
lines(seq(AL,AH,0.05),AAPE(seq(AL,AH,0.05),PM),col=2,lwd=3,lty=2)
abline(v=PM,col=1,lty=3,lwd=3);abline(v=PM,col=2,lty=3,lwd=3)
dev.off() 
postscript("F1_2_20150711.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(AL,AH,0.01),APE(seq(AL,AH,0.01),P1),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
lines(seq(AL,AH,0.01),AAPE(seq(AL,AH,0.01),P1A),col=2,lwd=3,lty=2)
#abline(v=P1,col=1,lty=3,lwd=3);abline(v=P1A,col=2,lty=3,lwd=3)
abline(v=2.5,col=1,lty=3,lwd=3);
#P1 0.1
#P1A 2.06
dev.off() 
postscript("F1_3_20150711.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(AL,AH,0.01),APE(seq(AL,AH,0.01),P2),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
lines(seq(AL,AH,0.05),AAPE(seq(AL,AH,0.05),P2A),col=2,lwd=3,lty=2)
#abline(v=P2,col=1,lty=3,lwd=3);abline(v=P2A,col=2,lty=3,lwd=3)
abline(v=2.5,col=1,lty=3,lwd=3);
#P2 0.64
#P2A 1.71
dev.off() 
postscript("F1_4.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(AL,AH,0.05),APE(seq(AL,AH,0.05),P3),type="l",lwd=3,xlab="A",ylab="")
lines(seq(AL,AH,0.05),AAPE(seq(AL,AH,0.05),P3A),col=2,lwd=3,lty=2)
abline(v=P3,col=4,lty=3,lwd=2);abline(v=P3A,col=4,lty=3,lwd=2)
dev.off() 

### uniform discrete
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

SE1UD=function(p){
  rng=c(0:5)
  fx<-rep(1/6,6)
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

AD=function(A,P){
  abs(A-P)
}
AD1UD<-function(p){
  rng=c(0:5)
  fx<-rep(1/6,6)
  sum(AD(rng,p)*(fx))
}
AD1UD=function(p){
  rng=c(0:5)
  fx<-rep(1/6,6)
  sum(AD(rng,p)*(fx))
}


SEUD<- findlossoptimal(SE1UD,c(0,5),0.01)$op
ADUD<- findlossoptimal(AD1UD,c(0,5),0.01)$op
AUD1=findlossoptimal(Aloss1UD,c(0,5),0.01)$op 
AUD2=findlossoptimal(Aloss2UD,c(0,5),0.01)$op 
AAUD1=findlossoptimal(AAloss1UD,c(0,5),0.01)$op 
AAUD2=max(findlossoptimal(AAloss2UD,c(0,5),0.01)$op )


AH<-5
postscript("UD1_20150711.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),AUD1),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
  lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AAUD1),col=2,lwd=3,lty=2)
  #abline(v=ANB1,col=1,lty=3,lwd=3);abline(v=AANB1,col=2,lty=3,lwd=3)
  #abline(v=ADUD,col=1,lty=3,lwd=2)
  abline(v=2.5,col=1,lty=3,lwd=3);
#AUD1 0.01
#AAUD1 2
  #rect(2,-1,3,5,col=rgb(0.8,0.8,0.8,0.5),border=0)
dev.off() 

postscript("UD2_20150711.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),AUD2),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
  lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AAUD2),col=2,lwd=3,lty=2)
  #abline(v=ANB2,col=1,lty=3,lwd=3);abline(v=AANB2,col=2,lty=3,lwd=3)
  #abline(v=ADUD,col=1,lty=3,lwd=2)
#AUD2 0.05
#AAUD2 1.42
  abline(v=2.5,col=1,lty=3,lwd=3);
  #rect(2,-1,3,5,col=rgb(0.8,0.8,0.8,0.5),border=0)
dev.off() 


##################### f(x) is negative binomial case #################
APE=function(A,P,rp=0.01){
  A[A==0]=rp
  abs(1-P/A)
}

AAPE=function(A,P){
  tmp<-cbind(A,P)
  tmp[rowSums(tmp)==0,]=0.1
  out=atan(abs(1-tmp[,2]/tmp[,1]))
  #out[P+A==0]=atan(0)
  return(out)
}

#AAPE=function(A,P){
#  out=atan(abs(1-P/A))
#  out[P+A==0]=atan(0)
#  return(out) 
#}


Aloss1NB=function(p){
  rng=c(0:(50*m))
  fx<-dnbinom(rng,r,mu=m)
  sum(APE(rng,p,rp=0.01)*(fx))
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



p^r=0.15
r*log(p)=log(0.15)
r=log(0.15)/log(p)

out=c()
for(pp in seq(0.001,0.999,0.001)){
  out<-c(out,abs(log(0.15)/log(pp)*(1-pp)/pp-2.5))
}
p=seq(0.001,0.999,0.001)[out==min(out)]

r<<-log(0.15)/log(p)
m<<-r*(1-p)/p

tmp<-findlossoptimal(AAloss1NB,c(0,5))
plot(tmp$tb[,1],tmp$tb[,2])



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


SENB<- findlossoptimal(SE1NB,c(0,5),0.01)$op
ADNB<- findlossoptimal(AD1NB,c(0,5),0.01)$op
ANB1=findlossoptimal(Aloss1NB,c(0,5),0.01)$op 
ANB2=findlossoptimal(Aloss2NB,c(0,5),0.01)$op 
AANB1=findlossoptimal(AAloss1NB,c(0,5),0.01)$op 
AANB2=max(findlossoptimal(AAloss2NB,c(0,5),0.01)$op )


AH<-5
  postscript("NB1_20150711.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),ANB1),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
  lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AANB1),col=2,lwd=3,lty=2)
  #abline(v=ANB1,col=1,lty=3,lwd=3);abline(v=AANB1,col=2,lty=3,lwd=3)
  abline(v=ADNB,col=1,lty=3,lwd=2)
  abline(v=2.5,col=2,lty=1,lwd=1)
  dev.off() 

  postscript("NB2_20150711.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),ANB2),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
  lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AANB2),col=2,lwd=3,lty=2)
  #abline(v=ANB2,col=1,lty=3,lwd=3);abline(v=AANB2,col=2,lty=3,lwd=3)
  abline(v=ADNB,col=1,lty=3,lwd=2)
  abline(v=2.5,col=2,lty=1,lwd=1)
  dev.off() 
  
  

SENB<-c()
ANB1<-c()
ADNB<-c()
AANB1<-c()
for(j in seq(1,5,0.01)){
  m<<-j
  SENB<-c(SENB, findlossoptimal(SE1NB,c(0,5),0.01)$op)
  ADNB<-c(ADNB, findlossoptimal(AD1NB,c(0,5),0.01)$op)
  ANB1<-c(ANB1,findlossoptimal(Aloss1NB,c(0,5),0.01)$op )
  AANB1<-c(AANB1,findlossoptimal(AAloss1NB,c(0,5),0.01)$op) 
}

postscript("NB_m.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  plot(seq(1,5,0.01),SENB,ylim=c(0,5),type="l",lwd=3,xlab="m",ylab="Optimal point",cex.axis=1.5,cex.lab=1.5)
  lines(seq(1,5,0.01),ANB1,lwd=3,lty=3)
  lines(seq(1,5,0.01),AANB1,col=2,lwd=3)
  lines(seq(1,5,0.01),ADNB,col=4,lwd=3,lty=2)
dev.off() 

################### geometric poisson
exp(-1*lambda)=0.25
lambda=log(0.15)*(-1)
m<<-2.5
thm<<-lambda/m

tmp<-findlossoptimal(AAloss1GP,c(0,5))
plot(tmp$tb[,1],tmp$tb[,2])

#why is it here?
findlossoptimal<-function(loss,rg,sep=0.1){
  tmp<-c()
  for(i in seq(rg[1],rg[2],sep)){
    tmp<-c(tmp,mean(loss(i)))
  }
  tb=cbind(seq(rg[1],rg[2],sep),tmp)
  return(list(tb=tb,op=as.numeric(tb[tb[,2]==min(tb[,2]),1])))
}



SE1GP=function(p){
  rng=c(0:(50*m))
  lambda=thm*m
  fx<-as.numeric(lapply(rng,GP,lambda))
  sum(SE(rng,p)*(fx))
  
}
AD1GP=function(p){
  rng=c(0:(50*m))
  lambda=thm*m
  fx<-as.numeric(lapply(rng,GP,lambda))
  sum(AD(rng,p)*(fx))
  
}


Aloss1GP=function(p){
  rng=c(0:(50*m))
  lambda=thm*m
  fx<-as.numeric(lapply(rng,GP,lambda))
  sum(APE(rng,p,rp=0.01)*(fx))
  
}


Aloss2GP=function(p){
  rng=c(0:(50*m))
  lambda=thm*m
  fx<-as.numeric(lapply(rng,GP,lambda))
  max(sum(APE(rng[rng<p],p,rp=0.01)*fx[rng<p]),sum(APE(rng[rng>=p],p,rp=0.01)*fx[rng>=p]))
}

AAloss1GP=function(p){
  
  rng=c(0:(50*m))
  lambda=thm*m
  fx<-as.numeric(lapply(rng,GP,lambda))
  
  sum(AAPE(rng,p)*(fx))
  
}

AAloss2GP=function(p){
  
  rng=c(0:(50*m))
  lambda=thm*m
  fx<-as.numeric(lapply(rng,GP,lambda))
  if(p==0)return(sum(AAPE(rng[rng>=p],p)*fx[rng>=p]))
  if(p>0) return(max(sum(AAPE(rng[rng<p],p)*fx[rng<p]),sum(AAPE(rng[rng>=p],p)*fx[rng>=p])))
  
}



GP<-function(n,lambda){
  theta=thm
  if(n==0) out=exp(-1*lambda)
  if(n>0){
    out<-0
    for(k in 1:n){
      out=out+exp(-1*lambda)*lambda^k/factorial(k)*(1-theta)^(n-k)*theta^k*factorial(n-1)/factorial(k-1)/factorial(n-k)
    }
  } 
  return(out)
}

tmp<-findlossoptimal(AAloss1GP,c(0,5),sep=0.1)
plot(tmp$tb[,1],tmp$tb[,2])



SEGP<- findlossoptimal(SE1GP,c(0,5),0.01)$op
ADGP<- findlossoptimal(AD1GP,c(0,5),0.01)$op
AGP1=findlossoptimal(Aloss1GP,c(0,5),0.01)$op 
AGP2=findlossoptimal(Aloss2GP,c(0,5),0.01)$op 
AAGP1=findlossoptimal(AAloss1GP,c(0,5),0.01)$op 
AAGP2=max(findlossoptimal(AAloss2GP,c(0,5),0.01)$op )



postscript("GP1_20150711.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),AGP1),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
  lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AAGP1),col=2,lwd=3,lty=2)
  #abline(v=AGP1,col=1,lty=3,lwd=3);abline(v=AAGP1,col=2,lty=3,lwd=3)
  abline(v=ADNB,col=1,lty=3,lwd=3)
dev.off() 
postscript("GP2_20150711.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  plot(seq(0.00001,AH,0.01),APE(seq(0.00001,AH,0.01),AGP2),ylim=c(0,2),type="l",lwd=3,xlab="A",ylab="",cex.axis=1.5,cex.lab=1.5)
  lines(seq(0.00001,AH,0.01),AAPE(seq(0.00001,AH,0.01),AAGP2),col=2,lwd=3,lty=2)
  #abline(v=AGP2,col=1,lty=3,lwd=3);abline(v=AAGP2,col=2,lty=3,lwd=3)
  abline(v=ADNB,col=1,lty=3,lwd=3)
dev.off() 



SEGP<-c()
AGP1<-c()
ADGP<-c()
AAGP1<-c()
for(j in seq(1,5,0.01)){
  m<<-j
  SEGP<-c(SENB, findlossoptimal(SE1GP,c(0,5),0.1)$op)
  ADGP<-c(ADNB, findlossoptimal(AD1GP,c(0,5),0.1)$op)
  AGP1<-c(ANB1,findlossoptimal(Aloss1GP,c(0,5),0.1)$op )
  AAGP1<-c(AANB1,findlossoptimal(AAloss1GP,c(0,5),0.1)$op) 
}

postscript("GP_m.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(1,5,0.01),SENB,ylim=c(0,5),type="l",lwd=3,xlab="m",ylab="Optimal point",cex.axis=1.5,cex.lab=1.5)
lines(seq(1,5,0.01),ADNB,col=4,lwd=3,lty=2)
lines(seq(1,5,0.01),AANB1,col=2,lwd=3,lty=4)
lines(seq(1,5,0.01),ANB1,lwd=3,lty=3)
dev.off() 


############## lubricant ###############
SAPE=function(a,b){
  abs((a-b)/(a+b)*2)
}

MASE=function(a,b,denom){
  (abs(a-b))/denom
}

library(Mcomp)
data(M3)
plot(M3$N0647)
library(fma)
#postscript("lubricant.eps",width = 8.0, height = 3.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(c(1:36),productC,ylab="Unit sold",type="b",xlab="Month",lwd=3)
abline(v=24.5,lty=2,lwd=3)
#dev.off() 

y<-productC[1:24]
y_1<-y[1:23]
sum(abs(y[2:24]-y_1))/23

F=mean(productC[1:24])
F2=c(0.1,-0.1,0.1,7,1.1,0.1,-0.1,0.9,0.1,1.1,-0.1,0.1)


mean(MASE(productC[c(28,29,32,34)],F,mean(abs(y[2:24]-y_1)))) #0.26
mean(MASE(productC[c(28,29,32,34)],c(7,1.1,0.9,1.1),mean(abs(y[2:24]-y_1)))) #0.43
mase1=MASE(productC[25:36],F,mean(abs(productC[2:24]-productC[1:23])))
mase2=MASE(productC[25:36],F2,mean(abs(y[2:24]-y_1)))
mean(mase1) #0.44
mean(mase2) #0.17



mase<-c()
for(i in 2:24){
actual=productC[i]
fcst=mean(productC[c(1:(i-1))])
tmp=abs(productC[c(2:(25-1))]-productC[c(1:(25-2))])
denom=mean(tmp)
ase=abs(actual-fcst)/denom
mase<-c(mase,ase)
}


temp<-abs(productC[2:24]-c(cumsum(productC[1:23])/c(1:23)))/(cumsum(abs(y[2:24]-y_1))/c(1:23))





#excluding
mean(APE(productC[c(28,29,32,34)],F)) #0.39
mean(APE(productC[c(28,29,32,34)],c(7,1.1,0.9,1.1)))#0.41
#including
mean(APE(productC[25:36],F))
mean(APE(productC[25:36],F2))


#excluding
(mean(SAPE(productC[c(28,29,32,34)],F))) #0.41
(mean(SAPE(productC[c(28,29,32,34)],c(7,1.1,0.9,1.1)))) #0.27
#including
mean(SAPE(productC[25:36],F))  #1.47
mean(SAPE(productC[25:36],F2))


#excluding
(mean(AAPE(productC[c(28,29,32,34)],F))) #0.37
(mean(AAPE(productC[c(28,29,32,34)],c(7,1.1,0.9,1.1)))) #0.31
#including
aape1<-mean(AAPE(productC[25:36],F))
aape2<-mean(AAPE(productC[25:36],F2))
(aape1) #1.17
(aape2) #1.15


actl<-productC[1:24]
tmp=abs(actl[c(2:length(actl))]-actl[c(1:(length(actl)-1))])
denom=mean(tmp)

ASE=function(y,b,denom=NULL){
  y_1<-y[1:(length(y)-1)]
  if(is.null(denom)) denom=mean(abs(y[2:length(y)]-y_1))
  (abs(y-b))/denom
}


#MASE
#excluding
(mean(ASE(productC[c(28,29,32,34)],F,denom))) #0.26
(mean(ASE(productC[c(28,29,32,34)],c(7,1.1,0.9,1.1),denom))) #0.43
#including
mean(ASE(productC[25:36],F,denom))  #0.44
mean(ASE(productC[25:36],F2,denom)) #0.17

#MAE/mean
#excluding
(mean(ASE(productC[c(28,29,32,34)],F,mean(actl)))) #0.5
(mean(ASE(productC[c(28,29,32,34)],c(7,1.1,0.9,1.1),mean(actl)))) #0.81
#including
mean(ASE(productC[25:36],F,mean(actl)))  #0.83
mean(ASE(productC[25:36],F2,mean(actl))) #0.32



postscript("lubricant_plot.eps",width = 8.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(c(25:36),productC[25:36],type="l",xlab="Month", ylab="Sales",lwd=2,cex.lab=1.4, ylim=c(-0.5,7.5))
points(c(25:36),productC[25:36],pch=1,lwd=2)
abline(h=F,col=4,lwd=2,lty=2)
points(c(25:36),rep(F,12),col=4,pch=4,lwd=2)
lines(c(25:36),c(0.1,-0.1,0.1,7,1.1,0.1,-0.1,0.9,0.1,1.1,-0.1,0.1),col=2,lwd=3,lty=3)
points(c(25:36),c(0.1,-0.1,0.1,7,1.1,0.1,-0.1,0.9,0.1,1.1,-0.1,0.1),col=2,pch=3,lwd=2)
legend(33.5, 7, c("Actual", "Forecast 1", "Forecast 2"), col = c(1,4,2),
       pch = c(1,4,3), lwd=c(2,2,2), lty=c(1,2,3),cex=1,merge = F)
dev.off() 

library(xtable)
DD=data.frame(cbind(c(productC),APE(productC,F),AAPE(productC,F)))

xtable(DD)



####### simulation



APE=function(A,P,rp=0.1){
  A[A==0]=rp
  abs(1-P/A)
}


AAPE=function(A,P){

  tmp<-cbind(A,P)
  tmp[rowSums(tmp)==0,]=0.1
  out=atan(abs(1-tmp[,2]/tmp[,1]))
  #out[P+A==0]=atan(0)
  return(out)
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

#pp<-c()
#for(vv in seq(0.01,0.1,0.001)){
#  pp<-rbind(pp,sim(vv))
#}

pp<-c()
for(vv in seq(1:1000)){
  pp<-rbind(pp,sim(0.01))
}
simple<-data.frame(1-pp[,1:2]/100)
names(simple)<-c("MAPE","MAAPE")

postscript("error_rate3.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  boxplot(as.data.frame(simple),xlab="",ylab="Error rate",cex.lab=1.5)
dev.off() 

postscript("error_rate.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(1:10,1-pp[,1]/100,type="p",ylim=c(0,0.6),ylab="Error rate",xlab=expression(delta),cex.lab=1.5,cex=1.5)
points(1:10,1-pp[,2]/100,col=2,pch=19,cex=1.5)
points(1:10,1-pp[,3]/100,col=3,pch=19,cex=1.5)
legend(0.08,0.2, c("MAPE", "MAAPE","sMAPE"), col = c(1,2,3),pch = c(1,19,19), cex = 1)
dev.off() 




postscript("error_rate.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(seq(0.01,0.1,0.001),1-pp[,1]/100,type="p",ylim=c(0,0.6),ylab="Error rate",xlab=expression(delta),cex.lab=1.5,cex=1.5)
points(seq(0.01,0.1,0.001),1-pp[,2]/100,col=2,pch=19,cex=1.5)
points(seq(0.01,0.1,0.001),1-pp[,3]/100,col=3,pch=19,cex=1.5)
legend(0.08,0.2, c("MAPE", "MAAPE","sMAPE"), col = c(1,2,3),pch = c(1,19,19), cex = 1)
dev.off() 


sum(pp[,1]<=pp[,2])/nrow(pp)


###### zero forecast #######
aa<<-c(1,10,1,5,1)
ff1<-c(1,100,1,5,1)
ff2<-c(1,100000,1,5,1)


t1=AAPE(aa,ff1)
t2=AAPE(aa,ff2)
t1
t2
(mean(t1))  #0.29
(mean(t2))  #0.31



abs(aa-ff)
APE(aa,ff)

mean(abs(aa-0))

mean(AAPE(aa,500))
mean(AAPE(aa,1))
mean(AAPE(aa,3))


###############
aa<-c(1,1,5,5,1)
median(aa)
mean(APE(aa,5))
mean(AAPE(aa,0))

x=3
sgn<-as.numeric(x>aa)-as.numeric(x<aa)
t(aa/(2*aa*x-x^2))%*%(-1*sgn)

AAD<-function(A,P){
  atan(abs(A-P))
}


exp((log(min(aa))+log(max(aa)))/2)

aa<-rnbinom(9,2,mu=1)
temp<-c();temp2<-c()
for(i in seq(0,5,0.1)){
  temp<-c(temp,mean(AD(aa,i)))
  temp2<-c(temp2,mean(AAD(aa,i)))
}
seq(0,5,0.1)[temp==min(temp)]
seq(0,5,0.1)[temp2==min(temp2)]

plot(seq(0,5,0.1),temp,ylim=c(0,4))
lines(seq(0,5,0.1),temp2)



#### new real example #####

library(plyr)
library(forecast)
setwd("/Users/sungilkim/Dropbox/Sungil_MAAPE_(submitted)/code")
#setwd("C:/Users/Sungil/Documents/My Dropbox/Sungil_MAAPE_(submitted)/code")

##functions ##
APEforReal=function(a,b){
  re=abs(a-b)/a #MAPE    
  re[(a==0)&(b==0)]=0
  return(re)
}

AAPEforReal=function(a,b){
  re=atan(abs(a-b)/a) #MAAPE    
  re[(a==0)&(b==0)]=0
  return(re)
}



ASE=function(y,b,denom=NULL){
  y_1<-y[1:(length(y)-1)]
  if(is.null(denom)) denom=mean(abs(y[2:length(y)]-y_1))
  (abs(y-b))/denom
}

SAPE=function(a,b){
  re=abs((a-b)/(a+b)*2)
  re[(a==0)&(b==0)]=0
  return(re)
}


ASEinSample<-function(actl,fct){
  mase<-c()
  tmp=abs(actl[c(2:length(actl))]-actl[c(1:(length(actl)-1))])
  denom=mean(tmp)
  for(i in 2:length(actl)){
    actual=actl[i]
    fcst=fct[i]
    ase=abs(actual-fcst)/denom
    mase<-c(mase,ase)
  }
  mase[(actl==0)&(fct==0)]=0
  return(mase)
}

ASE2inSample<-function(actl,fct){
  mase<-c()

  denom=mean(actl)
  for(i in 2:length(actl)){
    actual=actl[i]
    fcst=fct[i]
    ase=abs(actual-fcst)/denom
    mase<-c(mase,ase)
  }
  mase[(actl==0)&(fct==0)]=0
  return(mase)
}


inoutsample_compare=function(dt,MEASURE){
  dt<-dt[-c(1:2)]
  a=dt[1:95]
  x10=dt[96:105]
  
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
  
  
  x=a
  
    fit.ets=ets(x)
    fcst1=forecast(fit.ets,h=10)$mean

    fit.holt <- HoltWinters(x,gamma=FALSE,beta=FALSE)
    fcst2=forecast(fit.holt,h=10)$mean

    fit.arima <- forecast(auto.arima(x),h=10)
    fcst3=forecast(fit.arima,h=10)$mean

    fit.croston=croston(x, h=10, alpha=0.5)
    fcst4=fit.croston[1]$mean

  
  ff1=MEASURE(as.numeric(x10),as.numeric(fcst1))
  ff2=MEASURE(as.numeric(x10),as.numeric(fcst2))
  ff3=MEASURE(as.numeric(x10),as.numeric(fcst3))
  ff4=MEASURE(as.numeric(x10),as.numeric(fcst4))

  
  return(c(round(sum(x==0)+sum(x10==0)/(length(x)+length(x10)),2),mean(aa1),mean(aa2),mean(aa3),mean(aa4),mean(ff1),mean(ff2),mean(ff3),mean(ff4)))
}

inoutsample_compare_mase=function(dt){
  a=dt[3:95]
  x10=dt[96:105]
  
  fit.ets=ets(a)
  fit.holt <- HoltWinters(a,gamma=FALSE,beta=FALSE) ## Exponential Smoothing
  fit.arima <- forecast(auto.arima(a),h=10)
  fit.croston=croston(a, h=10, alpha=0.5)
  
  fitted.ets=fit.ets$fitted
  fitted.holt=fit.holt$fitted[,1]
  fitted.arima=fit.arima$fitted
  fitted.croston=fit.croston$fitted
  
  aa1=ASEinSample(a,fitted.ets)
  aa2=ASEinSample(a[-c(1)],fitted.holt)
  aa3=ASEinSample(a,fitted.arima)
  aa4=ASEinSample(a,fitted.croston)
  
  aa1=aa1[!is.na(aa1)];aa2=aa2[!is.na(aa2)];aa3=aa3[!is.na(aa3)];aa4=aa4[!is.na(aa4)]
  
  
  x=a
  
  fit.ets=ets(x)
  fcst1=forecast(fit.ets,h=10)$mean
  
  fit.holt <- HoltWinters(x,gamma=FALSE,beta=FALSE)
  fcst2=forecast(fit.holt,h=10)$mean
  
  fit.arima <- forecast(auto.arima(x),h=10)
  fcst3=forecast(fit.arima,h=10)$mean
  
  fit.croston=croston(x, h=10, alpha=0.5)
  fcst4=fit.croston[1]$mean
  
  tmp=abs(x[c(2:length(x))]-x[c(1:(length(x)-1))])
  denom=mean(tmp)
  
  ff1=ASE(as.numeric(x10),as.numeric(fcst1),denom)
  ff2=ASE(as.numeric(x10),as.numeric(fcst2),denom)
  ff3=ASE(as.numeric(x10),as.numeric(fcst3),denom)
  ff4=ASE(as.numeric(x10),as.numeric(fcst4),denom)
  
  
  return(c(round(sum(x==0)/length(x),2),mean(aa1),mean(aa2),mean(aa3),mean(aa4),mean(ff1),mean(ff2),mean(ff3),mean(ff4)))
}

inoutsample_compare_mase2=function(dt){
  a=dt[3:95]
  x10=dt[96:105]
  
  fit.ets=ets(a)
  fit.holt <- HoltWinters(a,gamma=FALSE,beta=FALSE) ## Exponential Smoothing
  fit.arima <- forecast(auto.arima(a),h=10)
  fit.croston=croston(a, h=10, alpha=0.5)
  
  fitted.ets=fit.ets$fitted
  fitted.holt=fit.holt$fitted[,1]
  fitted.arima=fit.arima$fitted
  fitted.croston=fit.croston$fitted
  
  aa1=ASE2inSample(a,fitted.ets)
  aa2=ASE2inSample(a[-c(1)],fitted.holt)
  aa3=ASE2inSample(a,fitted.arima)
  aa4=ASE2inSample(a,fitted.croston)
  
  aa1=aa1[!is.na(aa1)];aa2=aa2[!is.na(aa2)];aa3=aa3[!is.na(aa3)];aa4=aa4[!is.na(aa4)]
  
  
  x=a
  
  fit.ets=ets(x)
  fcst1=forecast(fit.ets,h=10)$mean
  
  fit.holt <- HoltWinters(x,gamma=FALSE,beta=FALSE)
  fcst2=forecast(fit.holt,h=10)$mean
  
  fit.arima <- forecast(auto.arima(x),h=10)
  fcst3=forecast(fit.arima,h=10)$mean
  
  fit.croston=croston(x, h=10, alpha=0.5)
  fcst4=fit.croston[1]$mean
  

  denom=mean(x)
  
  ff1=ASE(as.numeric(x10),as.numeric(fcst1),denom)
  ff2=ASE(as.numeric(x10),as.numeric(fcst2),denom)
  ff3=ASE(as.numeric(x10),as.numeric(fcst3),denom)
  ff4=ASE(as.numeric(x10),as.numeric(fcst4),denom)
  
  
  return(c(round(sum(x==0)/length(x),2),mean(aa1),mean(aa2),mean(aa3),mean(aa4),mean(ff1),mean(ff2),mean(ff3),mean(ff4)))
}


##################################

dat=read.csv("../realdata/All833.csv")
sku833=unique(dat$Sku)
weekdays=sort(unique(dat$WeekInDayFmt))


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
for(i in c(2,4,8,6)){
  print(i)
  dt=dat2[dat2[,1]=="1",][i,]
  result1<-rbind(result1,inoutsample_compare(dt,APEforReal))
  result2<-rbind(result2,inoutsample_compare(dt,AAPEforReal))
  result3<-rbind(result3,inoutsample_compare(dt,SAPE))
  result4<-rbind(result4,inoutsample_compare_mase(dt))
  result5<-rbind(result5,inoutsample_compare_mase2(dt))
}

#rows<-c(6,9,18,23,39,29,56,4,11,1)
#demand<-dat2[rows,-c(1:2)]
#rows<-c(1:12)[result1[,1]<1]
rows<-c(2,4,8,6)
demand<-dat2[rows,-c(1:2)]

library(xtable)

tb.stack<-c()
for (k in 1:4){
  tb<-data.frame(rbind(
    (result1[k,]),
    (result2[k,]),
    (result3[k,]),
    (result4[k,]),
    (result5[k,])
  ))
  names(tb)<-c("Measures","Insample:ets","Insample:holt","Insample:arima","Insample:croston",
               "Outsample:ets","Outsample:holt","Outsample:arima","Outsample:croston")
  tb[,1]<-c("MAPE","MAAPE","sMAPE","MASE","MAD/mean")
  
  tb.stack<-rbind(tb.stack,tb)
}

cbind(tb.stack[,1],round(tb.stack[,c(2,6,3,7,4,8,5,9)],2))

xtable(tb.stack[,c(1,2,6,3,7,4,8,5,9)])


postscript("patterns10.eps",width = 12.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[1,],type="l",xlab="Weeks",ylab="Sales",cex.lab=1.4,ylim=c(0,110),lwd=2,col=1)
points(demand[1,],pch=1,col=1)
for(j in 2:4){
  lines(demand[j,],type="l",lwd=2,col=j)
  points(demand[j,],pch=j,col=j)
}
legend(95,90, c("SKU A","SKU B","SKU C","SKU D"), col = 1:4, pch = c(1,2,3,4))
dev.off() 

postscript("skuD.eps",width = 12.0, height = 4.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[1,],type="l",xlab="Weeks",ylab="Sales of SKU D",cex.lab=1.4,lwd=2,col=1)
points(demand[1,],pch=1,col=1)
dev.off() 
postscript("skuA.eps",width = 12.0, height = 4.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[2,],type="l",xlab="Weeks",ylab="Sales of SKU A",cex.lab=1.4,lwd=2,col=1)
points(demand[2,],pch=1,col=1)
dev.off() 
postscript("skuB.eps",width = 12.0, height = 4.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[3,],type="l",xlab="Weeks",ylab="Sales of SKU B",cex.lab=1.4,lwd=2,col=1)
points(demand[3,],pch=1,col=1)
dev.off() 
postscript("skuC.eps",width = 12.0, height = 4.0, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(demand[4,],type="l",xlab="Weeks",ylab="Sales of SKU C",cex.lab=1.4,lwd=2,col=1)
points(demand[4,],pch=1,col=1)
dev.off() 


result_maape=c()
for(i in 1:nrow(dat2)){
  print(i)
  x=as.ts(dat2[i,c(3:95)])
  x10=as.ts(dat2[i,c(96:105)])
  min_value=fitted_compare(x,AAPEforReal)
  choice=order(min_value)[1]
  
  result_maape=rbind(result_maape,c(dat2[i,c(1:2)],min_value,choice,fcst_compare(choice,x,x10)))#,round(sum(x==0)/length(x),2)))
}




result_mase=c()
for(i in 1:nrow(dat2)){
  print(i)
  x=as.ts(dat2[i,c(3:95)])
  x10=as.ts(dat2[i,c(96:105)])
  min_value=fitted_compare(x,ASEinSample) #1: ets 2: holts 3: arima 4:croston
  choice=order(min_value)[1]
  
  result_mase=rbind(result_mase,c(dat2[i,c(1:2)],min_value,choice,fcst_compare(choice,x,x10)))#,round(sum(x==0)/length(x),2)))
}


#write.csv(result_mase, file = "result_mase_20150713.csv", row.names = FALSE)
#write.csv(result_mape, file = "result_mape_20150713.csv", row.names = FALSE)
#write.csv(result_maape, file = "result_maape_20150713.csv", row.names = FALSE)

result_mase=read.csv("result_mase_20150713.csv")
result_mape=read.csv("result_mape_20150713.csv")
result_maape=read.csv("result_maape_20150713.csv")

head(result_mape)

## visualization
ff=function(dd){
  c(sum(dd[,2]<dd[,4] & dd[,2]<dd[,6],na.rm=T),  #mape로 최종 평가했을 때, mape로 모델 선택이 좋았던 경우의 수  
    sum(dd[,2]>dd[,4] & dd[,6]>dd[,4] ,na.rm=T), #mape로 최종 평가했을 때, maape로 모델 선택이 좋았던 경우의 수  
    sum(dd[,3]<dd[,5] & dd[,3]<dd[,7],na.rm=T), #maape로 최종 평가했을 때, mape로 모델 선택이 좋았던 경우의 수  
    sum(dd[,3]>dd[,5] & dd[,7]>dd[,5],na.rm=T), #maape로 최종 평가했을 때, maape로 모델 선택이 좋았던 경우의 수  
    sum(dd[,6]<dd[,2] & dd[,6]<dd[,4],na.rm=T), #mape로 최종 평가했을 때, mase로 모델 선택이 좋았던 경우의 수  
    sum(dd[,7]<dd[,3] & dd[,7]<dd[,5],na.rm=T) #maape로 최종 평가했을 때, mase로 모델 선택이 좋았던 경우의 수  
  )
}

test=data.frame(cbind(result_mape[,c(1,8,9)],result_maape[,c(8,9)],result_mase[,c(8,9)] ))
names(test)=c('Store','mape_mape','mape_maape','maape_mape','maape_maape','mase_mape','mase_maape')
test2=ddply(test,.(Store),ff)


## exclude stores why??
## same 
excludestore=test2[test2[,2]>test2[,3],1]
[1]  14  15  28  29  32  46  48  56  63  68  72  73  76  81  88  90  95  96  98 101 105
[22] 107 111 112 113 114 123 128 129 133 139 142 175


stores102=setdiff(all_store, excludestore)
> stores102
[1]   1   2   3   4   5   7   8   9  10  11  12  16  17  19  21  22  23  24  26  27  30  31  33  35  36
[26]  38  39  40  41  42  44  45  47  50  52  54  59  61  62  64  65  66  67  69  70  75  78  79  80  82
[51]  84  85  89  91  97  99 100 102 106 108 109 110 116 127 140 141 150 169 170 171 172 501 502  34  37
[76]  49  51  58  92  93 119 120 122 125 132 134 164  18  20  55  57 130 145 153  25  53  60  83  86  87
[101] 104 503

result_mape2=result_mape[result_mape[,1]%in%stores102,]
result_maape2=result_maape[result_maape[,1]%in%stores102,]
result_mase2=result_mase[result_mase[,1]%in%stores102,]



test1=data.frame(cbind(result_mape2[,c(1,2,7,8,9,10)],result_maape2[,c(7,8,9,10)],result_maape2[,c(7,8,9,10)] ))
names(test1)=c('store','sku','mape_choice','mape_mape','mape_maape','mape_mase'
               ,'maape_choice','maape_mape','maape_maape','maape_mase'
               ,'mase_choice','mase_mape','mase_maape','mase_mase')



postscript("../figs/comparebyMAPE.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")

  sorted_test1=test1[order(test1$mape_mape,decreasing =T),]
  plot(sorted_test1$mape_mape,type="b",xlab="",ylab="MAPE",cex=1.5)
  points(sorted_test1$maape_mape,col=2)
  legend(970,1.5, c("MAPE", "MAAPE","MASE"), col = c(1,2,4),   pch = c(1,1,3), cex = 1.0)
  points(sorted_test1$mase_mape,col=4,pch=3)

dev.off() 

sorted_test1=test1[order(test1$maape_mape,decreasing =T),]
plot(sorted_test1$maape_mape,type="b",xlab="",ylab="MAPE",cex=1.5)
points(sorted_test1$mape_mape,col=2)

169 8006256 
x=dat2[dat2[,1]=="47" & dat2[,2]=="8091613",3:95]
x10=dat2[dat2[,1]=="47" & dat2[,2]=="8091613",96:105]
8006256 1.0747468         1.2433906   




postscript("../figs/comparebyMAAPE.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")

  sorted_test1a=test1[order(test1$mape_maape,decreasing =T),]
  plot(sorted_test1a$mape_maape,type="b",xlab="",ylab="MAAPE",cex=1.5)
  points(sorted_test1a$maape_maape,col=2)
  legend(1100,1.4, c("MAPE", "MAAPE","MASE"), col = c(1,2,4),   pch = c(1,1,3), cex = 1.0)
  points(sorted_test1a$mase_maape,col=4,pch=3)

dev.off() 


postscript("../figs/comparebyMASE.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")

  sorted_test1s=test1[order(test1$mape_mase,decreasing =T),]
  plot(sorted_test1s$mape_mase,type="p",xlab="",ylab="MASE",cex=1.5)
  points(sorted_test1s$maape_mase,col=2)
  legend(1100,21.4, c("MAPE", "MAAPE","MASE"), col = c(1,2,4),   pch = c(1,1,3), cex = 1.0)
  points(sorted_test1s$mase_mase,col=4,pch=3)

dev.off() 
