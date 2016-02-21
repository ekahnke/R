library(lpSolve)

#2b - (W,C)
c<-c(2000,3000)
A<-matrix(0,3,2)
A[1,]<-c(1,1)
A[2,]<-c(3,2)
A[3,]<-c(2,4)
dir<-rep("<=",3)
b<-c(450,1000,1200)
s=lp("max",c,A,dir,b,compute.sens = 1)
s$solution
#200 200

#2c,d
s$duals
s$duals.from
s$duals.to

s$sens.coef.from
s$sens.coef.to

#3 - (1,2,3,4,5)
c<-c(13,16,16,14,39)
A<-matrix(0,7,5)
A[1,]<-c(11,53,5,5,29)
A[2,]<-c(3,6,5,1,34)
A[3,]<-c(1,0,0,0,0)
A[4,]<-c(0,1,0,0,0)
A[5,]<-c(0,0,1,0,0)
A[6,]<-c(0,0,0,1,0)
A[7,]<-c(0,0,0,0,1)
dir<-rep("<=",7)
b<-c(40,20,1,1,1,1,1)
s=lp("max",c,A,dir,b,compute.sens = 1)
s$solution
#1.0000000 0.2008600 1.0000000 1.0000000 0.2880835

#4 - (C,M,WB)
c<-c(.18,.23,.05)
A<-matrix(0,7,3)
A[1,]<-c(72,121,65)
A[2,]<-c(107,500,0)
A[3,]<-c(1,0,0)
A[4,]<-c(0,1,0)
A[5,]<-c(0,0,1)
A[6,]<-c(72,121,65)
A[7,]<-c(107,500,0)
dir1<-rep("<=",5)
dir2<-rep(">=",2)
dir<-c(dir1,dir2)
b<-c(2250,50000,10,10,10,2000,5000)
s=lp("min",c,A,dir,b,compute.sens = 1)
s$solution
#1.944444 10.000000 10.000000

#5 - (UT,FT,UC,FC)
c<-c(30,100,30,80)
A<-matrix(0,2,4)
A[1,]<-c(40,40,30,30)
A[2,]<-c(2,5,2,4)
dir<-rep("<=",2)
b<-c(40000,6000)
s=lp("max",c,A,dir,b,compute.sens = 1)
s$solution
#0.000    0.000    0.000 1333.333

s$duals
s$duals.from
s$duals.to
s$sens.coef.from
s$sens.coef.to