library("lpSolveAPI")

#example
my.lp<-make.lp(8,10)

set.column(my.lp,1,c(105,0,0,0,0,0,0,0))
set.column(my.lp,2,c(3.5,103.5,0,0,0,0,0,0))
set.column(my.lp,3,c(5,105,0,0,0,0,0,0))
set.column(my.lp,4,c(3.5,3.5,103.5,0,0,0,0,0))
set.column(my.lp,5,c(4,4,4,104,0,0,0,0))
set.column(my.lp,6,c(9,9,9,9,109,0,0,0))
set.column(my.lp,7,c(6,6,6,6,106,0,0,0))
set.column(my.lp,8,c(8,8,8,8,8,108,0,0))
set.column(my.lp,9,c(9,9,9,9,9,9,109,0))
set.column(my.lp,10,c(7,7,7,7,7,7,7,107))

set.objfn(my.lp,c(102,99,101,98,98,104,100,101,102,94))

set.rhs(my.lp,c(12000,18000,20000,20000,16000,15000,12000,10000))
set.constr.type(my.lp,rep("=",8))

my.lp
ColNames <- c("X1", "X2", "X3","X4", "X5", "X6","X7", "X8", "X9","X10")
RowNames <- c("Y1","Y2","Y3","Y4","Y5","Y6","Y7","Y8")
dimnames(my.lp) <- list(RowNames, ColNames)

#write to text file
write.lp(my.lp,'model1.lp',type='lp')

solve(my.lp)
get.variables(my.lp)
get.objective(my.lp)

#Bonds function
BondsLiab <- function(Price, Coupon, Maturity, Liability){
  
  library("lpSolveAPI")
  
  #Creating a dummy matrix to construct input to lp
  Dummy = matrix(0,length(Liability),length(Maturity))
  
  for(i in seq(1,length(Liability),1)){
    for(j in 1:length(Maturity))
    {
      Dummy[i,j] <- ifelse(Maturity[j] == i,Coupon[j]+100,ifelse(Maturity[j] > i,Coupon[j],0))
      #Dummy[i,j] <- ifelse(Maturity[j] <= i && Maturity[j] > (i-1),Coupon[j]+100,ifelse(Maturity[j] > i,Coupon[j],0))
    }
  }
  
  my.lp <-make.lp(length(Liability),length(Price))
  
  for(i in 1:ncol(Dummy)){
    set.column(my.lp,i,as.vector(Dummy[,i]))
  }
  
  #Set objective function
  set.objfn(my.lp,Price)
  
  #set objective direction
  lp.control(my.lp,sense='min')
  
  set.rhs(my.lp,Liability)
  
  #Set constraint direction
  set.constr.type(my.lp,rep("=",length(Liability)))
  
  #Creating empty column and row names vectors
  ColNames <- list()
  RowNames <- list()
  
  #Setting dimensions
  for(i in 1:length(Price)){
    ColNames[i] <- paste("Bond",i)
  }
  for(i in 1:length(Liability)){
    RowNames[i] <- paste("Liability",i)
  }
  dimnames(my.lp) <- list(RowNames, ColNames)
  
  #write to text file
  write.lp(my.lp,'model.lp',type='lp')
  
  solve(my.lp)
  variables <- get.variables(my.lp)
  
  return(my.lp)
  
}

#Test on toy problem
Price <- c(102,99,101,98,98,104,100,101,102,94)
Coupon <- c(5,3.5,5,3.5,4,9,6,8,9,7)
Maturity <- c(1,2,2,3,4,5,5,6,7,8)
Liability <- c(12000,18000,20000,20000,16000,15000,12000,10000)

get.variables(BondsLiab(Price,Coupon,Maturity,Liability))

#WSJ Bdnds

#Reading the files

#wsj has the table from http://online.wsj.com/mdc/public/page/2_3020-treasury.html
#Columns in wsj: Maturity, Coupon, Bid, Asked, Chg, Asked.Yield
wsj = read.csv("WSJ_Web.csv")

#liab has the liablity details
#Columns in liab: Date, Liability, Period
liab = read.csv("Liability_Web.csv")

#Modifying dates to "date" format
wsj$date.maturity <- as.Date(as.Date(wsj$Maturity, format = "%m/%d/%Y"))
liab$date.liab <- as.Date(as.Date(liab$Date, format = "%m/%d/%Y"))
liab$Liability <- as.numeric(liab$Liability)

#Subsetting WSJ for bonds maturing only on liability dates; joining it to the period of maturity
wsj1 <- subset(wsj, wsj$date.maturity %in% liab$date.liab)
wsj <- merge(x = wsj1, y = liab[ ,c("date.liab", "Period")], by.x= c("date.maturity"), by.y=c("date.liab"), all.x=TRUE)

#Creating vectors for functions
P = wsj$Asked
C = wsj$Coupon/2
M = wsj$Period
L = liab$Liability

Bonds <- BondsLiab(P,C,M,L)
get.variables(Bonds)
get.dual.solution(Bonds)

#Plotting duals wrt liability date
Duals = get.dual.solution(Bonds)[1:12]
Liability_Dates = liab$date.liab
Periods = c(1,2,3,4,5,6,7,8,9,10,11,12)

plot.data <- data.frame(Liability_Dates,Periods,Duals)

#plot(Liability_Dates,Duals,main='Sensitivity Parameters',type='o',col = 'dark blue')
#points(Liability_Dates,Duals,bg="dark blue", pch = 22, col = "black")

library(ggplot2)
ggplot(data = plot.data, aes(x = Liability_Dates, y = Duals)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) + 
  labs(title = "Sensitivity Plot",x = "Liability Dates", y = "Duals") + 
  theme(plot.title = element_text(size = rel(1.5)))
