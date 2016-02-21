
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
