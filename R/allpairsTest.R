allpairsTest = function(x,y,calculateTest=TRUE,type="hybridTest",nbins=NULL,alpha=0.05,alpha0=0.05,B=1000,method="emp",trace=TRUE)
{

  if(!type%in%c("hybridTest","permTest","chisqTest")) stop("Incorrect type!")

  K = ncol(x)
  for(j in 1:K){
    if(!is.factor(x[,j])){
      if(!all(x[,j]==round(x[,j]))){
        if(is.null(nbins)){
          x[,j] = infotheo::discretize(x[,j])[,1]
        }else{
          x[,j] = infotheo::discretize(x[,j],nbins=nbins)[,1]
        }
      }
    }
  }
  if(!is.factor(y)){
    if(!all(y==round(y))){
      if(is.null(nbins)){
        y = infotheo::discretize(y)[,1]
      }else{
        y = infotheo::discretize(y,nbins=nbins)[,1]
      }
    }
  }

  combs = combn(1:K,2)
  ncombs = ncol(combs)
  if(calculateTest){
    results = matrix(0,ncol=4,nrow=ncombs)
  }else{
    results = matrix(0,ncol=3,nrow=ncombs)
  }

  s=1
  for(i in 1:ncombs){
    if(trace){
    if(ncombs>1000){
      if(i%%1000==0){
        cat("Pair ",i/1000," out of", ceiling(ncombs/1000)," thousands \n")
      }
    }
  }
    labelsSel = combs[,i]
    x1 = x[,labelsSel[1]]
    x2 = x[,labelsSel[2]]
    tempData = data.frame(x1,x2,y)
    intInfo = -interinformation(tempData)

    results[s,1]= labelsSel[1]
    results[s,2]= labelsSel[2]
    results[s,3]= intInfo
    if(calculateTest){
      if(type=="hybridTest"){
        Test =hybridTest(x1,x2,y,nbins=nbins,alpha=alpha,alpha0=alpha0,B=B,method=method)
        pv =Test$pv
      }else if(type=="permTest"){
        Test =permTest(x1,x2,y,nbins=nbins,alpha=alpha,B=B,method=method)
        pv=Test$pv
      }else if(type=="chisqTest"){
        Test =chisqTest(x1,x2,y,nbins=nbins,alpha=alpha,method=method)
        pv=Test$pv
      }
      results[s,4]=pv

    }
    s=s+1
  }
  results = data.frame(results)
  names(results)[1:3]=c("Var 1","Var 2","Interaction Information")
  if(calculateTest){
    names(results)[4]="p-value"
  }

  return(results)
}


# #Example (strong interaction between 1 and 2 variable in x):
# x= matrix(0,nrow=100,ncol=4)
# x[,1]=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
# x[,2]=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
# x[,3] = rnorm(100)
# x[,4] = rnorm(100)
# y=c(rep(1,50),rep(0,50))
# allpairsTest1=allpairsTest(x,y,calculateTest=TRUE)
# print(allpairsTest1)
