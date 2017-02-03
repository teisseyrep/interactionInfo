chisqTest = function(y1,y2,x1,nbins=NULL,alpha=0.05,method="emp")
{

  if(!is.factor(x1)){
    if(!all(x1==round(x1))){
      if(is.null(nbins)){
        x1 = infotheo::discretize(x1)[,1]
      }else{
        x1 = infotheo::discretize(x1,nbins=nbins)[,1]
      }
    }
  }
  if(!is.factor(y1)){
    if(!all(y1==round(y1))){
      if(is.null(nbins)){
        y1 = infotheo::discretize(y1)[,1]
      }else{
        y1 = infotheo::discretize(y1,nbins=nbins)[,1]
      }
    }
  }
  if(!is.factor(y2)){
    if(!all(y2==round(y2))){
      if(is.null(nbins)){
        y2 = infotheo::discretize(y2)[,1]
      }else{
        y2 = infotheo::discretize(y2,nbins=nbins)[,1]
      }
    }
  }
  tempData0 = data.frame(x1,y1,y2)
  intInfo0 = -interinformation(tempData0,method=method)
  df1=(length(unique(y1))-1)*(length(unique(y2))-1)*(length(unique(x1))-1)
  n = length(x1)
  pv = 1-pchisq(2*n*intInfo0,df=df1)
  dec = ifelse(pv>alpha,FALSE,TRUE)

  # chisqTest = new.chisqTest()
  res=list(pv=NULL,intInfo0=NULL,alpha=NULL,dec=NULL,df=NULL)
  res$pv = pv
  res$intInfo0 = intInfo0
  res$alpha = alpha
  res$dec = dec
  res$df = df1
  class(res)="chisqTest"

  return(res)
}


# #Example (XOR problem- all variables discrete):
# y1=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
# y2=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
# x1=c(rep(1,50),rep(0,50))
#
# chisqTest1=chisqTest(y1,y2,x1,nbins=NULL,alpha=0.05)
# print(chisqTest1)
#
# #Example (XOR problem- x1 continuous):
# y1=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
# y2=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
# x1=c(rnorm(50,1,0.5),rnorm(50,0,0.5))
#
# chisqTest1=chisqTest(y1,y2,x1,nbins=NULL,alpha=0.05)
# print(chisqTest1)






