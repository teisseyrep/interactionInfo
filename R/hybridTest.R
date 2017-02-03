hybridTest = function(y1,y2,x1,nbins=NULL,alpha=0.05,alpha0=0.05,B=1000,method="emp")
{
  intInfo = numeric(B)

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

  n=length(y1)
  tab=table(y1,y2)
  mi = mi.plugin(tab)
  df1 = (length(unique(y1))-1)*(length(unique(y2))-1)
  pv0  = 1-pchisq(2*n*mi,df=df1)

  type=NULL

  if(pv0>alpha0){
    #Use chisqTest:
    Test1 = chisqTest(y1,y2,x1, nbins=nbins,alpha=alpha,method=method)
    pv = Test1$pv
    intInfo0= Test1$intInfo0
    intInfo=NULL
    alpha = Test1$alpha
    dec=Test1$dec
    B=NULL
    df = Test1$df
    type="chisqTest"
  }else{
    #Use permTest:
    Test1=permTest(y1,y2,x1,nbins=nbins,alpha=alpha,B=B,method=method)
    pv = Test1$pv
    intInfo0=Test1$intInfo0
    intInfo=Test1$intInfo
    alpha= Test1$alpha
    dec= Test1$dec
    B= Test1$B
    df=NULL
    type="permTest"
  }

  res=list()
  res$pv = pv
  res$intInfo0= intInfo0
  res$intInfo=intInfo
  res$alpha = alpha
  res$alpha0 = alpha0
  res$dec=dec
  res$B=B
  res$df = df
  res$type=type
  class(res)="hybridTest"

  return(res)
}


# #Example (Positive Interaction Information, no dependence between y1 and y2):
# y1=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
# y2=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
# x1=c(rep(1,50),rep(0,50))
#
# hybridTest1=hybridTest(y1,y2,x1)
# #In this case, chisqTest was used in hybridTest:
# print(hybridTest1)
#
#
# #Example (Positive Interaction Information, dependence between y1 and y2):
# y1=c(rep(0,40),rep(1,40),rep(1,10),rep(0,10))
# y2=c(rep(0,40),rep(1,40),rep(0,10),rep(1,10))
# x1=c(rep(1,80),rep(0,20))
#
# hybridTest1=hybridTest(y1,y2,x1)
# #In this case, permTest was used in hybridTest:
# print(hybridTest1)









