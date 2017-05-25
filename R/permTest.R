permTest = function(y1,y2,x1,nbins=NULL,alpha=0.05,B=1000,method="emp")
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


    for(b in 1:B){
      perm1 = sample(1:length(x1))
      x1p = x1[perm1]
      tempData = data.frame(x1p,y1,y2)
      intInfo[b] = -interinformation(tempData,method=method)
    }


  tempData0 = data.frame(x1,y1,y2)
  intInfo0 = -interinformation(tempData0,method=method)
  pv=length(which(intInfo>=intInfo0))/length(intInfo)
  dec = ifelse(pv>alpha,FALSE,TRUE)

  res=list(pv=NULL,intinfo=NULL,intInfo0=NULL,alpha=NULL,dec=NULL,B=NULL)
  res$pv = pv
  res$intInfo0 = intInfo0
  res$intInfo = intInfo
  res$alpha = alpha
  res$dec = dec
  res$B = B
  class(res)="permTest"

  return(res)
}


# #Example (XOR problem):
# y1=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
# y2=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
# x1=c(rep(1,50),rep(0,50))
#
# permTest1=permTest(y1,y2,x1)
# # Make histogram for Interaction Information, based on permutation samples.
# hist(permTest1$intInfo)
#
# #Example (XOR problem- x1 continuous):
# y1=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
# y2=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
# x1=c(rnorm(50,1,0.5),rnorm(50,0,0.5))
#
# permTest1=permTest(y1,y2,x1)
# # Make histogram for Interaction Information, based on permutation samples.
# hist(permTest1$intInfo)






