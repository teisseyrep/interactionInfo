\name{hybridTest}
\alias{hybridTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hybrid test for the Interaction Information.
}
\description{
The function performs a hybrid test for the positiveness of Interaction Information I(y1,y2;x1)=MI(y1,y2|x1)-MI(y1,y2), where MI(y1,y2|x1) is conditional mutual information between y1 and y2, given x1 and MI(y1,y2) is mutual information between y1 and y2. The null hypothesis is

H0: x1 is indpependent from (y1,y2).

The alternative hypothesis is

H1: I(y1,y2;x1)>0.

The hybrid test combines a permutation test (see function \code{\link{permTest}}) and chi squared test (see function \code{\link{chisqTest}}). If the dependence between y1 and y2 is weak then the chi squared test is used. Otherwise, the permutation test is used.
}
\usage{
hybridTest(y1,y2,x1,nbins=NULL,alpha=0.05,alpha0=0.05,B=1000,method="emp")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y1}{
First variable.
}
  \item{y2}{
Second variable.
}
  \item{x1}{
The additional coviariate.
}
  \item{nbins}{
Number of bins to be used for the discretization. By default the number of bins is set to (N)^(1/3) where N is the number of samples.
}
  \item{alpha0}{
Significance level of the initial test for dependence between y1 and y2, the default value is 0.05.
}
  \item{alpha}{
Significance level, the default value is 0.05.
}
  \item{B}{
Number of permutations, the default value is 1000.
}
\item{method}{
The method used to estimate entropy. See function interinformation in R package infotheo for details.
}
}
\details{
If the variables y1, y2, x1 are not factors, they are discretized using 'discretize' function from R package 'infotheo'. Discretization is needed to calculate Interaction Information. The Interaction Information is computed using function  interinformation from R package infotheo.
}
\value{
A list with class "hybridTest" containing the following components:
 \item{pv}{P-value of permutation test.}
 \item{intInfo0}{Interaction Information computed on original data.}
 \item{intInfo}{Vector of length B, containing values of Interaction Information corresponding to B permuted samples. The value is returned only if the permutation test was used. Otherwise it is NULL.}
  \item{alpha}{Significance level.}
   \item{alpha0}{Significance levelof the initial test.}
   \item{dec}{Logical value. TRUE denotes significantly positive Interaction Information.}
  \item{B}{Number of permutations. The value is returned only if the permutation test was used. Otherwise it is NULL.}
  \item{df}{Degrees of freedom. The value is returned only if the chi squared test was used. Otherwise it is NULL.}
  \item{type}{Type of the test used in the hybrid procedure. The possible values are: "permTest" if the permutation test was used and "chisqTest" if the chi sqaured test was used.}
}
\references{
Pawel Teisseyre, Jan Mielniczuk, Michal J. Dabrowski, Detection of hidden associations and interactions in biomedical data using Interaction Information, manuscript, 2017.
}
\author{
Pawel Teisseyre
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{chisqTest}}, \code{\link{permTest}}
}
\examples{
#Example (Positive Interaction Information, no dependence between y1 and y2):
y1=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
y2=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
x1=c(rep(1,50),rep(0,50))

hybridTest1=hybridTest(y1,y2,x1)
#In this case, chisqTest was used in hybridTest:
print(hybridTest1)


#Example (Positive Interaction Information, dependence between y1 and y2):
y1=c(rep(0,40),rep(1,40),rep(1,10),rep(0,10))
y2=c(rep(0,40),rep(1,40),rep(0,10),rep(1,10))
x1=c(rep(1,80),rep(0,20))

hybridTest1=hybridTest(y1,y2,x1)
#In this case, permTest was used in hybridTest:
print(hybridTest1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
