\name{chisqTest}
\alias{chisqTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chi squared test for the Interaction Information.
}
\description{
The function performs chi-squared test for the positiveness of Interaction Information I(y1,y2;x1)=MI(y1,y2|x1)-MI(y1,y2), where MI(y1,y2|x1) is conditional mutual information between y1 and y2, given x1 and MI(y1,y2) is mutual information between y1 and y2. The null hypothesis is

H0: x1 is indpependent from (y1,y2).

The alternative hypothesis is

H1: I(y1,y2;x1)>0.
}
\usage{
chisqTest(y1, y2, x1, nbins=NULL, alpha=0.05,method="emp")
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
  \item{alpha}{
Significance level, the default value is 0.05.
}
\item{method}{
The method used to estimate entropy. See function interinformation in R package infotheo for details.
}
}
\details{
If the variables y1, y2, x1 are not factors, they are discretized using 'discretize' function from R package 'infotheo'. Discretization is needed to calculate Interaction Information. The Interaction Information is computed using function  interinformation from R package infotheo.
}
\value{
A list with class "chisqTest" containing the following components:
 \item{pv}{P-value of chi squared test.}
 \item{intInfo0}{Interaction Information.}
  \item{alpha}{Significance level.}
   \item{dec}{Logical value. TRUE denotes significantly positive Interaction Information.}
  \item{df}{Degrees of freedom.}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Example (XOR problem- all variables discrete):
y1=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
y2=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
x1=c(rep(1,50),rep(0,50))

chisqTest1=chisqTest(y1,y2,x1,nbins=NULL,alpha=0.05)
print(chisqTest1)

#Example (XOR problem- x1 continuous):
y1=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
y2=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
x1=c(rnorm(50,1,0.5),rnorm(50,0,0.5))

chisqTest2=chisqTest(y1,y2,x1,nbins=NULL,alpha=0.05)
print(chisqTest2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
