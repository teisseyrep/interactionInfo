\name{allpairsTest}
\alias{allpairsTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tests for the Interaction Information for several pairs of variables.
}
\description{
The function performs tests for the positeveness of Interaction Information (chi squared test, permutation test or hybrid test) for all pairs in the data set.
}
\usage{
allpairsTest(x,y,calculateTest=TRUE,type="hybridTest",nbins=NULL,alpha=0.05,alpha0=0.05,B=1000,method="emp",trace=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Matrix containing variables in the columns.
}
  \item{y}{
Additional variable.
}
  \item{calculateTest}{
Logical variable indicating whether the tests should be performed. If the value is FALSE, only the Interaction Information is computed. Default is TRUE.
}
  \item{type}{
Type of the test for positeveness of the Interaction Information. The possible values are: "hybridTest", "chisqTest" and "permTest". Default is "hybridTest".
}
  \item{nbins}{
Number of bins to be used for the discretization. By default the number of bins is set to (N)^(1/3) where N is the number of samples.
}
  \item{alpha0}{
Significance level of the initial test (see \code{\link{hybridTest}} for details), the default value is 0.05.
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
\item{trace}{
Logical variable indicating whether to print the analysis progress.
}
}
\details{
The function computes the Interaction Information I(xi,xj;y), where xi and xj are variables in x (columns of matrix x), for all pairs (xi,xj).
If the variables xi, xj, y are not factors, they are discretized using 'discretize' function from R package 'infotheo'. Discretization is needed to calculate Interaction Information. The Interaction Information is computed using function  interinformation from R package infotheo.
}
\value{
Data frame containing 4 (or 3 if calculateTest=FALSE) columns: index of the first variable, index of the second variable, Interaction Information, p-value.
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
\code{\link{hybridTest}}, \code{\link{chisqTest}}, \code{\link{permTest}}
}
\examples{
#Example (strong interaction between 1 and 2 variable in x and no interactions between the remaining variables):
x= matrix(0,nrow=100,ncol=4)
x[,1]=c(rep(0,25),rep(1,25),rep(1,25),rep(0,25))
x[,2]=c(rep(0,25),rep(1,25),rep(0,25),rep(1,25))
x[,3] = rnorm(100)
x[,4] = rnorm(100)
y=c(rep(1,50),rep(0,50))
allpairsTest1=allpairsTest(x,y,calculateTest=TRUE)
print(allpairsTest1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
