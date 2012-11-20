\name{depth}
\alias{depth}
\title{Depth calculation}
\description{
Computes the depth of a point or vectors of points with respect to a multivariate data set.
}
\usage{
depth(u, X, method = "Projection", ndir = 1000)
}
\arguments{
  \item{u}{Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.}
  \item{X}{The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).}
  \item{method}{Character string which determines the depth function. \code{method} can be "Projection" (the default), "Mahalanobis", "Euclidean" or "Tukey". For details see \code{\link{depth}.}}
  \item{ndir}{Number of random directions used when Projection and Tukey depth is approximated.}
}
\details{ 

Irrespective of dimension, Projection and Tukey's depth is obtained by approximate calculation. 

Calculation of Mahalanobis and Euclidean depth is exact.

}
\value{
Returns the depth of multivariate point \code{u} with respect to data set \code{X}.
}
\references{Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 783--858.

Rousseuw, P.J. and Ruts, I. (1996), AS 307 : Bivariate location depth, \emph{Appl. Stat.-J. Roy. S. C},  \bold{45}, 516--526.

Rousseeuw, P.J. and Struyf, A. (1998), Computing location depth and regression depth in higher dimensions, \emph{Stat. Comput.}, \bold{8}, 193--203.

Zuo, Y. amd Serfling, R. (2000), General Notions of Statistical Depth Functions, \emph{Ann. Statist.},  \bold{28}, no. 2, 461--482.}

\author{Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.}

\seealso{\code{\link{depthContour}} and \code{\link{depthPersp}} for depth graphics.}
\examples{
## Calculation of Projection depth
data(starsCYG, package = "robustbase")
depth(t(colMeans(starsCYG)), starsCYG)

#Aslo for matrices
depth(starsCYG, starsCYG)

## Projection depth applied to a large bivariate data set
set.seed(356)
x <- matrix(rnorm(9999), nc = 3)
depth(x, x)

}

\keyword{multivariate}
\keyword{nonparametric}
\keyword{robust}
\keyword{depth function}
