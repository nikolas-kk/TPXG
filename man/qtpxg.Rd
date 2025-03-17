\name{Inverse Cumulative Density Function of the TPXG Distribution}
\alias{qtpxg}
\title{
Inverse Cumulative Density Function of the TPXG Distribution
}
\description{
Computes the inverse cumulative density function of the Two-Parameter Xgamma distribution for given probabilities.
}
\usage{
qtpxg(p, alpha = 1, theta = 1 , tol = 1e-5)
}
\arguments{
\item{p}{
A numeric vector containing values in [0,1].
}
\item{alpha}{
A positive real number.
}
\item{theta}{
A positive real number
}
\item{tol}{
A positive real number specifying the tolerance level for the convergence of the Newton-Raphson algorithm.
}
}
\details{
This function uses the Newton-Raphson algorithm in order to estimate the inverse cumulative density function.
}
\value{
A numeric vector containing the inverse cumulative density function values of the TPXG distribution
for each of the given values of x.
}
\references{
"Sen, S., Chandra, N. and Maiti, S. S. (2018). On properties and applications of a two-parameter XGamma distribution. Journal of Statistical Theory and Applications, 17(4): 674--685."
}
\author{
Nikolaos Kontemeniotis.

R implementation and documentation: Nikolaos Kontemeniotis \email{kontemeniotisn@gmail.com}
and Michail Tsagris \email{mtsagris@uoc.gr}.
}
\seealso{
\code{\link{dtpxg},\link{ptpxg},\link{rtpxg}}
}
\examples{
p <- runif(100)
qtpxg(p, 1, 1)
}
