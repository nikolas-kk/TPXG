\name{Inverse Cumulative Mass Function of the TPPXG Distribution}
\alias{qtppxg}
\title{
Inverse Cumulative Mass Function of the TPPXG Distribution
}
\description{
Computes the inverse cumulative mass function (quantile function) of the Two Parameter Poisson Xgamma distribution for given probability values.
}
\usage{
qtppxg(p, alpha = 1, theta = 1, tol = 1e-5)
}
\arguments{
\item{p}{
A numeric vector containg values in [0,1].
}
\item{alpha}{
A positive real number.
}
\item{theta}{
A positive real number.
}
\item{tol}{
A positive real number specifying the tolerance level for the convergence of the Newton-Raphson algorithm.
}
}
\details{
This function uses the Newton-Raphson algorithm in order to estimate the inverse cumulative mass function.
}
\value{
A numeric vector containing the inverse cumulative mass function of the
TPPXG distribution at the given values of p.
}
\references{
"Wani, M. A., Ahmad, P. B., Para, B. A. and Elah, N. (2023). A new regression model for count data with applications to health care data. International Journal of Data Science and Analytics."
}
\author{
Nikolaos Kontemeniotis.

R implementation and documentation: Nikolaos Kontemeniotis \email{kontemeniotisn@gmail.com}
and Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{\link{dtppxg},\link{ptppxg},\link{rtppxg}}
}
\examples{
p <- runif(100)
qtppxg(p, 1, 1)
}
