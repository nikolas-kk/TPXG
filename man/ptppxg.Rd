\name{Cumulative Mass Function of the TPPXG Distribution}
\alias{ptppxg}
\title{
Cumulative Mass Function of the TPPXG Distribution
}
\description{
Computes the cumulative mass function of the Two Parameter Poisson Xgamma distribution for given values.
}
\usage{
ptppxg(x , alpha = 1, theta = 1)
}
\arguments{
\item{x}{
A numeric vector containg non-negative integer values.
}
\item{alpha}{
A positive real number.
}
\item{theta}{
A positive real number.
}
}
\details{
The cumulative distribution function of the Two Parameter Poisson Xgamma is given by:
\deqn{
F(x)=1-\frac{1}{2(\alpha+\theta)(1+\theta)^{x+3}}\left((x^2+5x+6)\alpha \theta^2+2(x+3)\alpha \theta
+2(1+\theta)^2+2\alpha \right)
}
}
\value{
A numeric vector containing the cumulative mass function of the
TPPXG distribution for each of the given values of x.
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
\code{\link{dtppxg},\link{qtppxg},\link{rtppxg}}
}
\examples{
x <- rtppxg(100)
ptppxg(x, 1, 1)
}
