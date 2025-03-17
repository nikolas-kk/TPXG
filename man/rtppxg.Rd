\name{Random Numbers from the TPPGX Distribution}
\alias{rtppxg}
\title{
Random Numbers from the TPPGX Distribution
}
\description{
Generates random numbers form the Two Parameter Poisson Xgamma distribution.
}
\usage{
rtppxg(n, alpha = 1, theta = 1)
}
\arguments{
\item{n}{
An integer indicating the desired sample size.
}
\item{alpha}{
A positive real number.
}
\item{theta}{
A positive real number.
}
}
\details{
In order to obtain random numbers from the TPPXG distribution this function works in two parts. First it
generates n random \eqn{\lambda} values where \eqn{\lambda|\alpha,\theta \sim \text{TPXG}(\alpha,\theta)}. Given
this, it generates n numbers X where \eqn{X|\lambda \sim \text{Poisson}(\lambda)}.
}
\value{
A numeric vector of size n containing random values from the TPPXG distribution.
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
\code{\link{dtppxg},\link{qtppxg},\link{ptppxg}}
}
\examples{
x <- rtppxg(100)
}
