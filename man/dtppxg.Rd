\name{Probability Mass Function of the TPPXG Distribution}
\alias{dtppxg}
\title{
Probability Mass Function of the TPPXG Distribution
}
\description{
Computes the probability mass function of the Two Parameter Poisson Xgamma distribution
for a given set of non-negative integer values.
}
\usage{
dtppxg(x, alpha = 1, theta = 1)
}
\arguments{
\item{x}{
A numeric vector containing non-negative integer values.
}
\item{alpha}{
A positive real number.
}
\item{theta}{
A positive real number.
}
}
\details{
Assume a random variable X follows the two-parameter Poisson-Xgamma distribution,
which has the following stochastic representation:
\deqn{
X|\lambda \sim \text{Poisson}(\lambda)
}
\deqn{
\lambda|\alpha,\theta \sim \text{TPXG}(\alpha,\theta)
}
Then the probability mass function of X is given by:
\deqn{
P(X=x)=\frac{\theta^2}{(\alpha+\theta)(1+\theta)^{x+3}}
\left\{(1+\theta)^2+\frac{\alpha \theta}{2}(x+1)(x+2)\right\}; x = 0, 1, 2, 3, \dots
}
}
\value{
A numeric vector containing the probability mass function value of the
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
\code{\link{ptppxg},\link{qtppxg},\link{rtppxg}}
}
\examples{
x <- rtppxg(100)
dtppxg(x, 1, 1)
}
