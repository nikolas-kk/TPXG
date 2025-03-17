\name{Maximum Likelihood Estimation of TPPXG Distribution}
\alias{tppxg.mle}
\title{
Maximum likelihood estimation of the TPPXG distribution parameters.
}
\description{
Estimation of \eqn{\alpha} and \eqn{\theta} parameters of Two Parameter Poisson Xgamma distribution using maximum likelihood.
}
\usage{
tppxg.mle(x)
}
\arguments{
\item{x}{
A numeric vector containg non-negative integer values.
}
}
\details{
The log-likelihood function of the TPPXG distribution is given by:
\deqn{
\ln L(\alpha,\theta)=2n\ln \theta-n\ln(\alpha+\theta)-\left(3n+\sum_{i=1}^n x_i\right)\ln(1+\theta)+
\sum_{i=1}^n\ln \left((1+\theta)^2+\frac{\alpha \theta}{2}(x_i+1)(x_i+2)\right)
}
}
\value{
A named numeric vector containing the estimated values for \eqn{\alpha}, \eqn{\theta} and maximum likelihood.
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
\code{\link{tppxg.reg}}
}
\examples{
x <- rtppxg(1000)
tppxg.mle(x)
}
