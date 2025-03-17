\name{Maximum Likelihood Estimation of TPXG Distribution}
\alias{tpxg.mle}
\title{
Maximum likelihood estimation of the TPXG distribution parameters.
}
\description{
Estimation of \eqn{\alpha} and \eqn{\theta} parameters of Two Parameter Xgamma distribution using maximum likelihood.
}
\usage{
tpxg.mle(x)
}
\arguments{
\item{x}{
A numeric vector containg strictly positive values.
}
}
\details{
The log-likelihood functiono of the TPXG distribution is given by:
\deqn{\ln L(\alpha, \theta|x) = 2n \ln \theta - n \ln (\alpha + \theta) - \theta \left(\sum_{i=1}^{n} x_i\right)
+ \sum_{i=1}^{n} \ln \left(1 + \frac{\alpha\theta}{2}x_i^2\right)}
}
\value{
A named numeric vector containing the estimated values for \eqn{\alpha}, \eqn{\theta} and maximum likelihood.
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
\code{\link{tpxg.reg}}
}
\examples{
x <- rtpxg(1000)
tpxg.mle(x)
}

