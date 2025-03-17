\name{Maximum Likelihood Estimation of TPXG Regression Coefficients}
\alias{tpxg.reg}
\title{
Estimation of log-link TPXG regression coefficients.
}
\description{
This function estimates the Two Parameter Xgamma regression coefficients as well as the
\eqn{\alpha} parameter of the Two Parameter Xgamma distribution using the maximum likelihood method.
}
\usage{
tpxg.reg(y,x)
}
\arguments{
\item{y}{
A numeric vector containg strictly positive values.
}
\item{x}{
A matrix or a data.frame with the predictor variables.
}
}
\details{
This implementation employs a logarithmic link function to relate the \eqn{\theta} parameter of the
Two-Parameter Xgamma distribution to the predictor variables.
Specifically, the relationship is defined as:
\deqn{
\theta=e^{X\beta}
}
where X is a matrix whose columns represent the predictor variables, and
\eqn{\beta} is a column vector of corresponding regression coefficients.
}
\value{
A named list containing \eqn{\alpha} parameter, a vector containing the \eqn{\beta} coefficients and the maximum likelihood value.
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
\code{\link{tpxg.mle}}
}
\examples{
x <- matrix( rnorm(100 * 2), ncol = 2 )
y <- rtpxg(100)
tpxg.reg(y, x)
}
