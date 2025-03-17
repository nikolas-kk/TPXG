\name{Maximum Likelihood Estimation of TPPXG Regression Coefficients}
\alias{tppxg.reg}
\title{
Estimation of the TPPXG regression coefficients.
}
\description{
This function estimates the Two Parameter Poisson Xgamma regression coefficients as well as the
\eqn{\alpha} parameter of the Two Parameter Poisson Xgamma distribution using the maximum likelihood method.
}
\usage{
tppxg.reg(y, x)
}
\arguments{
\item{y}{
A numeric vector containg non-negative integer values.
}
\item{x}{
A matrix or a data.frame with the predictor variables.
}
}
\details{
The \eqn{\theta} parameter has been transformed as a function of the expected value of the response variable Y
in the following manner:
\deqn{
\theta=\frac{1-\alpha \mu +\sqrt{(\alpha \mu -1)^2+12\alpha \mu}}{2\mu}}
Given that the response variable satisfies \eqn{Y_i \sim \text{TPPXG}(\alpha, \mu_i)}, then the
\eqn{i^{\text{th}}} mean of Y is related to the predictor variables using the log link function:
\deqn{
\mu_i=e^{x_i^T \beta} \quad i=1,2,3,\dots n
}
For more details, see the paper referenced below.

}
\value{
A named list containing \eqn{\alpha} parameter, a vector containing the \eqn{\beta} coefficients and the maximum likelihood value.
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
\code{\link{tppxg.mle}}
}
\examples{
x <- matrix( rnorm(100 * 2), ncol = 2 )
y <- rtppxg(100)
tppxg.reg(y, x)
}
