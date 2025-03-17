\name{Probability Density Function of TPXG Distribution}
\alias{dtpxg}
\title{
Probability Density Function of TPPXG Distribution
}
\description{
Computes the probability density function of the Two Parameter Xgamma distribution
for a given set positive real values.
}
\usage{
dtpxg(x, alpha = 1, theta = 1)
}
\arguments{
 \item{x}{
A numeric vector containing strictly positive values.
}
\item{alpha}{
A positive real number.
}
\item{theta}{
A positive real number.
}
}
\details{
Let \eqn{U\sim \text{TPXG}(\alpha,\theta)}.Then the probability density function of U is given by:
\deqn{
f(u;\alpha,\theta)=\frac{\theta^2}{\alpha+\theta}(1+\frac{\alpha \theta}{2}u^2)e^{-\theta u}
\quad \theta,\alpha > 0 , u > 0
}
}
\value{
A numeric vector containing the probability density function value of the
TPXG distribution for each of the given values of x.
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
\code{\link{rtpxg},\link{qtpxg},\link{ptpxg}}
}
\examples{
x <- rtpxg(100)
dtpxg(x, 1, 1)
}
