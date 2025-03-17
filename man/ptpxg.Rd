\name{Cumulative Density Function of the TPXG Distribution}
\alias{ptpxg}
\title{
Cumulative Density Function of the TPXG Distribution
}
\description{
Computes the cumulative density function of the Two-Parameter Xgamma distribution for given values.
}
\usage{
ptpxg(x, alpha = 1, theta = 1)
}
\arguments{
\item{x}{
A numeric vector with strictly positive values.
}
\item{alpha}{
A positive real number.
}
\item{theta}{
A positive real number.
}
}
\details{
Let \eqn{X \sim \text{TPXG}(\alpha,\theta)}. Then the cumulative distribution function of X is given by:
\deqn{
F(x)=1-\frac{(\alpha+\theta+\alpha \theta x+\frac{1}{2}\alpha \theta^2 x^2)}{(\alpha+\theta)}e^{-\theta x}
\quad x,\theta,\alpha > 0
}
}
\value{
A numeric vector containing the cumulative density function values of the TPXG distribution
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
\code{\link{dtpxg},\link{qtpxg},\link{rtpxg}}
}
\examples{
x <- ptpxg(100)
ptpxg(x, 1, 1)
}
