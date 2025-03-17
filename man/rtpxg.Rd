\name{Random Numbers from the TPGX Distribution}
\alias{rtpxg}
\title{
Random Numbers from the TPGX Distribution
}
\description{
Generates random numbers form the Two Parameter Xgamma distribution.
}
\usage{
rtpxg(n, alpha = 1, theta = 1)
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
The TPXG distribution is a mixture of \eqn{exponential(\theta)} and \eqn{gamma(3,\theta)} with mixing proportions
\eqn{\frac{\theta}{\alpha+\theta}} and \eqn{\frac{\alpha}{\alpha+\theta}} respectively.
}
\value{
A numeric vector of size n containing random values from the TPXG distribution.
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
\code{\link{dtpxg},\link{qtpxg},\link{ptpxg}}
}
\examples{
x <- rtpxg(100)
}
