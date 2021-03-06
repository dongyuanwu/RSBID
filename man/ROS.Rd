% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROS.R
\name{ROS}
\alias{ROS}
\title{Random Over-Sampling Algorithm}
\usage{
ROS(data, outcome, perc_maj = 100)
}
\arguments{
\item{data}{A dataset containing the predictors and the outcome. The predictors
can be continuous (\code{numeric} or \code{integer}) or catigorical (\code{character}
or \code{factor}). The outcome must be binary.}

\item{outcome}{The column number or the name of the outcome variable in the dataset.}

\item{perc_maj}{The desired percentage of the size of majority samples that the
minority samples would be reached in the new dataset. The default is 100.}
}
\value{
A new dataset has been balanced.
}
\description{
A balanced dataset would be return by using random over-sampling (ROS) algorithm.
}
\details{
The random over-sampling algorithm generates new samples by randomly sampling
the minority samples with replacement according to the sample size of majority
class, in order to get a more balanced dataset.
}
\examples{
data(abalone)
table(abalone$Class)

newdata1 <- ROS(abalone, 'Class')
table(newdata1$Class)

newdata2 <- ROS(abalone, 'Class', perc_maj=50)
table(newdata2$Class)
}
