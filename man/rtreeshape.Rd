\name{rtreeshape}
\alias{rtreeshape}
\alias{rbiased}
\alias{rpda}
\alias{ryule}

\title{ Generate a list of random binary trees according to a given model  }

\description{
These functions generate a tree or a list of trees of class \code{"treeshape"} according to the Yule, PDA, Biased models. Speciation-specified models are also allowed.
}

\usage{
rtreeshape(n, tip.number = 50, p = 0.3, model="", Q="")
rbiased(tip.number = 20, p = 0.3)
rpda(tip.number)
ryule(tip.number)
}

\arguments{
\item{n}{ The number of trees to generate.}
\item{tip.number}{ The number of tips of the trees to generate. Can be a vector.}
\item{p}{ Only used when \code{model="biased"} or in \code{rbiased}. It represents the bias factor of the tree to generate.}  \item{model}{ A character string equals to \code{"yule"}, \code{"pda"} or \code{"biased"}.}
\item{Q}{A two variables (\code{n} and \code{i}) function.}
}

\details{
The \code{"Q"} and \code{"model"} arguments cannot be specified at the same time. An error will be returned if both arguments are specified. \cr
  
If \code{tip.number} is a vector, \code{n} trees will be generated for each size contained by \code{tip.number}
  
\code{Q} enables you to build trees of class \code{"treeshape"} according to the \emph{Markov branching} model described by D. Aldous. \code{Qn(i)} is the probability that the left daughter clade of an internal node with \code{n} descendents contains \code{i} tips. The \code{Qn(i)} need not sum to one. Still, be carefull when you specify this distribution: computational errors may occur for complicated distributions and/or large trees. \cr
  
The Yule model, also known as Markov model, can be described as follows. At any time, all the extant branches have the same probability to split into two subspecies. \cr
The PDA model (Proportional to Distinguishable Arrangements) is not a model of growing tree. Instead, each tree with n tips has the same probability to be generated under this model. There is \eqn{(2n-3)!!}{(2n-3)!!} possible trees with n tips.\cr
The Biased model is a model of growing tree. When a species with speciation rate \emph{r} splits, one of its descendent species is given the rate \emph{pr} and the other is given the speciation rate \eqn{1-pr}{(1-pr)} where p is a probability parameter. The Biased model was introduced by Kirkpatrick and Slatkin (1993).
  }

\value{
A list of objects of class \code{"treeshape"} if \code{n}>1 or a single tree of class \code{"treeshape"} if \code{n}=1.
}

\references{
Mooers, A. O. and Heard, S. B. (Mar., 1997), Inferring Evolutionnary Process from Phylogenetic Tree Shape. \emph{The Quarterly Review of Biology}, \bold{72}, 31-54, for more details about the Yule and PDA models.
  
Aldous, D. J. (1996), \emph{Probability Distributions on Cladograms}. pp.1-18 of \emph{Random Discrete Structures} eds D. Aldous and R. Pemantle, IMA Volumes Math. Appl. 76.

Kirkpatrick, M. and Slatkin, M. (1993) Searching for evolutionary patterns in the shape of a phylogenetic tree. \emph{Evolution}, \bold{47}, 1171 -- 1181.
}

\author{
Michael Blum <\email{michael.blum@imag.fr}>\cr 
Nicolas Bortolussi <\email{nicolas.bortolussi@imag.fr}>\cr
Eric Durand <\email{eric.durand@imag.fr}>\cr
Olivier François <\email{olivier.francois@imag.fr}>
}

\examples{

## Summary of a PDA tree with 100 tips:
summary(rpda(100))
## Summary of a Yule tree with 100 tips:
summary(ryule(100))
  
## Generate trees with different sizes
trees=rtreeshape(n=2, tip.number=c(10,20), model="yule")
length(trees)
plot(trees[[1]])
plot(trees[[4]])
  
## Histogram of Colless' indices for a list of 1000 PDA trees with 60 tips
hist(sapply(rtreeshape(1000,60,model="pda"),FUN=colless,norm="pda"),freq=FALSE)

## Histogram of shape statistics for a list of 1000 Yule trees with 100 tips 
##      (takes some time to compute) 
main="Histogram of shape statistics for a list of 1000 Yule trees"
hist(sapply(rtreeshape(1000,100,model="yule"),FUN=shape.statistic,norm="yule"),
      freq=FALSE, main=main)
## It should be a gaussian with mean 0 and standard deviation 1.
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x))	

## Building a tree using Markov splitting model
Q <- function(n,i) {
      if (i==1) {return(1)}
      else {return(0)}
}
tree=rtreeshape(n=1, tip.number=10, Q=Q)
plot(tree)
}

\keyword{datagen}

