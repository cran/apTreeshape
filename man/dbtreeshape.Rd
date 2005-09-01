\name{dbtreeshape}
\alias{dbtreeshape}
\alias{pandit}
\alias{treebase}

\title{ Collect trees from a database. }

\description{
\code{dbtreeshape} is a function that calls either \code{pandit} or \code{treebase}.\cr
\code{pandit} connects to the database Pandit and collects a given list of trees from there, converting them into the \code{"treeshape"} or \code{"phylo"} class. \cr
\code{treebase} does the same on the TreeBASE database.
}

\usage{
dbtreeshape(db, tree, class="treeshape", type="s", quiet=FALSE, 
      model=NULL, p=0.3)
pandit(tree, class="treeshape", type="s", quiet=FALSE, model=NULL, p=0.3)
treebase(tree, class="treeshape", quiet=FALSE, model=NULL, p=0.3)
}

\arguments{
\item{db}{ A character string equals to the name of the database to connect: \code{"pandit"} or \code{"treebase"}.}
\item{tree}{ ID vector or list of the trees to be collected from the base.}
\item{class}{The class of the returned trees: must be one of \code{"treeshape"} (default) or \code{"phylo"}.}
\item{type}{ The type of the tree: an object of class \code{"character"} equals to either \code{"s"} (default) or \code{"f"} (seed or full). This option is only used for the Pandit database.}
\item{quiet}{ A \code{logical} value. If \code{TRUE}, nothing will be printed on screen. If \code{FALSE} (default), lines will be printed to indicate informations about the gathered trees.}
\item{model}{ Argument to be passed to \code{as.treshape.phylo}. One of \code{NULL} (default), \code{"pda"} or \code{"yule"}}
\item{p}{ Argument to be passed to \code{as.treshape.phylo}.}  
}

\details{
The aim of these functions is to provide a simplified method to retrieve trees and to put them into the \code{"treeshape"} or \code{"phylo"} format, in order to be able to apply the methods \code{apTreeshape} provides. \cr
These functions connect to databases and retrieve trees from their reference number in the database. There is currently no option to retrieve trees from their names. See references for more details about the Pandit and TreeBASE databases and the description of their data. \cr
These functions may simulate trees with polytomies (see \code{as.treeshape} for more details about the polytomy resolution).
}

\value{
An object of class \code{"treeshape"} or class \code{"phylo"} if only one tree is requested, a list of trees otherwise.
}

\seealso{
\code{\link{as.treeshape}} for more details about the convertion of non-binary trees.
}

\references{ 
\url{http://www.ebi.ac.uk/goldman-srv/pandit/} for more details about the Pandit database.
  
\url{http://www.treebase.org/treebase/} for informations about the TreeBASE database. 
}

\author{ 
Michael Blum <\email{michael.blum@imag.fr}>\cr
Nicolas Bortolussi <\email{nicolas.bortolussi@imag.fr}> \cr
Eric Durand <\email{eric.durand@imag.f}>\cr
Olivier François <\email{olivier.francois@imag.fr}>
}

\examples{
if(!inherits(try(open(url("http://www.google.com")), silent = TRUE),
   "try-error")) {
## Sackin's index of a tree within TreeBASE.
sackin(treebase(tree=715))

## Colless' index of a tree within Pandit
colless(pandit(tree=1))
  
## Collects a tree without printings: 
plot(pandit(709, quiet=TRUE))

## Collects a list of trees :
trees=pandit(1:5)
summary(trees[[2]])
  
## Collects a non-binary tree
phy=treebase(741, class="phylo")
plot(phy)
tree=treebase(741, class="treeshape")
tree=treebase(741, class="treeshape", model="yule")
plot(tree)
}
}
\keyword{manip}
\keyword{IO}
