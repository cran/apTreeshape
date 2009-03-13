pkgname <- "apTreeshape"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('apTreeshape')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("aldous.test")
### * aldous.test

flush(stderr()); flush(stdout())

### Name: aldous.test
### Title: Visualizing balance via scatter diagrams
### Aliases: aldous.test
### Keywords: htest

### ** Examples

library(quantreg)
aldous.test(rbiased(2000, p=.5))

## Test with a huge balanced tree:
aldous.test(rbiased(2000, p=.5))


  



cleanEx()
nameEx("all.equal.treeshape")
### * all.equal.treeshape

flush(stderr()); flush(stdout())

### Name: all.equal.treeshape
### Title: Compare two objects of class treeshape
### Aliases: all.equal.treeshape
### Keywords: manip

### ** Examples

  
## Trees with permutations 
data(carnivora.treeshape)
tree=carnivora.treeshape
tree$merge[8,]=c(tree$merge[8,2],tree$merge[8,1])
all.equal(tree, carnivora.treeshape)
 
## Trees with different heights
merge=matrix(NA, 3, 2)
merge[,1]=c(-3,-1,2); merge[,2]=c(-4,-2,1);tree1=treeshape(merge)
merge[,1]=c(-1,-3,1); merge[,2]=c(-2,-4,2);tree2=treeshape(merge)
  
plot(tree1, tree2)
all.equal(tree1, tree2)
all.equal(tree1, tree2, height=TRUE)

## Trees with different names
tree3=treeshape(tree1$merge, c("a", "b", "c", "d"))
tree4=treeshape(tree1$merge, c("1", "2", "3", "4"))
plot(tree3, tree4)
all.equal(tree3, tree4)
all.equal(tree3, tree4, names=TRUE)




cleanEx()
nameEx("as.phylo.treeshape")
### * as.phylo.treeshape

flush(stderr()); flush(stdout())

### Name: as.phylo.treeshape
### Title: Conversion among tree objects
### Aliases: as.phylo.treeshape
### Keywords: manip

### ** Examples


data(primates)
plot(primates)

library(ape)
  
primates.phylo=as.phylo(primates)
plot(primates.phylo)



cleanEx()
nameEx("as.treeshape")
### * as.treeshape

flush(stderr()); flush(stdout())

### Name: as.treeshape
### Title: Conversion among tree objects
### Aliases: as.treeshape as.treeshape.phylo
### Keywords: manip

### ** Examples


library(ape)
data(bird.orders)
## Data set from APE
plot(bird.orders)
  
## "treeshape" conversion
tree=as.treeshape(bird.orders)
plot(tree)
summary(tree)
 



cleanEx()
nameEx("cladesize")
### * cladesize

flush(stderr()); flush(stdout())

### Name: cladesize
### Title: Compute the number of children of a randomly chosen node
### Aliases: cladesize
### Keywords: univar

### ** Examples


# Histogram of random clade sizes 
main="Random clade sizes for random generated trees"
xlabel="clade size"
hist(sapply(rtreeshape(100,tip.number=40,model="yule"),FUN=cladesize),
      freq=FALSE,main=main,xlab=xlabel)



cleanEx()
nameEx("colless")
### * colless

flush(stderr()); flush(stdout())

### Name: colless
### Title: Compute the Colless' shape statistic on tree data
### Aliases: colless
### Keywords: univar

### ** Examples


## Colless' index for a randomly generated PDA tree (unnormalized value)
tpda<-rtreeshape(1,tip.number=70,model="pda")
colless(tpda[[1]],norm="pda")
  
## Histogram of Colless' indices for randomly generated Yule trees
main="Colless' indices for randomly generated Yule trees"
xlab="Colless' indices"
hist(sapply(rtreeshape(300,tip.number=50,model="yule"),FUN=colless,norm="yule"),
      freq=FALSE,main=main,xlab=xlab)
  
## Change the number of tips
hist(sapply(rtreeshape(300,tip.number=100,model="yule"),FUN=colless,norm="yule"),
      freq=FALSE,main=main,xlab=xlab)




cleanEx()
nameEx("cutreeshape")
### * cutreeshape

flush(stderr()); flush(stdout())

### Name: cutreeshape
### Title: Cut objects of class "treeshape"
### Aliases: cutreeshape
### Keywords: manip

### ** Examples


## Data set provided with the library. Type help(cytochromc) for more infos.
data(carnivora.treeshape)  
data(hivtree.treeshape)

## Examples of "bottom" cutting:
bottom.tree=cutreeshape(carnivora.treeshape, 3, "bottom")
plot(carnivora.treeshape, bottom.tree)
bottom.tree=cutreeshape(carnivora.treeshape, 8, "bottom")
plot(carnivora.treeshape, bottom.tree)
  
## Examples of "top" pruning:
top.tree=cutreeshape(hivtree.treeshape, 158, "top")
plot(hivtree.treeshape, top.tree)



cleanEx()
nameEx("hivtree.treeshape")
### * hivtree.treeshape

flush(stderr()); flush(stdout())

### Name: hivtree.treeshape
### Title: Phylogenetic Tree of 193 HIV-1 Sequences
### Aliases: hivtree.treeshape
### Keywords: datasets

### ** Examples


data("hivtree.treeshape") 
summary(hivtree.treeshape)
plot(hivtree.treeshape)



cleanEx()
nameEx("index.test")
### * index.test

flush(stderr()); flush(stdout())

### Name: index.test
### Title: Perform a test on the Yule or PDA hypothesis based on the
###   Colless or the Sackin statistic
### Aliases: colless.test sackin.test
### Keywords: htest

### ** Examples


## Test on a randomly generated Yule tree with 30 tips
a<-rtreeshape(1,30,model="yule")
a<-a[[1]]
  
## Is it more balanced than a Yule tree ?
colless.test(a,alternative="less",model="yule")
## Is it less balanced than a PDA tree ?
colless.test(a,model="pda",alternative="greater")
 
## Test on the phylogenetic tree hiv.treeshape: is it more balanced than 
##      predicted by the Yule model?
data(hivtree.treeshape)
## The tree looks compatible with the null hypothesis
colless.test(hivtree.treeshape, alternative="greater", model="yule")
 
## What happen when we look at the top the tree?
colless.test(cutreeshape(hivtree.treeshape, 160, "top"),
      alternative="greater", model="yule")
colless.test(cutreeshape(hivtree.treeshape, 160, "top"), 
      alternative="greater", model="pda")

## Test with the Sackin's index: is the HIV tree less balanced than 
##      predicted by the PDA model?
sackin.test(hivtree.treeshape,alternative="greater",model="pda") 
## The p.value equals to 1...



cleanEx()
nameEx("likelihood.test")
### * likelihood.test

flush(stderr()); flush(stdout())

### Name: likelihood.test
### Title: Test the Yule model vs PDA (uniform) model.
### Aliases: likelihood.test
### Keywords: htest

### ** Examples


## Generate a Yule tree with 150 tips. Is it likely to be fitting the PDA model?
likelihood.test(ryule(150),model="pda") 
## The p.value is close from 0. We reject the PDA hypothesis.

## Test on the Carnivora tree: is it likely to be fitting the Yule model?
data(carnivora.treeshape)
likelihood.test(carnivora.treeshape) 
## The p.value is high, so it's impossible to reject the Yule hypothesis.



cleanEx()
nameEx("maxlik.betasplit")
### * maxlik.betasplit

flush(stderr()); flush(stdout())

### Name: maxlik.betasplit
### Title: Maximum likelihood of beta in the Beta-splitting model
### Aliases: maxlik.betasplit
### Keywords: htest

### ** Examples

tree.pda<-rtreeshape(n=1, tip.number=50, model="pda")[[1]]
maxlik.betasplit(tree.pda,confidence.interval="none")
maxlik.betasplit(tree.pda,confidence.interval="bootstrap")
maxlik.betasplit(tree.pda,confidence.interval="profile")



cleanEx()
nameEx("plot.treeshape")
### * plot.treeshape

flush(stderr()); flush(stdout())

### Name: plot.treeshape
### Title: Plot phylogenetic treeshapes.
### Aliases: plot.treeshape
### Keywords: hplot

### ** Examples


## Visual representation of the universal tree of life provided in data
data(universal.treeshape)
plot(universal.treeshape)

## Visual representation of two trees at the same time
data(carnivora.treeshape)
plot(carnivora.treeshape, cutreeshape(carnivora.treeshape, 8, "bottom"))



cleanEx()
nameEx("rtreeshape")
### * rtreeshape

flush(stderr()); flush(stdout())

### Name: rtreeshape
### Title: Generate a list of random binary trees according to a given
###   model
### Aliases: rtreeshape
### Keywords: datagen

### ** Examples


## Summary of a PDA tree with 100 tips:
summary(rtreeshape(n=1, tip.number=100, model="pda")[[1]])
## Summary of a Yule tree with 100 tips:
summary(rtreeshape(n=1, tip.number=100, model="yule")[[1]])
  
## Generate trees with different sizes
trees=rtreeshape(n=2, tip.number=c(10,20), model="yule")
length(trees)
plot(trees[[1]])
plot(trees[[2]])
  
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
Q <- function(n,i) (i==1)

tree=rtreeshape(n=1, tip.number=10, FUN=Q)
plot(tree[[1]])



cleanEx()
nameEx("sackin")
### * sackin

flush(stderr()); flush(stdout())

### Name: sackin
### Title: Compute the Sackin's index of a tree
### Aliases: sackin
### Keywords: univar

### ** Examples


## Index of Sackin of a PDA tree :
tpda<-rtreeshape(1,tip.number=70,model="pda")
tpda<-tpda[[1]]
sackin(tpda,norm="pda")
  
## Histogram of the Sackin's indices for randomly generated Yule trees, 
##      with no normalization
main="Histogram of Sackin's indices for randomly generated Yule trees"
xlab="Sackin's index"
hist(sapply(rtreeshape(300,tip.number=50,model="yule"),FUN=sackin,norm="yule"),
      freq=FALSE, main=main, xlab=xlab)

## Change the size of the trees:
hist(sapply(rtreeshape(300,tip.number=100,model="yule"),FUN=sackin,norm="yule"),
      freq=FALSE, main=main, xlab=xlab)



cleanEx()
nameEx("shape.statistic")
### * shape.statistic

flush(stderr()); flush(stdout())

### Name: shape.statistic
### Title: Computes the log of the likelihood ratio (yule/pda)
### Aliases: shape.statistic
### Keywords: univar

### ** Examples

data(universal.treeshape)
tree <- universal.treeshape
plot(tree)
summary(tree)

likelihood.test(tree,  model = "yule", alternative = "two.sided")
likelihood.test(tree,  model = "pda", alternative = "two.sided")

## Histogram of shape statistics for a list of Yule trees 
##      (may take some time to compute)
main="Histogram of shape statistics"; xlab="shape statistic"	
hist(sapply(rtreeshape(1000,tip.number=100,model="yule"),FUN=shape.statistic,
      norm="yule"), freq=FALSE, main=main, xlab=xlab)

## Does it fit the Gaussian distribution with mean=0 and sd=1 ?
x<-seq(-3,3,by=0.001)
lines(x,dnorm(x))



cleanEx()
nameEx("shift.test")
### * shift.test

flush(stderr()); flush(stdout())

### Name: shift.test
### Title: Testing diversification rate variation in phylogenetic trees
### Aliases: shift.test
### Keywords: htest

### ** Examples

	## Detecting diversification rate variation in bird families (135 tips)
 data(bird.families)
 tree.birds <- as.treeshape(bird.families, model = "yule")
 class(tree.birds) <- "treeshape"
 pv <- sapply(1:135, FUN = function(i) shift.test(tree.birds, i, lambda1 = 1, lambda2 = 100, nrep = 1000, silent = TRUE))

	## Significant shifts detected at nodes = 67 and 78	
 pv[c(67,78)]
 shift.test(tree.birds, node = 67, lambda1 = 1, lambda2 = 100, nrep = 10000, silent = TRUE)
 shift.test(tree.birds, node = 78, lambda1 = 1, lambda2 = 100, nrep = 10000, silent = TRUE)

 	## visualize the shifts
 par(mfrow=c(2,1)) 
 plot(cutreeshape(tree.birds, ancestor(tree.birds, 67) , "bottom"))
 plot(cutreeshape(tree.birds, 78 , "bottom"))




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("smaller.clade.spectrum")
### * smaller.clade.spectrum

flush(stderr()); flush(stdout())

### Name: smaller.clade.spectrum
### Title: Compute the smaller clade spectrum of a tree.
### Aliases: smaller.clade.spectrum
### Keywords: univar

### ** Examples


# computes the log-likelihood for Aldous' model
shape.new <- function(tree){
h <- function(n){sum(1/(1:n))}
mat <- smaller.clade.spectrum(tree)
parent <- mat[,1]
daughter <- mat[,2]
nh.n <- sapply(parent, FUN = function(x){x*h(x-1)} )
s <- sum(log(daughter/parent) + log(1 - daughter/parent) + log(nh.n))
return(s)}
 
# distribution over 200 replicates

tr <- rtreeshape(200, 100, FUN =
function(n,i){if((i>0)&(i<n))return(1/i/(n-i)) else return(0)})
res <- sapply( tr, FUN = shape.new)  
hist(res)


cleanEx()
nameEx("spectrum.treeshape")
### * spectrum.treeshape

flush(stderr()); flush(stdout())

### Name: spectrum.treeshape
### Title: Compute the spectrum of a tree
### Aliases: spectrum.treeshape
### Keywords: univar

### ** Examples


## A random Yule tree with 30 tips
tr<-rtreeshape(n=1,tip.number=30,model="yule")
tr<-tr[[1]]
spectre=spectrum.treeshape(tr)
spectre
  
## Number of cherries of the tree : nrow(tr$merge)==29
spectre[29]



cleanEx()
nameEx("subtree.test")
### * subtree.test

flush(stderr()); flush(stdout())

### Name: subtree.test
### Title: Test the Yule or PDA hypothesis
### Aliases: subtree.test
### Keywords: htest

### ** Examples


## Generate a random pda tree with 50 tips
tr<-rtreeshape(n=1,tip.number=50,model="pda")
tr<-tr[[1]]

## Test the yule hypothesis, using subtrees of size 2 (Cherries), 
##      with the alternative hypothesis "less"
subtree.test(tr,size=2,alternative="less")



cleanEx()
nameEx("summary.treeshape")
### * summary.treeshape

flush(stderr()); flush(stdout())

### Name: summary.treeshape
### Title: Print a summary of an object of class "treeshape"
### Aliases: summary.treeshape
### Keywords: manip

### ** Examples


## Summary of a PDA tree with 100 tips.
summary(rpda(100)) 
## Note that the standard deviation is very large. 

## Summary of a Yule tree with 100 tips.
summary(ryule(100)) 
## The standard deviation under the Yule model is much smaller than under 
##      the PDA model.

## Summary of the HIV tree.
data(hivtree.treeshape)
summary(hivtree.treeshape) 
## The HIV tree is much closer from the Yule model than from the PDA model. 



cleanEx()
nameEx("tipsubtree")
### * tipsubtree

flush(stderr()); flush(stdout())

### Name: tipsubtree
### Title: Extract a subtree that contains pre-specified tip names or
###   labels
### Aliases: tipsubtree
### Keywords: manip

### ** Examples

  
## The universal tree of life provided in the data sets.
data(universal.treeshape)
  
## One might want to extract the tree containing the Animals, the Plants,
##      the Aquifex and the Microsporidia
tree1<-tipsubtree(universal.treeshape,tips=c("Animals", "Aquifex", 
      "Microsporidia", "Plants"))
plot(universal.treeshape, tree1)

## Labels that do not appear in the tree are ignored
tree2<-tipsubtree(universal.treeshape,tips=c("Human", "Animals", "Aquifex", 
      "Microsporidia", "Plants"))
plot(universal.treeshape, tree2)
  
tree3<-tipsubtree(universal.treeshape,tips=c(1,3,7), numeric=TRUE)
plot(universal.treeshape, tree3)



cleanEx()
nameEx("treeshape")
### * treeshape

flush(stderr()); flush(stdout())

### Name: treeshape
### Title: Builds an object of class treeshape
### Aliases: treeshape
### Keywords: datagen

### ** Examples


## Nodes will define the nodes of a five tips tree
nodes<-matrix(nrow=4,ncol=2)
nodes[1,]<-c(-5,-4)
nodes[2,]<-c(1,-1)
nodes[3,]<-c(-3,2)
nodes[4,]<-c(-2,3)

## Now we can build the tree and plot it.
tree1<-treeshape(nodes)
plot(tree1)

## Computation of the sackin index for the tree :
sackin(tree1)

## Label will define the names of the tips
label=c("a", "b", "c", "d", "e")
tree2<-treeshape(nodes, label)
plot(tree1, tree2)



cleanEx()
nameEx("universal.treeshape")
### * universal.treeshape

flush(stderr()); flush(stdout())

### Name: universal.treeshape
### Title: Universal phylogenetic tree of life
### Aliases: universal.treeshape
### Keywords: datasets

### ** Examples


## Example tree in "treeshape" format
data("universal.treeshape") 

## Summary of the tree
summary(universal.treeshape)

## Visual representation of the tree
plot(universal.treeshape)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
