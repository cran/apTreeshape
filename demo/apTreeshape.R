if(dev.cur() <= 1) get(getOption("device"))()

opar <- par(ask = interactive() &&
            (.Device %in% c("X11", "GTK", "gnome", "windows","quartz")))

##randomly generated Yule tree with 100 leaves and summary
tree1<-ryule(100)
summary(tree1)

##randomly generated PDA tree with 100 leaves and summary
tree2<-rpda(100)
summary(tree2)

##computation of the colless index of 1000 randomly generated PDA trees with 50 leaves. The colless index is normalized. The histogram is plotted.
main = "Colless' indices for randomly generated Yule trees"
xlab = "Colless' indices"
hist(sapply(rtreeshape(500,50,model="yule"),FUN=colless,norm="yule"),freq=FALSE,main=main,xlab=xlab)

##A graphical test on a tree retrieved within the Pandit database.
tree3<-pandit(117)
summary(tree3)
aldous.test(tree3)
 
##the HIV tree
data(hivtree.treeshape)
summary(hivtree.treeshape)
##aldous graphical test of this tree
aldous.test(hivtree.treeshape)
##cut of the top part of the HIV tree and plot
plot(hivtree.treeshape,cutreeshape(hivtree.treeshape,n=158,"top"))
