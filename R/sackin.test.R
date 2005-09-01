"sackin.test" <-
function(tree,model="yule",alternative="less",n.mc=500) {
	
	#defender 
	if (class(tree)!='treeshape') {
		stop("invalid arguments")
	}
	
	if ((model != "yule") && (model!="pda")) {
		stop("Argument 'model' is incorrect. Should be a string 'pda' or 'yule'")
	}
	
	tip.number.mc<-nrow(tree$merge)+1
	
	trees.mc<-rtreeshape(n=n.mc,tip.number=tip.number.mc,model=model)
	
	lind.mc<-sapply(trees.mc,FUN=sackin,norm=NULL)
	
	res<-ecdf(lind.mc) 
	
	m<-mean(lind.mc)
	#resbis<-quantile(lind.mc,probs=seq(0,1,length=101))
	
	
	stat<-sackin(tree,norm=NULL)
	cat("	Test of the ")
	cat(model)
	cat(" hypothesis using the Sackin index ","\n")
	cat("Statistic = ")
	cat(sackin(tree), "\n")
	cat("Standardized Statistic = ")
	cat(stat,"\n")
	cat("p-value = ")
	if (alternative == "less") {
		p.value<-res(stat)
		cat(p.value,"\n")
		cat("alternative hypothesis: the tree is more balanced than predicted by the ")
		cat(model)
		cat(" model","\n") 
	}
	else if (alternative == "greater") {
		p.value<-1-res(stat)
		cat(p.value,"\n")	
		cat("alternative hypothesis: the tree is less balanced than predicted by the ")
		cat(model)
		cat(" model","\n") 
	}
	else {
		cat("\n")
		stop("Argument 'alternative' is incorrect. Should be a string 'less' or 'greater'")
	}
	cat("\n")
	cat("Note : the p-value was computed using a Monte-Carlo method")
	cat("\n")
	
	res<-list(model=model,statistic = stat,p.value = p.value, alternative = alternative)
	
}

