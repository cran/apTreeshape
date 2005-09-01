"treebase" <-
function (tree, class="treeshape", quiet=FALSE, model=NULL, p=0.3) {
	
	if (class!="treeshape" & class!="phylo") {
		stop("'class' must be one of 'phylo' or 'treeshape'\n")
	}
	
	if (class(tree)=='list'){
		tree=unlist(tree)
	}
	
	err=options()$show.error.messages
	warn=options()$warn
	options(warn=-1)
	options(show.error.messages=FALSE)
		
      if(inherits(try(open(url("http://www.google.com")), silent = TRUE),
             "try-error")) {
		cat("No connexion available\n")
		return(NULL)
      }	
	options(warn=warn)
	options(show.error.messages=err)
	

	number=0
	res=list()
#Recuperation des arbres :
	
	for (i in 1:length(tree)) {
		URL="http://www.phylo.org/treebase/trees/Tree"
		aux=as.character(tree[i])
		URL = paste(URL, aux, ".nhx",sep="")
		
		text=""
		if (!quiet) {cat("Recuperation de l'arbre", tree[i])}
		
		err=options()$show.error.messages
		warn=options()$warn
		options(warn=-1)
		options(show.error.messages=FALSE)
		text=try(scan(file=URL, what="", quiet=TRUE, comment.char=""), silent=TRUE)
		options(warn=warn)
		options(show.error.messages=err)
		
		if (class(text)!="try-error") {
			if (length(text)==1) {
				text=paste(text, ";", sep="")
				
				text <- strsplit(text, ":")[[1]]
				tmp<-""
				for (i in 1:length(text)) {
					tmp<-paste(tmp, text[i], sep="")
				}
				text <- tmp
				
				text <- strsplit(text, "&apos;")[[1]]
				tmp<-""
				for (i in 1:length(text)) {
					tmp<-paste(tmp, text[i], sep="")
				}
				text <- tmp
				text <- strsplit(text, "&quot;")[[1]]
				tmp<-""
				for (i in 1:length(text)) {
					tmp<-paste(tmp, text[i], sep="")
				}
				text <- tmp
				
				phy=read.tree(text=text)
				if (class=="treeshape") {
					tmp=as.treeshape.phylo(phy, model, p)
					if (identical(tmp, NULL)==FALSE) {
						number=number+1
						res[[number]]=tmp
					}
				} 
				if (class=="phylo") {
					number=number+1
					res[[number]]=phy
				}
			}
		}
		if (!quiet) {cat("  OK\n")}
	}
	
	#Transformation des arbres
	if (number==0){
		if (!quiet) {cat("no tree\n")}
		return(NULL)
	}
	if (number==1) {
		if (!quiet) {cat("1 tree\n")}
		return(res[[1]])
	}
	if (!quiet) {cat(number, "trees\n")}
	return(res)
}

"treebase" <-
function (tree, class="treeshape", quiet=FALSE, model=NULL, p=0.3) {
	
	if (class!="treeshape" & class!="phylo") {
		stop("'class' must be one of 'phylo' or 'treeshape'\n")
	}
	
	if (class(tree)=='list'){
		tree=unlist(tree)
	}
	
	err=options()$show.error.messages
	warn=options()$warn
	options(warn=-1)
	options(show.error.messages=FALSE)
		
      if(inherits(try(open(url("http://www.google.com")), silent = TRUE),
             "try-error")) {
		cat("No connexion available\n")
		return(NULL)
      }	
	options(warn=warn)
	options(show.error.messages=err)
	

	number=0
	res=list()
#Recuperation des arbres :
	
	for (i in 1:length(tree)) {
		URL="http://www.phylo.org/treebase/trees/Tree"
		aux=as.character(tree[i])
		URL = paste(URL, aux, ".nhx",sep="")
		
		text=""
		if (!quiet) {cat("Recuperation de l'arbre", tree[i])}
		
		err=options()$show.error.messages
		warn=options()$warn
		options(warn=-1)
		options(show.error.messages=FALSE)
		text=try(scan(file=URL, what="", quiet=TRUE, comment.char=""), silent=TRUE)
		options(warn=warn)
		options(show.error.messages=err)
		
		if (class(text)!="try-error") {
			if (length(text)==1) {
				text=paste(text, ";", sep="")
				
				text <- strsplit(text, ":")[[1]]
				tmp<-""
				for (i in 1:length(text)) {
					tmp<-paste(tmp, text[i], sep="")
				}
				text <- tmp
				
				text <- strsplit(text, "&apos;")[[1]]
				tmp<-""
				for (i in 1:length(text)) {
					tmp<-paste(tmp, text[i], sep="")
				}
				text <- tmp
				text <- strsplit(text, "&quot;")[[1]]
				tmp<-""
				for (i in 1:length(text)) {
					tmp<-paste(tmp, text[i], sep="")
				}
				text <- tmp
				
				phy=read.tree(text=text)
				if (class=="treeshape") {
					tmp=as.treeshape.phylo(phy, model, p)
					if (identical(tmp, NULL)==FALSE) {
						number=number+1
						res[[number]]=tmp
					}
				} 
				if (class=="phylo") {
					number=number+1
					res[[number]]=phy
				}
			}
		}
		if (!quiet) {cat("  OK\n")}
	}
	
	#Transformation des arbres
	if (number==0){
		if (!quiet) {cat("no tree\n")}
		return(NULL)
	}
	if (number==1) {
		if (!quiet) {cat("1 tree\n")}
		return(res[[1]])
	}
	if (!quiet) {cat(number, "trees\n")}
	return(res)
}

