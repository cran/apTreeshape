"as.treeshape.phylo" <-
function (x, model=NULL, p, ...) {

	phy=x
	
	if (identical(model, NULL)) {convertion=FALSE}
	else {convertion=TRUE}
	
	if (missing(p)) {p=0.3}
	
	bin=is.binary.phylo(phy)
	randomize=FALSE
	if (!(identical(bin,TRUE))){
		if (!convertion) {return(NULL)}
		else {
			randomize=TRUE
			for (i in nrow(bin):1) {
				new.nodes=bin[i,2]-2
				
				for (j in 1:length(phy$edge)) {
					if (as.numeric(phy$edge[j])<(-bin[i,1]))
						{phy$edge[j]=as.numeric(phy$edge[j])-new.nodes}
				}
				if (model=="pda") {tmp=rpda(bin[i,2])}
				if (model=="yule") {tmp=ryule(bin[i,2])}
				if (model=="biased") {tmp=rbiased(bin[i,2],p)}
				
				tmp=as.phylo(tmp)
				
				for (j in 1:length(tmp$edge)) {
					if (as.numeric(tmp$edge[j])<0) {
						tmp$edge[j]<-as.numeric(tmp$edge[j])+1-bin[i,1]
					}
				}
				tmp$edge[as.numeric(tmp$edge)>0] <- phy$edge[phy$edge[,1]==(-bin[i,1]),2]
				
				current.line=new.nodes+1
				m=matrix(nrow=nrow(phy$edge)+new.nodes, ncol=2)
				
				m[1:new.nodes,]=tmp$edge[1:new.nodes,]
				for(line in 1:nrow(phy$edge)) {
					if (as.numeric(phy$edge[line,1])==(-bin[i,1])) {
						m[line+new.nodes,]=tmp$edge[current.line,]
						current.line=current.line+1
					} else {
						m[line+new.nodes,]=phy$edge[line,]
					}
				}
				phy$edge=m
				
			}
		}
	}
	
	phy$edge[,1]=-as.numeric(phy$edge[,1])
	phy$edge[,2]=-as.numeric(phy$edge[,2])
	height <- (nrow(phy$edge)/2)
	merge <- matrix(0, ncol=2, nrow=height)
	current.node=1
	nodes <- height:1
	
	for (i in nrow(phy$edge):1) {
		if (as.numeric(phy$edge[i,2])<0) {
			tmp=phy$edge[i,2]
		} else {
			tmp=nodes[as.numeric(phy$edge[i,2])]
		}
		if (merge[nodes[as.numeric(phy$edge[i])],2]==0) {
			merge[nodes[as.numeric(phy$edge[i])],2]=as.numeric(tmp)
		} else {
			merge[nodes[as.numeric(phy$edge[i])],1]=as.numeric(tmp)
		}
		
	}
	
# 	res <- list(merge=nodes, names=phy$tip.label)
# 	class(res)<-'treeshape'
	res=treeshape(merge, phy$tip.label)
	if (randomize){class(res)=c('treeshape', 'randomized-tree')}
	
	res
}

