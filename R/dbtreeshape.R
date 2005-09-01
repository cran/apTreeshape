"dbtreeshape" <-
function(db, tree, class="treeshape", type="s", quiet=FALSE, model=NULL, p=0.3) {
	
	if (db=="pandit") {
		return(pandit(tree, class, type, quiet, model, p))
	}
	if (db=="treebase") {
		return(treebase(tree, class, quiet, model, p))
	}
	stop("wrong argument for parameter db")

}

