# Utility function which saves all stanfit or shinystan objects in the working memory to a .RData file with today's date. Optionally add a suffix to describe this set of models

savestan <- function(suffix=NULL) {
  
  tosave <- which(
		sapply(ls(envir=.GlobalEnv), function(x) class(get(x)))
				=="stanfit" |
		  sapply(ls(envir=.GlobalEnv), function(x) class(get(x)))
		=="shinystan" 
		  )
  suffix = paste("", suffix, collapse = "")
	save(file=paste("Stan Output ", Sys.Date(), suffix, ".RData", sep=""), list = ls(envir=.GlobalEnv)[tosave])
	
	} 