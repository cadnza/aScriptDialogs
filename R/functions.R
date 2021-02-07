.esc <- function(x,doubleQuotes=TRUE,wrapInDouble=TRUE){
	if(is.na(x))
		stop("Can't escape NA")
	if(is.logical(x))
		return(tolower(x))
	if(!nchar(x))
		return("\"\"")
	if(!is.numeric(x)&!is.integer(x))
		return(
			paste0(
				if(wrapInDouble) "\"",
				ifelse(
					doubleQuotes,
					gsub("\\\"","\\\\\"",x),
					gsub("'","'\\\\''",x)
				),
				if(wrapInDouble) "\""
			)
		)
	else
		return(x)
}

.formatCommand <- function(command){
	command <- paste(command[!is.na(command)],collapse=" ")
	command <- trimws(gsub("  *"," ",command))
	return(command)
}

.vectorToAscriptList <- function(v){
	if(class(v)=="list")
		stop("Please supply a vector instead of a list.")
	if(!is.null(names(v)))
		stop("Please supply an unnamed vector.")
	esced <- sapply(v,.esc)
	final <- paste0(
		"{",
		paste(esced,collapse=","),
		"}"
	)
	return(final)
}

.macOScheck <- function()
	if(unname(Sys.info()["sysname"])!="Darwin")
		stop("aScriptDialogs only works on macOS. Sorry!")

.runInOSAscript <- function(command){
	generateRandomString <- function()
		gsub("\\.","x",paste(abs(rnorm(10)),collapse=""))
	separator <- generateRandomString()
	separatorName <- generateRandomString()
	command <- c(paste("set vals to",command))
	command <- c(
		command,
		paste0("set separator to \"",separator,"\""),
		paste0("set separatorName to \"",separatorName,"\""),
		"set tagText to \"text\"",
		"set tagButton to \"button\"",
		"if class of vals is record then",
		"try",
		paste(
			"set vals to {",
			"tagText & separatorName & text returned of vals,",
			"tagButton & separatorName & button returned of vals",
			"}"
		),
		"end try",
		"try",
		"set vals to tagText & separatorName & text returned of vals",
		"end try",
		"try",
		"set vals to tagButton & separatorName & button returned of vals",
		"end try",
		"end if",
		"if vals is true or vals is false then return vals",
		"set package to \"\"",
		"if (count of vals) > 1 and class of vals is list then",
		"repeat with val in vals",
		"set package to package & val & separator",
		"end repeat",
		"else",
		"set package to vals",
		"end if",
		"return package"
	)
	command <- paste(command,collapse="\n")
	wrapped <- paste0(
		"osascript -e '",
		.esc(command,FALSE,FALSE),
		"'"
	)
	tryCatch(
		{
			final <- system(wrapped,intern=TRUE)
		},
		warning=function(x)
			stop(x),
		error=function(x)
			stop(x)
	)
	if(final=="true")
		return(TRUE)
	if(final=="false")
		return(FALSE)
	final <- strsplit(final,separator)[[1]]
	if(all(grepl(separatorName,final))){
		items <- strsplit(final,separatorName)
		final <- sapply(items,function(x) x[[2]])
		names(final) <- sapply(items,function(x) x[[1]])
	}
	return(final)
}

aScriptDisplayDialog <- function(
	text,
	defaultAnswer=NA,
	hiddenAnswer=FALSE,
	buttons=NA,
	defaultButton=NA,
	cancelButton=NA,
	withTitle=NA,
	withIcon=NA,
	givingUpAfter=NA
){
	.macOScheck()
	command <- c(
		"display dialog",
		.esc(text),
		ifelse(
			is.na(defaultAnswer),
			NA,
			paste("default answer",.esc(defaultAnswer))
		),
		ifelse(
			is.na(hiddenAnswer),
			NA,
			paste("hidden answer",.esc(hiddenAnswer))
		),
		ifelse(
			all(is.na(buttons)),
			NA,
			paste("buttons",.vectorToAscriptList(buttons))
		),
		ifelse(
			is.na(defaultButton),
			NA,
			paste("default button",.esc(defaultButton))
		),
		ifelse(
			is.na(cancelButton),
			NA,
			paste("cancel button",.esc(cancelButton))
		),
		ifelse(
			is.na(withTitle),
			NA,
			paste("with title",.esc(withTitle))
		),
		ifelse(
			is.na(withIcon),
			NA,
			paste(
				"with icon",
				ifelse(
					withIcon%in%c("stop","note","caution"),
					withIcon,
					.esc(withIcon)
				)
			)
		),
		ifelse(
			is.na(givingUpAfter),
			NA,
			paste("giving up after",givingUpAfter)
		)
	)
	command <- .formatCommand(command)
	osa <- .runInOSAscript(command)
	return(osa)
}

aScriptDisplayAlert <- function(
	text,
	message=NA,
	as=NA,
	buttons=NA,
	defaultButton=NA,
	cancelButton=NA,
	givingUpAfter=NA
){
	.macOScheck()
	allowedAs <- c("critical","informational","warning")
	if(!is.na(as))
		if(!as%in%allowedAs)
			stop(
				paste(
					"The `as` argument must be one of the following:",
					paste(allowedAs,collapse="\n"),sep="\n"
				)
			)
	command <- c(
		"display alert",
		.esc(text),
		ifelse(
			is.na(message),
			NA,
			paste("message",.esc(message))
		),
		ifelse(
			is.na(as),
			NA,
			paste("as",as)
		),
		ifelse(
			all(is.na(buttons)),
			NA,
			paste("buttons",.vectorToAscriptList(buttons))
		),
		ifelse(
			is.na(defaultButton),
			NA,
			paste("default button",.esc(defaultButton))
		),
		ifelse(
			is.na(cancelButton),
			NA,
			paste("cancel button",.esc(cancelButton))
		),
		ifelse(
			is.na(givingUpAfter),
			NA,
			paste("giving up after",givingUpAfter)
		)
	)
	command <- .formatCommand(command)
	osa <- .runInOSAscript(command)
	return(osa)
}

aScriptChooseFromList <- function(
	listOfItems,
	withTitle=NA,
	withPrompt=NA,
	defaultItems=NA,
	OKbuttonName=NA,
	cancelButtonName=NA,
	multipleSelectionsAllowed=FALSE,
	emptySelectionAllowed=FALSE
){
	.macOScheck()
	command <- c(
		"choose from list",
		.vectorToAscriptList(listOfItems),
		ifelse(
			is.na(withTitle),
			NA,
			paste("with title",.esc(withTitle))
		),
		ifelse(
			is.na(withPrompt),
			NA,
			paste("with prompt",.esc(withPrompt))
		),
		ifelse(
			all(is.na(defaultItems)),
			NA,
			paste("default items",.vectorToAscriptList(defaultItems))
		),
		ifelse(
			is.na(OKbuttonName),
			NA,
			paste("OK button name",.esc(OKbuttonName))
		),
		ifelse(
			is.na(cancelButtonName),
			NA,
			paste("cancel button name",.esc(cancelButtonName))
		),
		ifelse(
			!multipleSelectionsAllowed,
			NA,
			paste("multiple selections allowed",.esc(multipleSelectionsAllowed))
		),
		ifelse(
			!emptySelectionAllowed,
			NA,
			paste("empty selection allowed",.esc(emptySelectionAllowed))
		)
	)
	command <- .formatCommand(command)
	osa <- .runInOSAscript(command)
	return(osa)
}
