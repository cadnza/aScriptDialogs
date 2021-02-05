.numericRegex <- "^\\d*(\\.\\d*)?$"

.esc <- function(x,doubleQuotes=TRUE){
	if(is.na(x))
		stop("Can't escape NA")
	if(is.logical(x))
		return(tolower(x))
	if(!nchar(x))
		return("\"\"")
	if(!grepl(.numericRegex,x))
		return(
			paste0(
				"\"",
				ifelse(
					doubleQuotes,
					gsub("\\\"","\\\\\"",x),
					gsub("'","\\\\'",x)
				),
				"\""
			)
		)
	else
		return(x)
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
	wrapped <- paste0(
		"osascript -e '",
		.esc(command,FALSE),
		"'"
	)
	return(wrapped) #TEMP
	# Run command, check for errors, and return formatted output # Maybe outsource to function? #TEMP
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
	# Restrict to macOS #TEMP
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
	command <- paste(command[!is.na(command)],collapse=" ")
	command <- trimws(gsub("  *"," ",command))
	final <- .runInOSAscript(command)
	return(final)
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
	# Restrict to macOS #TEMP
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
	command <- paste(command[!is.na(command)],collapse=" ")
	command <- trimws(gsub("  *"," ",command))
	final <- .runInOSAscript(command)
	return(final)
}

cat( # Testing #TEMP
	aScriptChooseFromList(
		listOfItems=c("a","b","c",1:3),
		withTitle="oranges are my friends",
		withPrompt="",
		defaultItems=c("hello",5),
		OKbuttonName="bru\"ce",
		cancelButtonName="he'nna",
		multipleSelectionsAllowed=TRUE,
		emptySelectionAllowed=TRUE
	)
)
