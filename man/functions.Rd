\name{aScriptDisplayDialog}
\alias{aScriptChooseFromList}
\alias{aScriptDialogs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AppleScript dialogs for R}
\description{
A simple package for interfacing with AppleScript dialogs.
}
\usage{
aScriptDisplayDialog(
	text,
	defaultAnswer=NA,
	hiddenAnswer=FALSE,
	buttons=NA,
	defaultButton=NA,
	cancelButton=NA,
	withTitle=NA,
	withIcon=NA,
	givingUpAfter=NA
)

aScriptChooseFromList(
	listOfItems,
	withTitle=NA,
	withPrompt=NA,
	defaultItems=NA,
	OKbuttonName=NA,
	cancelButtonName=NA,
	multipleSelectionsAllowed=FALSE,
	emptySelectionAllowed=FALSE
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  These arguments and defaults are all standard in AppleScript. Their documentation can be found at \href{https://developer.apple.com/library/archive/documentation/LanguagesUtilities/Conceptual/MacAutomationScriptingGuide/DisplayDialogsandAlerts.html#//apple_ref/doc/uid/TP40016239-CH15-SW1}{https://developer.apple.com/} (click the link; it'll take you right to the spot).
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
aScriptDialogs::aScriptChooseFromList(
	listOfItems=c("a","b","c",1:3),
	withTitle="Hello world!",
	withPrompt="Right here:",
	defaultItems=c("b",5),
	OKbuttonName="a",
	cancelButtonName="c",
	multipleSelectionsAllowed=TRUE,
	emptySelectionAllowed=TRUE
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
