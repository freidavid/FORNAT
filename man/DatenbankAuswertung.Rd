\name{DatenbankAuswertung}
\alias{DatenbankAuswertung}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Datenbankauswertung
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DatenbankAuswertung(input = NULL, output = NULL, combine = F, befischung = NULL, project = NULL, durchgang = NULL, species = NULL, color = T, analyze_all = F, fish_id = F, cutoff = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
The path to input file in excel format, which in this case is the latest version of the fishdatabase.
}
  \item{output}{
The path to store the output files.
}
  \item{combine}{
Default is FALSE. If combine is set to TRUE, all data of one project is combined.
}
  \item{befischung}{
This is used to specify a specific Befischungs-ID to analyse. It also works with multiple IDs, e.g. c(12,25,938:940)
}
  \item{project}{
Analyse all Befischungs-IDs of one project.
}
  \item{durchgang}{
Only analyse a certain durchgang, e.g. durchgang=c(1,2).
}
  \item{species}{
Subset the analysis to a certain species.
}
  \item{color}{
If set to TRUE (default), plots are in color, if set to FALSE, plots are black and white.
}
  \item{analyze_all}{
If set to TRUE, the function analayzez the whole data base (Default is FALSE).
}
  \item{fish_id}{
IF set to TRUE, the points of the plots are labelled with the specific fish-IDs. Default is FALSE..
}
  \item{cutoff}{
Cutoff to detemine the 0 and 0+ categories. Default is 100, which corresponds to 100 mm.
}
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
David Frei
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##To analyse Befischungs-IDs 1,938,939 and 940
auswertung(befischung=c(1,938:940),input=in_file,output=output2)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
