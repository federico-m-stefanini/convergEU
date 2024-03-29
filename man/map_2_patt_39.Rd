% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_2_patt_39.R
\name{map_2_patt_39}
\alias{map_2_patt_39}
\title{Values to patterns}
\usage{
map_2_patt_39(vaMS, vaEU, vaT, remap = FALSE)
}
\arguments{
\item{vaMS}{member state values  sorted in ascending order by time.}

\item{vaEU}{EU values sorted in ascending order by time.}

\item{vaT}{sorted pair of times.}

\item{remap}{is FALSE for the original numerical labelling of patterns
otherwise TRUE to map to old numerical correspondence.}
}
\value{
a number referring to  pattern whose label depends on the
         indicator type as originally produced in the technical report.
}
\description{
Gradients values and Delta2 are mapped to one pattern (string and number).
 See Eurofound 2018 report.
 
 
 In the mapping table within this function
 +1 means greater than zero, 0 means equal to zero,
 -1 means smaller than 0.
 For column EU_vs_MS,
 if  graEU > graMS then  EU_vs_MS = +1;
 if  graEU < graMS then  EU_vs_MS = -1;
 if  graEU == graMS then  EU_vs_MS = 0.
 Code NA is left to indicate not relevant features.
 Further codes are added here from 13 to 18 for parallelism;
 codes 19 and 20 are for crossed lines joining the EU pair and the MS pair.
 Code 21 stands for "to be visually inspected".
}
