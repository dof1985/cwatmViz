% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circles.R
\name{buildCircle}
\alias{buildCircle}
\title{Extract required CWatM circle variables}
\usage{
buildCircle(
  circles = NULL,
  flux = "hydro",
  dataPath = "./",
  modflow = FALSE,
  wastewater = FALSE,
  limitAbstr = TRUE,
  outlet = NULL,
  spatial = NULL,
  timeCons = NULL,
  annualAverage = FALSE,
  printBalance = FALSE,
  loud = FALSE
)
}
\arguments{
\item{circles}{A `character` vector with circle names. See `getCodes()`, (default: `NULL`, so all circles are included).}

\item{flux}{A `character` vector with a flux code. See `getFluxes()`, (default: `hydro`, so only water-circles are included).}

\item{dataPath}{A `character` string, defines the location of the simulation outputs.}

\item{modflow}{A `logical` variable indicating if `modflow` simulation was included (default: `FALSE`).}

\item{wastewater}{A `logical` variable indicating if the `wastewater` module was included (default: `FALSE`).}

\item{limitAbstr}{A `logical` variable indicating if `limitAbstraction` was set to `TRUE`, (default: `FALSE`).}

\item{outlet}{A `numeric` vector defined (x, y) coordinate of the outlet, (default: `NULL`, only required in some circles).}

\item{spatial}{A `data.frame` with  (x, y) coordinate to create a circle for specific grid-cells (e.g., lake), or a boolean `RasterLayer` mask, (default: `NULL`).}

\item{timeCons}{A `Date` vector with a (start_date, end_date) for temporal sub-setting, (default: `NULL`).}

\item{annualAverage}{A `logical`. If set to `TRUE` the circles show annual average values, (default: `FALSE`).}

\item{printBalance}{A `logical`. If set to `TRUE` the circles' `data.frame` are printed, (default: `FALSE`).}

\item{loud}{A `logical`. If set to `TRUE` the function prints the names of all loaded files, (default: `FALSE`).}
}
\value{
a comma-separated `character` string with all required output variables.
}
\description{
The function provide a string with all  CWatM output variables, required to create  selected circles, given specified settings.
}
