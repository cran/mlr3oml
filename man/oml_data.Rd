% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OMLData.R
\name{oml_data}
\alias{oml_data}
\alias{OMLData}
\title{Interface to OpenML Data Sets}
\description{
This is the class for data sets served on \href{https://www.openml.org/search?type=data&status=active}{OpenML}.
This object can also be constructed using the sugar function \code{\link[=odt]{odt()}}.
}
\section{mlr3 Integration}{

\itemize{
\item A \link[mlr3:Task]{mlr3::Task} can be obtained by calling \code{\link[mlr3:as_task]{mlr3::as_task()}}.
The target column must either be the default target (this is the default behaviour) or one of \verb{$feature_names}.
In case the target is specified to be one of \verb{$feature_names}, the default target is added to the features
of the task.
\item A \link[mlr3:DataBackend]{mlr3::DataBackend} can be obtained by calling \code{\link[mlr3:as_data_backend]{mlr3::as_data_backend()}}. Depending on the
selected file-type, the returned backend is a \link[mlr3:DataBackendDataTable]{mlr3::DataBackendDataTable} (arff) or
\link[mlr3db:DataBackendDuckDB]{mlr3db::DataBackendDuckDB} (parquet).
Note that a converted backend can contain columns beyond the target and the features (id column or ignore columns).
}
}

\section{Name conversion}{

Column names that don't comply with R's naming scheme are renamed (see \code{\link[base:make.names]{base::make.names()}}).
This means that the names can differ from those on OpenML.
}

\section{File Format}{

The datasets stored on OpenML are either stored as (sparse) ARFF or parquet.
When creating a new \code{OMLData} object, the constructor argument \code{parquet} allows to switch
between arff and parquet. Note that not necessarily all data files are available as parquet.
The option \code{mlr3oml.parquet} can be used to set a default.
If \code{parquet} is \code{TRUE} but not available, \code{"arff"} will be used as a fallback.
}

\section{ARFF Files}{

This package comes with an own reader for ARFF files, based on \code{\link[data.table:fread]{data.table::fread()}}.
For sparse ARFF files and if the \CRANpkg{RWeka} package is installed, the reader
automatically falls back to the implementation in (\code{\link[RWeka:read.arff]{RWeka::read.arff()}}).
}

\section{Parquet Files}{

For the handling of parquet files, we rely on \CRANpkg{duckdb} and \CRANpkg{DBI}.
}

\examples{
# For technical reasons, examples cannot be included in this R package.
# Instead, these are some relevant resources:
#
# Large-Scale Benchmarking chapter in the mlr3book:
# https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html
#
# Package Article:
# https://mlr3oml.mlr-org.com/articles/tutorial.html
}
\references{
Vanschoren J, van Rijn JN, Bischl B, Torgo L (2014).
\dQuote{OpenML.}
\emph{ACM SIGKDD Explorations Newsletter}, \bold{15}(2), 49--60.
\doi{10.1145/2641190.2641198}.
}
\section{Super class}{
\code{\link[mlr3oml:OMLObject]{mlr3oml::OMLObject}} -> \code{OMLData}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{qualities}}{(\code{data.table()})\cr
Data set qualities (performance values), downloaded from the JSON API response and
converted to a \code{\link[data.table:data.table]{data.table::data.table()}} with columns \code{"name"} and \code{"value"}.}

\item{\code{tags}}{(\code{character()})\cr
Returns all tags of the object.}

\item{\code{parquet}}{(\code{logical(1)})\cr
Whether to use parquet.}

\item{\code{data}}{(\code{data.table()})\cr
Returns the data (without the row identifier and ignore id columns).}

\item{\code{features}}{(\code{data.table()})\cr
Information about data set features (including target), downloaded from the JSON API response and
converted to a \code{\link[data.table:data.table]{data.table::data.table()}} with columns:
\itemize{
\item \code{"index"} (\code{integer()}): Column position.
\item \code{"name"} (\code{character()}): Name of the feature.
\item \code{"data_type"} (\code{factor()}): Type of the feature: \code{"nominal"} or \code{"numeric"}.
\item \code{"nominal_value"} (\code{list()}): Levels of the feature, or \code{NULL} for numeric features.
\item \code{"is_target"} (\code{logical()}): \code{TRUE} for target column, \code{FALSE} otherwise.
\item \code{"is_ignore"} (\code{logical()}): \code{TRUE} if this feature should be ignored.
Ignored features are removed automatically from the data set.
\item \code{"is_row_identifier"} (\code{logical()}): \code{TRUE} if the column encodes a row identifier.
Row identifiers are removed automatically from the data set.
\item \code{"number_of_missing_values"} (\code{integer()}): Number of missing values in the column.
}}

\item{\code{target_names}}{(\code{character()})\cr
Name of the default target, as extracted from the OpenML data set description.}

\item{\code{feature_names}}{(\code{character()})\cr
Name of the features, as extracted from the OpenML data set description.}

\item{\code{nrow}}{(\code{integer()})\cr
Number of observations, as extracted from the OpenML data set qualities.}

\item{\code{ncol}}{(\code{integer()})\cr
Number of features (including targets), as extracted from the table of data set features.
This excludes row identifiers and ignored columns.}

\item{\code{license}}{(\code{character()})\cr
Returns all license of the dataset.}

\item{\code{parquet_path}}{(\code{character()})\cr
Downloads the parquet file (or loads from cache) and returns the path of the parquet file.
Note that this also normalizes the names of the parquet file.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-OMLData-new}{\code{OMLData$new()}}
\item \href{#method-OMLData-print}{\code{OMLData$print()}}
\item \href{#method-OMLData-download}{\code{OMLData$download()}}
\item \href{#method-OMLData-quality}{\code{OMLData$quality()}}
\item \href{#method-OMLData-clone}{\code{OMLData$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3oml" data-topic="OMLObject" data-id="help"><a href='../../mlr3oml/html/OMLObject.html#method-OMLObject-help'><code>mlr3oml::OMLObject$help()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OMLData-new"></a>}}
\if{latex}{\out{\hypertarget{method-OMLData-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OMLData$new(
  id,
  parquet = parquet_default(),
  test_server = test_server_default()
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{integer(1)})\cr
OpenML id for the object.}

\item{\code{parquet}}{(\code{logical(1)})\cr
Whether to use parquet instead of arff.
If parquet is not available, it will fall back to arff.
Defaults to value of option \code{"mlr3oml.parquet"} or \code{FALSE} if not set.}

\item{\code{test_server}}{(\code{character(1)})\cr
Whether to use the OpenML test server or public server.
Defaults to value of option \code{"mlr3oml.test_server"}, or \code{FALSE} if not set.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OMLData-print"></a>}}
\if{latex}{\out{\hypertarget{method-OMLData-print}{}}}
\subsection{Method \code{print()}}{
Prints the object.
For a more detailed printer, convert to a \link[mlr3:Task]{mlr3::Task} via \code{as_task()}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OMLData$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OMLData-download"></a>}}
\if{latex}{\out{\hypertarget{method-OMLData-download}{}}}
\subsection{Method \code{download()}}{
Downloads the whole object for offline usage.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OMLData$download()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OMLData-quality"></a>}}
\if{latex}{\out{\hypertarget{method-OMLData-quality}{}}}
\subsection{Method \code{quality()}}{
Returns the value of a single OpenML data set quality.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OMLData$quality(name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{(\code{character(1)})\cr
Name of the quality to extract.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OMLData-clone"></a>}}
\if{latex}{\out{\hypertarget{method-OMLData-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OMLData$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
