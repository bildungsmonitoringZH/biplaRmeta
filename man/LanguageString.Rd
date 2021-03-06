% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/languageString-class.R
\name{LanguageString}
\alias{LanguageString}
\title{R6 Class representing a string or a character vector in multiple languages}
\description{
R6 Class representing a string or a character vector in multiple languages

R6 Class representing a string or a character vector in multiple languages
}
\examples{

## ------------------------------------------------
## Method `LanguageString$new`
## ------------------------------------------------

s <- LanguageString$new(de = 'Hallo Welt', en = 'Hello World')
s$list

## ------------------------------------------------
## Method `LanguageString$edit`
## ------------------------------------------------

s <- LanguageString$new(de = 'Hallo Welt')
s$edit(en = 'Hello World')
s$list
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{list}}{The data of the LanguageString in list format}

\item{\code{json}}{The data of the LanguageString in json format}

\item{\code{xml}}{The data of the LanguageString in xml format}

\item{\code{is.na}}{Check whether the LanguageString contains any data}

\item{\code{printf}}{representation of the LanguageString Object}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{LanguageString$new()}}
\item \href{#method-edit}{\code{LanguageString$edit()}}
\item \href{#method-head}{\code{LanguageString$head()}}
\item \href{#method-clear}{\code{LanguageString$clear()}}
\item \href{#method-print}{\code{LanguageString$print()}}
\item \href{#method-clone}{\code{LanguageString$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a LanguageString object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LanguageString$new(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{character arguments, named with two letter language code}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{LanguageString} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{s <- LanguageString$new(de = 'Hallo Welt', en = 'Hello World')
s$list
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-edit"></a>}}
\if{latex}{\out{\hypertarget{method-edit}{}}}
\subsection{Method \code{edit()}}{
Edit the data of a LanguageString object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LanguageString$edit(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Strings named with two letter language code}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{s <- LanguageString$new(de = 'Hallo Welt')
s$edit(en = 'Hello World')
s$list
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-head"></a>}}
\if{latex}{\out{\hypertarget{method-head}{}}}
\subsection{Method \code{head()}}{
Keep only the first elements of the language strings
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LanguageString$head(n = 1L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{(integer) number of elements to keep}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clear"></a>}}
\if{latex}{\out{\hypertarget{method-clear}{}}}
\subsection{Method \code{clear()}}{
Remove any data of the LanguageString object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LanguageString$clear()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print representation of the LanguageString object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LanguageString$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LanguageString$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
