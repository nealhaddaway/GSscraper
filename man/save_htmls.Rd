% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_htmls.R
\name{save_htmls}
\alias{save_htmls}
\title{Download multiple results pages as html files}
\usage{
save_htmls(urls, pause = 0.5, backoff = FALSE)
}
\arguments{
\item{urls}{One or more URLs corresponding to pages of Google Scholar search results.}

\item{pause}{Integer specifying the number of seconds to wait between download attempts. The
default value is 4 seconds.}

\item{backoff}{A logical argument (TRUE or FALSE) specifying whether responsive backing-off should be used.
If set to TRUE, the time between calls is varied depending on how long the server takes to respond to the
original request. The responsive back-off time is set to multiple the response time by the `pause` time: i.e.
if the system takes 1.02 seconds to respond and `pause` time is set to 4 seconds, a 4.10 second delay will
be employed before the next call. The default for back-off is `FALSE`.}
}
\value{
HTML files are downloaded as a string object Pause and success messages are printed to the console.
}
\description{
Downloads one or more Google Scholar URLs as html files with a specific wait-time
to avoid IP address blocking.
}
\examples{
\dontrun{
urls <- c('https://www.google.com/search?q=site:www.sei.org+climate&start=0',
          'https://www.google.com/search?q=site:www.sei.org+climate&start=10')
htmls <- save_htmls(urls, pause = 5)
}
}
