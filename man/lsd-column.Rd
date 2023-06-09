% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsd-column.R
\name{lsd-column}
\alias{lsd-column}
\alias{deb_gather_lsd}
\alias{deb_spread_lsd}
\title{Helpers to create and separate a \code{deb_lsd} column in a data frame}
\usage{
deb_gather_lsd(
  df,
  l = l,
  s = s,
  d = d,
  bases = c(20, 12),
  lsd_col = lsd,
  replace = FALSE
)

deb_spread_lsd(df, lsd = lsd, l_col = l, s_col = s, d_col = d, replace = FALSE)
}
\arguments{
\item{df}{A data frame.}

\item{l}{Pounds column: Unquoted name of a numeric variable corresponding
to the pounds or libra unit. Default is \code{l}.}

\item{s}{Shillings column: Unquoted name of a numeric variable
corresponding to the shillings or solidus unit. Default is \code{s}.}

\item{d}{Pence column: Unquoted name of a numeric variable corresponding to
the pence or denarius unit. Default is \code{d}.}

\item{bases}{Numeric vector of length 2 used to specify the bases for the
solidus or s and denarius or d units. Default is \code{c(20, 12)}, which
conforms to the most widely used system of 1 pound = 20 shillings and
1 shilling = 12 pence.}

\item{lsd_col}{Unquoted name of the \code{deb_lsd} column created by the
function. Default is \code{lsd}.}

\item{replace}{Logical (default \code{FALSE}). When \code{TRUE}, the newly created
column(s) will replace the one(s) used to create it/them.}

\item{lsd}{\code{deb_lsd} column: Unquoted name of a \code{deb_lsd} column.
Default is \code{lsd}.}

\item{l_col}{Unquoted name for the pounds column created by the function.
Default is \code{l}.}

\item{s_col}{Unquoted name for the shillings column created by the function.
Default is \code{s}.}

\item{d_col}{Unquoted name for the pence column created by the function.
Default is \code{d}.}
}
\value{
A data frame with a new \code{deb_lsd} column for \code{deb_gather_lsd()}
or new pounds, shillings, and pence columns for \code{deb_spread_lsd()}.
}
\description{
\itemize{
\item \code{deb_gather_lsd()} creates a \code{deb_lsd} column from separate variables
representing pounds, shillings, and pence values.
\item \code{deb_spread_lsd()} creates separate variables for pounds, shillings,
and pence from a \code{deb_lsd} column.
}
}
\details{
When transcribing historical accounting data by hand, entering the pounds,
shillings, and pence values (lsd) into separate columns is probably the
easiest and least error prone method. The \code{deb_gather_()} and
\code{deb_spread_()} set of functions provide helpers to go back and forth
between this mode of data entry and the use of \code{deb_lsd} and \code{deb_tetra}
vectors within data frames in R. \code{deb_gather_lsd()} creates a \code{deb_lsd}
column from \code{l}, \code{s}, and \code{d} columns representing the three units of
this type of value. \code{deb_spread_lsd()} does the opposite. It takes a
\code{deb_lsd} column and spreads it into three separate pounds, shillings,
and pence columns.

Values for column names (\code{lsd_col}, \code{l_col}, \code{s_col}, and \code{d_col}) must
be valid column names. They can be quoted or unquoted, but they cannot be
vectors or bare numbers. This follows the rules of \code{\link[dplyr:rename]{dplyr::rename()}}.
}
\examples{

libra <- c(3, 5, 6, 2)
solidus <- c(10, 18, 11, 16)
denarius <- c(9, 11, 10, 5)

# data frame with separate l, s, and d variables and default names
x <- data.frame(accounts = c(1, 2, 3, 4),
                l = libra,
                s = solidus,
                d = denarius)

# data frame with deb_lsd variable and default names
y <- data.frame(accounts = c(1, 2, 3, 4),
                lsd = deb_lsd(l = libra,
                              s = solidus,
                              d = denarius))

# Gather l, s, and d variables into deb_lsd column
deb_gather_lsd(x, l = l, s = s, d = d)

# Spread deb_lsd column into separate l, s, and d columns
deb_spread_lsd(y, lsd = lsd)

# Replace original columns with replace = TRUE
deb_gather_lsd(x, replace = TRUE)
deb_spread_lsd(y, replace = TRUE)

# Choose non-default column names
deb_gather_lsd(x, lsd_col = data, replace = TRUE)
deb_spread_lsd(y,
               l_col = libra,
               s_col = solidus,
               d_col = denarius,
               replace = TRUE)

# The two functions are opposites
z <- x \%>\%
  deb_gather_lsd(replace = TRUE) \%>\%
  deb_spread_lsd(replace = TRUE)
all.equal(x, z)

}
\seealso{
\code{\link[=deb_gather_tetra]{deb_gather_tetra()}} and \code{\link[=deb_spread_tetra]{deb_spread_tetra()}} provide the
same functionality for the less common tetrapartite values of pounds,
shillings, pence, and farthings.
}
