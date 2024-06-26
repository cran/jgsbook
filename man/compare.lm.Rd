% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{compare.lm}
\alias{compare.lm}
\title{Compare Linear Models}
\usage{
compare.lm(dep, ind, predict = FALSE, steps = 0.01)
}
\arguments{
\item{dep}{A numeric vector representing the dependent variable.}

\item{ind}{A numeric vector representing the independent variable.}

\item{predict}{Logical. If TRUE, the function returns predicted values for each model. Defaults to FALSE.}

\item{steps}{Numeric. The step size for generating x-values for predictions. Only used if predict is TRUE. Defaults to 0.01.}
}
\value{
A data frame. If predict is FALSE, returns a data frame with the R-squared values for each model. If predict is TRUE, returns a data frame with the original data and predicted values for each model.
}
\description{
This function fits and compares several models (linear, quadratic, cubic, exponential, logarithmic, sigmoidal, power, logistic) to a given set of dependent and independent variables. It returns either a summary of the models with their R-squared values or predicted values based on the models.
}
\examples{
x <- c(6, 9, 12, 14, 30, 35, 40, 47, 51, 55, 60)
y <- c(14, 28, 50, 70, 89, 94, 90, 75, 59, 44, 27)
compare.lm(y, x)
compare.lm(y, x, predict=TRUE)

}
