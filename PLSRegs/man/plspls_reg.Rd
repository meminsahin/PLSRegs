\name{plspls_reg}
\alias{plspls_reg}
\title{PLS-PLS Regression Function}

\description{
  This function creates a Partial Least Squares (PLS) regression model to analyze and make predictions about the relationship between dependent and independent variables. PLS-PLS selects variables by considering the structural relationships between independent variables. The function is capable of cross-validation and can use this method to determine the optimal number of components.
}

\usage{
  plspls_reg(y, x, n_comp = NULL, nfold = 5, comp_max = 10, cv = TRUE)
}

\arguments{
  \item{x}{A Matrix of independent variables.}
  \item{y}{A Vector or matrix of dependent variables.}
  \item{n_comp}{An integer value defining the number of components to be used in the PLS   regression model. The default value is NULL. If NULL, then, its optimum value is determine by
  an n-fold cross-validation algorithm.}
  \item{nfold}{An integer defining the number of folds used in the cross-validation. The default value is 5.}
  \item{comp_max}{An integer defining the maximum number of components. The default value is 10.}
  \item{cv}{Logical. If TRUE, cross-validation is performed; otherwise, FALSE. The default value is TRUE.}
}

\details{
  The function performs PLS regression using a method of successive orthogonalization of column vectors. The predictor and response variables should be supplied as matrices, and the number of components to be used in the regression can be optionally specified. If not specified, the number of components is determined by cross-validation.
}

\value{
  Returns a list with the following components:
  \item{coef}{A Matrix containing regression coefficients.}
  \item{fitted.values}{A Matrix containing predicted values.}
  \item{residuals}{A Vector containing residuals.}
}

\references{M. Andersson (2009), "A comparison of nine PLS1 algorithms", Journal of Chemometrics, 23(10), 518-529.}

\author{Sude Gurer, Selay Sila Doner, Mehmet Emin Sahin}
\examples{
library(fds)
data("Octanespectrum")
data("Octanevalues")

y <- Octanevalues
x <- t(Octanespectrum$y)

n_train <- 40
n_test <- 20

train_index <- sample(1:(n_train+n_test), n_train)

y_train <- y[train_index]
y_test <- y[-train_index]

x_train <- x[train_index,]
x_test <- x[-train_index,]

model <- bidiag1_reg(y_train, x_train)
predictions <- predict_pls(model, x_test)

mean((y_test - predictions)^2)
}

