\name{bidiag1_reg}
\alias{bidiag1_reg}
\title{Bidiag1 Regression Function}

\description{
  This function performs two-way regression with optional cross-validation to determine the optimal number of components for the regression model.
}

\usage{
  bidiag1_reg(y, x, n_comp = NULL, nfold = 5, comp_max = 10, cv = TRUE)
}

\arguments{
  \item{x}{A matrix containing independent variables in its columnts.}
  \item{y}{A vector or matrix containing the observationsof dependent variable.}
  \item{n_comp}{An integer value defining the number of components to be used in the PLS   regression model. The default value is NULL. If NULL, then, its optimum value is determine by
  an n-fold cross-validation algorithm.}
  \item{nfold}{An integer defining the number of folds used in the cross-validation. The default value is 5.}
  \item{comp_max}{An integer defining the maximum number of components. The default value is 10.}
  \item{cv}{Logical. If TRUE, cross-validation is performed; otherwise, FALSE. The default value is TRUE.}
}

\details{
 The bidiag1_reg function is a regression function that utilizes the bidiagonalization process. It applies the bidiag1 function to perform bidiagonalization and then fits a model to the data. The function can either use a predetermined number of components for the regression (specified by the n_comp parameter), or n-fold cross-validation can be used to determine the optimum value of the PLS components.
}

\value{
  Returns a list object with the following components:
  \item{coef}{A matrix containing the regression coefficients.}
  \item{fitted.values}{A column matrix containing fitted values.}
  \item{residuals}{A vector containing residuals.}
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

