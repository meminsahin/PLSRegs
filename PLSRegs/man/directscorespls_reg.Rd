\name{directscorespls_reg}
\alias{directscorespls_reg}
\title{Directscores Regression Function}

\description{
This function conducts Direct Scores Partial Least Squares Regression. It determines the optimal number of components through cross-validation (if enabled), computes regression coefficients, generates predictions, and provides error metrics.
}

\usage{
  directscorespls_reg(y, x, n_comp = NULL, nfold = 5, comp_max = 10, cv = TRUE)
}

\arguments{
  \item{x}{A Matrix of independent variables, where each column represents an independent variable.}
  \item{y}{A Vector or matrix of dependent variables.}
  \item{n_comp}{An integer value defining the number of components to be used in the PLS   regression model. The default value is NULL. If NULL, then, its optimum value is determine by
  an n-fold cross-validation algorithm.}
  \item{nfold}{An integer defining the number of folds used in the cross-validation. The default value is 5.}
  \item{comp_max}{An integer defining the maximum number of components. The default value is 10.}
  \item{cv}{A logical item. If TRUE, cross-validation is performed; otherwise, the value n_fold provided by the users is used to perform the regression model.}
}

\details{
The function utilizes the Direct Scores Partial Least Squares (PLS) algorithm for regression analysis. It can either conduct cross-validation to determine the optimal number of components or utilize a fixed number of components specified by the user.
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

model <- directscorespls_reg(y_train, x_train)
predictions <- predict_pls(model, x_test)

mean((y_test - predictions)^2)
}

