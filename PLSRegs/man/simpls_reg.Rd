\name{simpls_reg}
\alias{simpls_reg}
\title{SIMPLS Regression Function}

\description{
  This function creates a Simultaneous Component Analysis for Partial Least Squares regression model to analyze and make predictions about the relationship between dependent and independent variables. SIMPLS selects variables by considering the structural relationships between independent variables. The function is capable of cross-validation and can use this method to determine the optimal number of components.
}

\usage{
  simpls_reg(y, x, n_comp = NULL, nfold = 5, comp_max = 10, cv = TRUE)
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
The function conducts a regression analysis utilizing the SIMPLS algorithm. When 'cv' is set to TRUE, it employs cross-validation to identify the optimal number of components. The function yields the PLS model coefficients, fitted values, and residuals as outputs.
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

model <- simpls_reg(y_train, x_train)
predictions <- predict_pls(model, x_test)

mean((y_test - predictions)^2)
}

