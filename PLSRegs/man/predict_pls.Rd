\name{predict_pls}
\alias{predict_pls}
\title{
  Predict Function for PLS Regression
}
\description{
  This function is used to predict the response values for new observations using a fitted PLS regression model.
}
\usage{
predict_pls(object, x_new)
}
\arguments{
  \item{object}{
    A list that represents the fitted PLS regression model. This list must include an element named 'coef' representing the regression coefficients.
  }
  \item{x_new}{
    A matrix of predictors for the new observations.
  }
}
\details{
  The function multiplies the matrix of new predictors by the regression coefficients from the fitted model to produce the predicted response values.
}
\value{
  A vector of predicted response values for the new observations.
}

\author{Sude Gurer, Selay Sila Doner, Mehmet Emin Sahin}


