
#' Add an element to a given list
#'
#' @param givenList Given list
#' @param elementToAdd Element to add
#' @return New list with added element
#' @export
addElement <- function(givenList, elementToAdd) {

  givenList[[length(givenList) + 1]] <- elementToAdd

  return(givenList)

}


#' Calculate vector of simple percent changes of input vector
#'
#' @param v Given vector
#'
#' @return Vector of simple percent changes
#' @export
calcPercentChanges <- function(v) {
  tail(v, -1) / head(v, -1) - 1
}


#' Calculate vector of log percent changes of input vector
#'
#' @param v Given vector
#'
#' @return Vector of log percent changes
#' @export
calcLogChanges <- function(v) {
  log(tail(v, -1) / head(v, -1))
}


#' Get last element of vector
#'
#' @param v given vector
#'
#' @return last element
#' @export
getLast <- function(v) {
  tail(v, 1)
}


#' Calculate percentage of vector's elements inside given range inclusive
#'
#' @param v given vector
#' @param range given range
#'
#' @return percentage of vector's elements inside range
#' @export
calcPercentInsideInclusive <- function(v, range) {
  length(v[v>=range[1] & v<=range[2]]) / length(v)
}


#' Get indices of max element of matrix
#'
#' @param m given matrix
#'
#' @return indices of max element of matrix
#' @export
getIndicesOfMax <- function(m) {
  which(m == max(m), arr.ind = TRUE)
}


#' Return set of colors for 3d surface
#' Author: ucgamdo@ucl.ac.uk
#'
#' @param z matrix of z-values of the surface
#' @param col palette
#'
#' @return vector of colors for 3d surface
#' @export
getSurfColors <- function(z, col = cm.colors(40)) {

  # First we drop the 'borders' and average the facet corners
  # we need (nx - 1)(ny - 1) facet colours!
  avg <- (z[-1, -1] + z[-1, -(ncol(z) - 1)] +
            z[-(nrow(z) -1), -1] + z[-(nrow(z) -1), -(ncol(z) - 1)]) / 4

  # Now we construct the actual colours matrix
  colors = col[cut(avg, breaks = length(col), include.lowest = T)]

  return (colors)

}


#' Build vector filled with repeating 1, 2, 3...
#'
#' @param periodLength How many times to repeat each number
#' @param numOfPeriods Number of periods
#'
#' @return Vector filled with repeating 1, 2, 3...
#' @export
buildPeriodVector <- function(periodLength, numOfPeriods) {

  v <- numeric()

  for (j in 1:numOfPeriods) {
    for (i in 1:periodLength) {
      v <- append(v, j)
    }
  }

  return (v)

}


#' Aggregate data of data frame summing up given periods
#'
#' @param data Given data frame
#' @param periodLength Length of period
#'
#' @return Aggregated data frame
#' @export
sumPeriod <- function(data, periodLength) {

  numOfPeriods <- trunc(length(data[,1]) / periodLength)
  v <- buildPeriodVector(periodLength, numOfPeriods)

  data <- head(data, periodLength * numOfPeriods)
  data <- aggregate(data, list(v), sum)
  data <- subset(data, select = -Group.1)

  return (data)

}


#' Chart nice histogram
#'
#' @param v Data vector
#' @param numOfBars Number of bars
#' @param color Color of histogram
#' @param xlab Label of x
#' @param main Title
#'
#' @return Histogram
#' @export
chartHist <- function(v, numOfBars = 50, color = rgb(0,0,1,1/2),
                      xlab = deparse(substitute(v)),
                      main = "Frequency") {

  histBreakStep <- (max(v) - min(v))/ numOfBars
  breaks <- seq(min(v) - histBreakStep, max(v) + histBreakStep, histBreakStep)
  hist(v, col = color, breaks = breaks, xlab = xlab, main = main)

}


#' Make a function of two arguments take a vector as a second argument
#'
#' @param f Function
#' @param k First argument as a constant
#' @param v Second argument as a vector
#'
#' @return Vector of outputs
#' @export
KV <- function(f, k, v) {

  l <- length(v)
  y <- numeric(l)

  for (i in 1:l) {
    y[i] <- f(k, v[i])
  }

  return (y)

}


#' Make a function of three arguments take vectors as a second and third arguments
#'
#' @param f Function
#' @param k First argument as a constant
#' @param v1 Second argument as a vector
#' @param v2 Third argument as a vector
#'
#' @return Vector of outputs
#' @export
KVV <- function(f, k, v1, v2) {

  l <- length(v1)
  y <- numeric(l)

  for (i in 1:l) {
    y[i] <- f(k, v1[i], v2[i])
  }

  return (y)

}


#' Make a function of four arguments take vectors as a second, third and fourth arguments
#'
#' @param f Function
#' @param k First argument as a constant
#' @param v1 Second argument as a vector
#' @param v2 Third argument as a vector
#' @param v3 Fourth argument as a vector
#'
#' @return Vector of outputs
#' @export
KVVV <- function(f, k, v1, v2, v3) {

  l <- length(v1)
  y <- numeric(l)

  for (i in 1:l) {
    y[i] <- f(k, v1[i], v2[i], v3[i])
  }

  return (y)

}


#' Make a function of four arguments take a vector as a fourth argument
#'
#' @param f Function
#' @param k First argument as a constant
#' @param v1 Second argument as a constant
#' @param v2 Third argument as a constant
#' @param v3 Fourth argument as a vector
#'
#' @return Vector of outputs
#' @export
KKKV <- function(f, k1, k2, k3, v) {

  l <- length(v)
  y <- numeric(l)

  for (i in 1:l) {
    y[i] <- f(k1, k2, k3, v[i])
  }

  return (y)

}


#' Common switch that takes expression as vector (helper.R)
#'
#' @param expression Expression vector
#' @param ... Cases and results. Results must be same class
#'
#' @return Result of case that matches expression[i]
#' @export
#'
#' @examples
#' vectorSwitch(c("a","c"), a = 1, b = 2, c = 3, 4)
#'
vectorSwitch <- function(expression, ...) {

  result <- NA
  class(result) <- class(list(...)[[1]])

  for (i in 1:length(expression))
    result[i] <- switch(expression[i], ...)

  return (result)

}


#' Switch that takes expression and results as vectors (helper.R)
#'
#' @param expression Expression vector
#' @param ... Cases and vectors of results. Results must be same class
#'
#' @return Result[i] of case that matches expression[i]
#' @export
#'
#' @examples
#' matrixSwitch(c("a","b","c"), a = 1:3, b = 11:13, c = 101:103, NA)
#'
matrixSwitch <- function(expression, ...) {

  result <- NA
  class(result) <- class(list(...)[[1]])

  for (i in 1:length(expression))
    result[i] <- switch(expression[i], ...)[i]

  return (result)

}


#' Return vector of lengths of list elements
#'
#' @param l List
#' @param atoms Vector of class names that lengths are assumed to be 1
#'
#' @return Vector of lengths
#' @export
getLengths <- function(l, atoms = c("atom")) {

  lens <- numeric()

  for (i in 1:length(l)) {
    if (any(class(l[[i]]) == atoms)) {
      lens[i] <- 1
    } else {
      lens[i] <- length(l[[i]])
    }
  }

  return (lens)

}


#' Check that all params have length = 1 or same length > 1
#'
#' @param ... Any number of atoms, vectors, lists
#' @param atoms Vector of class names that lengths are assumed to be 1
#'
#' @return Maximum length of params
#' @export
getLength <- function(..., atoms = c("atom")) {

  lens <- getLengths(list(...), atoms)
  maxLen <- max(lens)

  if (!all(lens == 1 | lens == maxLen))
    stop("ERROR! Vector length mismatch\n")

  return (maxLen)

}


#' Return element "i" of "a" assuming that "a" has infinite tail filled with its last element
#'
#' @param a Any atom, vector, list
#' @param i Index of value
#' @param atoms Vector of class names that cannot be broken apart by index referencing
#'
#' @return Value with index i
#' @export
getByIndex <- function(a, i, atoms = c("atom")) {

  if (any(class(a) == atoms)) {

    b <- a

  } else {

    len <- length(a)

    if (i <= len)
      b <- a[[i]]
    else
      b <- a[[len]]

  }

  return (b)

}


#' Return attribute of object
#'
#' @param obj Object or list of objects
#' @param attrName Attribute name as character
#'
#' @return Attribute of object
#' @export
getAttribute <- function(obj, attrName) {

  if (is.atomic(obj)) {

    a <- NA

  } else {

    if (class(obj) != "list") {

      a <- obj[[attrName]]
      if (is.null(a)) a <- NA

    } else {

      a <- list()
      len <- length(obj)

      for (i in 1:len) {
        a[[i]] <- obj[[i]][[attrName]]
        if (is.null(a[[i]])) a[[i]] <- NA
      }

      if (is.atomic(a[[1]])) a <- do.call("c", a)

    }

  }

  return (a)

}


