#' nof1gen: A package for N of 1 clinical trial analysis using Bayesian methods
#'
#' A package for analyzing N of 1 clinical trials
#'
#' An N of 1 trial is a clinical trial in which a single patient is the entire trial, a single case study.
#' The main purpose of this package is to create a method to analyize a standard N of 1 clinical trial.
#' It can fit bayesian versions of linear regression, logistic/ordinal regression, and poisson regression.
#' Package includes number of different plotting tools for visualization.
#'
#' @section Format of input data:
#'
#'The file must contain two parts, the data section and the metadata section.
#'The data section should contain all the data from the outcomes considered
#'during the trial. For each outcome, we will have a list (time series) of the
#' treatment the patient was on at the time followed by a list (time series)
#' of all the data points collected. These two lists should be the same length
#' although different outcomes can have different lengths. The metadata section
#' will then contain the  user_id, the trigger, the design of the trial, the
#' start and end  date (YYYY-MM-DD), followed by the response type of each
#' of the outcomes, either normal, binomial, poisson, or ordinal.
#' The response types must be in the same order as the outcomes in the
#' data section and must be the last pieces of information in the metadata
#' section. Refer to afib_form.json and diet_form.json for two examples
#' in the data-raw folder.
#'
#' @docType package
#' @name nof1gen
NULL

#' @import coda
#' @import ggplot2
#' @import jsonlite
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#' @import MASS
#' @import rjags
#' @import scales
#' @import reshape2
NULL
