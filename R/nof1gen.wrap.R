# formated_read_input <- function(data, metadata) { outcome_names <- names(data)
# output <- data # chaning binomials to 0, 1 for (i in 1:length(outcome_names)) {
# if (metadata[[length(metadata) - length(outcome_names) + i]] == 'binomial') {
# fixed <- unlist(data[, i]$result) if (all(fixed %in% c(0, 1, NA))) {
# output[[outcome_names[i]]]$result = list(fixed) } else { max <- max(fixed,
# na.rm = TRUE) fixed <- ifelse(max > fixed & (!is.nan(fixed)), 0, 1)
# output[[outcome_names[i]]]$result = list(fixed) } } } output }

#' Function to read in the data
#'
#' A function that allows us to read in the data and fix any binomial
#' inputs to be 0 and 1.
#'
#' @param data The data file we want to format. This is simply a data frame with
#' outcomes. We can input either a single outcome or multiple outcomes. The outcomes
#' each have a treatment and a result vector.
#' @param response_type The response type of the data. Can be a list of strings
#' if we have several outcomes to examine or a single string.
#' @return The function returns the formated data.
formated_read_input <- function(data, response_type) {
  output <- data
  if (length(response_type) != 1) {
    outcome_names <- names(data)
    # chaning binomials to 0, 1
    for (i in 1:length(outcome_names)) {
      if (response_type[i] == "binomial") {
        fixed <- unlist(data[, i]$result)
        if (all(fixed %in% c(0, 1, NA))) {
          output[[outcome_names[i]]]$result = list(fixed)
        } else {
          max <- max(fixed, na.rm = TRUE)
          fixed <- ifelse(max > fixed & (!is.nan(fixed)), 0, 1)
          output[[outcome_names[i]]]$result = list(fixed)
        }
      }
    }
    output
  } else {
    output <- data
    # chaning binomials to 0, 1
    if (response_type == "binomial") {
      fixed <- unlist(data$result)
      if (all(fixed %in% c(0, 1, NA))) {
        output$result = list(fixed)
      } else {
        max <- max(fixed, na.rm = TRUE)
        fixed <- ifelse(max > fixed & (!is.nan(fixed)), 0, 1)
        output$result = list(fixed)
      }
    }
  }
  return(output)
}

#' Washout function
#'
#' Allows the user to implement a washout period in the dataset. To do this,
#' the function calculates the length of the trial and then will replace some of
#' the original values with NULL with a different frequency depending on how often
#' the data was collected.
#'
#' @param read_data The data we want to apply a washout out
#' @param metadata The metadata section of a file. This is important as it contains
#' information about the length of the trial.
#' @return The function returns the data with a washout period implemented.
# Washout function. in some cases when we switch from A to B, for example, the
# first couple data points in B could be corrupted because the effects of A are
# still there. Thus we ingore the first couple data points (set them to NA).
washout <- function(read_data, metadata) {

  # calculating the duration of the study
  finish <- as.Date(metadata$trial_end_date, format = "%Y-%m-%d")
  start <- as.Date(metadata$trial_start_date, format = "%Y-%m-%d")
  date_diff <- as.numeric(finish - start + 1)

  # calculating the number of days and weeks
  num_days <- date_diff
  num_weeks <- date_diff/7

  # loop over the number of different observations we have
  for (i in 1:length(read_data)) {
    vec_treat <- unlist(read_data[[i]]$treatment)
    change_point <- cumsum(rle(vec_treat)$lengths)
    change_point <- change_point[-length(change_point)]
    # change point now gives us the number of points for the treatments (minus the
    # last one)

    # we only change weekly results and daily results, more than weekly does not make
    # much sense as enough time would have passed so that the previous treatment
    # would not make a difference anymore NB: at some point, may want to account for
    # different frequencies, ie. multiple time a day
    delete_obs_daily <- NULL
    delete_obs_weekly <- NULL

    if (length(vec_treat) == num_days) {
      for (j in 1:length(change_point)) {
        delete_obs_daily <- c(delete_obs_daily, (change_point[j] + 1):(change_point[j] +
          7))
      }
      delete_obs_daily
    } else if (length(vec_treat) == num_weeks) {
      for (j in 1:length(change_point)) {
        delete_obs_weekly <- c(delete_obs_weekly, (change_point[j] + 1))
      }
      delete_obs_weekly
    }
    read_data[[i]]$result[[1]][delete_obs_daily] <- NA
    read_data[[i]]$result[[1]][delete_obs_weekly] <- NA
  }
  read_data
}

# Finds the raw mean of the input vector. Returns raw mean for baseline and all
# other treatments.
find_raw_mean <- function(Y, Treat) {

  raw_mean <- c(mean(Y[Treat == "baseline"], na.rm = TRUE))
  for (i in 1:(length(unique(unlist(json.file$data[, 1]$treat))) - 1)) {
    raw_mean <- c(raw_mean, mean(Y[Treat == LETTERS[i]], na.rm = TRUE))
  }
  raw_mean[is.nan(raw_mean)] <- NA
  raw_mean
}

# Finds the raw median of the input vector. Returns raw median for baseline and
# all other treatments.
find_raw_median <- function(Y, Treat) {

  raw_median <- c(median(Y[Treat == "baseline"], na.rm = TRUE))
  for (i in 1:(length(unique(unlist(json.file$data[, 1]$treat))) - 1)) {
    raw_median <- c(raw_median, median(Y[Treat == LETTERS[i]], na.rm = TRUE))
  }
  raw_median[is.nan(raw_median)] <- NA
  raw_median
}

# Checking that for each outcome given, we have the correct number of treatments
check_nof_treatments <- function(treatment, data, nof_treat) {
  length(table(treatment[!is.na(data)])) == nof_treat
}

# i dont think this is needed at all. i dont rlly see the point of it tbh...
# check_success <- function(x){ ifelse(is.list(x), TRUE, x) }

# Rounds the raw mean. Unchanged if poisson or normal, multiply by 100 if
# binomial
round_number <- function(raw_mean, response) {

  if (response == "poisson" || response == "normal") {
    round(raw_mean, 1)
  } else if (response == "binomial") {
    round(raw_mean * 100)
  }
}

# This was used for calculate p-threshold, which is used for the graphs... Other
# stuff used for graphs not implemented: find_mean_difference,
# calculate_p_threshold, find_summary_graph
change <- function(x) {
  x = ifelse(x == 0, 1, x)
  x = ifelse(x == 100, 99, x)
  return(x)
}

# Our model file will not present our posterior distribution in an explicit form,
# we may need to exponentiate or use the inverse logit function.
link_function <- function(x, response) {
  answer <- if (response == "poisson") {
    exp(x)
  } else if (response == "binomial") {
    inv_logit(x)
  } else if (response == "normal") {
    x
  }
}

# Used for link_function. Defines inv_logit.
inv_logit <- function(a) {
  1/(1 + exp(-a))
}

#' Function to present a summary of our results
#'
#' A neat function to summarize the results.
#'
#' @param model_results A list which contains data file created
#' by \code{nof1.data} and the result file created by \code{nof1.run}
#' @param nof_treat The number of different treatments.
#' @param alpha The alpha value for the confidence interval. If no value is
#' entered will give the 95\% confidence interval.
#' @return The function returns some useful information about the simulation.
#' \item{input_mean}{The mean of the data inputed for each treatment type}
#' \item{input_median}{The median of the data inputed for each treatment type}
#' \item{treat_mean}{The mean of the data outputed for each treatment type}
#' \item{treat_median}{The median of the data outputed for each treatment type}
#' \item{coef_mean}{The mean of the data outputed for each coefficient}
#' \item{coef_median}{The median of the data outputed for each coefficient}
#' \item{treat_greater_zero}{The probability that the output value is greater
#'  than zero for each treatment type}
#' \item{coef_greater_zero}{The probability that the output value is greater
#'  than zero for each coefficient}
#' \item{quantile_confidence}{The quantile for the input data
#' set of the output value for each treatment type}
#' \item{quantile_confidence}{The quantile for the input data set
#' of the output value for each coefficient}
#' @export

# Function to present a summary of our results. When we take our draws we have
# coefficients: alpha, beta_A, beta_B, etc.  Baseline=alpha,
# treatmentA=alpha+beta_A, treatmentB=alpha+beta_B, etc. Function returns: input
# mean and median, mean and median for the treatments, mean and median for the
# coefs, P(treatment>0), P(coef>0), CI(alpha=0.5) for treatment and coef.
summarize_nof1 <- function(model_results, nof_treat, alpha = 0.025) {
  nof1 <- model_results[[1]]
  result <- model_results[[2]]

  with(c(nof1, result), {

    samples <- do.call(rbind, samples)

    # creating our list of treatment names. base, treat_A, treat_B, etc.
    treat_n = list("base")
    for (i in 1:(nof_treat - 1)) {
      treat_n <- c(treat_n, paste("treat", LETTERS[i], sep = "_"))
    }

    # creating our list of coef names
    coef_n = list("alpha")
    for (i in 1:(nof_treat - 1)) {
      coef_n <- c(coef_n, paste("beta", i, sep = "_"))
    }

    # mean and median for input vectors
    input_mean = c(mean(Y[Treat == "baseline"], na.rm = TRUE))
    input_median = c(median(Y[Treat == "baseline"], na.rm = TRUE))
    for (i in 1:(nof_treat - 1)) {
      input_mean <- c(input_mean, mean(Y[Treat == LETTERS[i]], na.rm = TRUE))
      input_median <- c(input_median, median(Y[Treat == LETTERS[i]], na.rm = TRUE))
    }

    input_mean[is.nan(input_mean)] <- NA
    input_median[is.nan(input_median)] <- NA

    # rounding mean and median
    input_mean <- sapply(input_mean, round_number, response)
    input_median <- sapply(input_median, round_number, response)

    names(input_mean) <- treat_n
    names(input_median) <- treat_n

    # creating treatment vectors
    treatment = list(samples[, 1])
    for (i in 2:nof_treat) {
      treatment <- c(treatment, list(link_function(samples[, 1] + samples[,
        i], response)))
    }
    names(treatment) <- treat_n

    # mean and median for treatment lists
    treat_mean <- list()
    treat_median <- list()
    treat_mean <- sapply(treatment, mean)
    treat_median <- sapply(treatment, median)
    names(treat_mean) <- treat_n
    names(treat_median) <- treat_n

    # mean and median for coef draws
    coef_mean <- list()
    coef_median <- list()
    for (i in 1:nof_treat) {
      coef_mean <- c(coef_mean, mean(samples[, i]))
      coef_median <- c(coef_median, median(samples[, i]))
    }
    names(coef_mean) <- coef_n
    names(coef_median) <- coef_n
    coef_mean <- unlist(coef_mean)
    coef_median <- unlist(coef_median)

    # probability that the treatment draw is greater than zero
    treat_greater_zero <- list()
    for (i in 1:nof_treat) {
      treat_greater_zero <- c(treat_greater_zero, sum(treatment[[i]] > 0)/length(treatment[[i]]))
    }
    names(treat_greater_zero) <- treat_n
    treat_greater_zero <- unlist(treat_greater_zero)

    # probability that the coef draw is greater than zero
    coef_greater_zero <- list()
    for (i in 1:nof_treat) {
      coef_greater_zero <- c(coef_greater_zero, sum(samples[, i] > 0)/length(samples[,
        i]))
    }
    names(coef_greater_zero) <- treat_n
    coef_greater_zero <- unlist(coef_greater_zero)

    # quantile for the treatment draw.
    quantile_confidence <- list()
    for (i in 1:nof_treat) {
      quantile_confidence <- rbind(quantile_confidence, quantile(treatment[[i]],
        c(alpha, 1 - alpha)))
    }
    rownames(quantile_confidence) <- treat_n

    # quantile for the coef draw.
    quantile_confidence <- list()
    for (i in 1:nof_treat) {
      quantile_confidence <- rbind(quantile_confidence, quantile(samples[, i], c(alpha,
        1 - alpha)))
    }
    rownames(coef_confidence) <- treat_n

    final <- list(input_mean = input_mean, input_median = input_median, treat_mean = treat_mean,
      treat_median = treat_median, coef_mean = coef_mean, coef_median = coef_median,
      treat_greater_zero = treat_greater_zero, coef_greater_zero = coef_greater_zero,
      quantile_confidence = quantile_confidence, quantile_confidence = quantile_confidence)

    return(final)
  })
}

#' Function to present a summary of our results
#'
#' A neat function to summarize the results when we run a simulation for each outcome
#'
#' @param data_result_list A list of data files and result files. This is the
#' output of \code{wrap_all}
#' @param nof_treat The number of different treatments.
#' @param alpha The alpha value for the confidence interval. If no value is
#' entered will give the 95\% confidence interval.
#' @return The function returns some useful information about the simulation. It
#' returns the following for each outcome.
#' \item{input_mean}{The mean of the data inputed for each treatment type}
#' \item{input_median}{The median of the data inputed for each treatment type}
#' \item{treat_mean}{The mean of the data outputed for each treatment type}
#' \item{treat_median}{The median of the data outputed for each treatment type}
#' \item{coef_mean}{The mean of the data outputed for each coefficient}
#' \item{coef_median}{The median of the data outputed for each coefficient}
#' \item{treat_greater_zero}{The probability that the output value is greater
#'  than zero for each treatment type}
#' \item{coef_greater_zero}{The probability that the output value is greater
#'  than zero for each coefficient}
#' \item{treat_confidence}{The confidence interval of the input data
#' set of the output value for each treatment type}
#' \item{coef_confidence}{The confidence interval of the input data set
#' of the output value for each coefficient}
#' @export
summarize_all_nof1 <- function(data_result_list, nof_treat, alpha = 0.025) {
  summary_list <- vector("list", length = length(data_result_list[[2]]))
  for (i in 1:length(data_result_list[[2]])) {
    summary_list[[i]] <- summarize_nof1(data_result_list[[2]][[i]], nof_treat,
      alpha)
  }
  names(summary_list) <- names(data_result_list[[2]])

  return(summary_list)
}

#' Function to run the analysis on all outcomes
#'
#' \code{wrap_all} allows the user to run an anaylsis on a specific data set.
#'
#' @param dataset The dataset the analysis will be conducted on.
#' @param result_list A list of all the models to run. Options are: normal,
#' binomial, poisson, or ordinal.
#' @return The function returns some useful information about the simulation
#'  as well as information about the simulation.
#' \item{system_info}{Provides information about the clincical trial conducted}
#' \item{user_id}{The user id for the particular patient whose data was analyzed}
#' \item{trigger}{The trigger the study was examining}
#' \item{design}{How the study was designed. How many weeks of treatment A? Of
#'  treatment B?}
#' \item{nof_treat}{The number of different treatments that were admininstered}
#' \item{model_results}{This is a list which contains the data file that was
#' constructed using \code{nof1.data} and the result file which was created
#' with \code{nof1.run}}
#' @export
wrap_all <- function(dataset, response_list, washout = TRUE) {
  data = dataset$data
  metadata = dataset$metadata

  # getting our data read and formated
  read_data <- tryCatch({
    read_dummy <- formated_read_input(data, response_list)
    if (!washout) {
      read_dummy
    } else {
      read_dummy <- washout(read_dummy, metadata)
      read_dummy
    }
  }, error = function(error) {
    return(paste("input read error: ", error))
  })

  # initializing some values
  names <- names(data)
  nof_responses <- length(data)
  read_len <- length(read_data)
  nof_treat <- length(unique(unlist(data[, 1]$treat)))

  # running our algorithm
  returns = list()
  for (i in 1:nof_responses) {
    returns[[i]] <- wrap_helper(read_data[, i], response_list[i])
  }
  names(returns) <- as.list(names)

  # checking that the number of treatments is correct for each observation
  # correct_treat <- list() for (i in 1:nof_responses) { correct_treat <-
  # c(correct_treat, check_nof_treatments(read_data[, i]$treatment, read_data[,
  # i]$result, nof_treat)) } names(correct_treat) <- as.list(names)

  # listing some info about algorithm run system_info <- list(enough_data =
  # correct_treat[[1]], user_id = metadata$user_id, trigger = metadata$trigger,
  # design = metadata$design, timestap_completion = Sys.time())

  system_info <- list(user_id = metadata$user_id, trigger = metadata$trigger, design = metadata$design,
    nof_treat = nof_treat, timestap_completion = Sys.time())

  final <- list(system_info = system_info, model_results = returns)

  # str(returns)
  return(final)
}

#' Function to run the analysis on a single outcome
#'
#' \code{wrap_single} allows the user to run an anaylsis
#'  on a specific outcome in a specific data set.
#'
#' @param dataset The dataset the analysis will be conducted on.
#' @param outcome_name The name of the outcome the analysis will be run on.
#' @param result_type The type of model to run. Options are: normal,
#' binomial, poisson, or ordinal.
#' @return The function returns some useful information about the simulation
#' as well as information about the simulation.
#' \item{system_info}{Provides information about the clincical trial conducted}
#' \item{user_id}{The user id for the particular patient whose data was analyzed}
#' \item{trigger}{The trigger the study was examining}
#' \item{design}{How the study was designed. How many weeks of treatment A? Of
#'  treatment B?}
#' \item{nof_treat}{The number of different treatments that were admininstered}
#' \item{model_results}{This is a list which contains the data file that was
#' constructed using \code{nof1.data} and the result file which was created
#' with \code{nof1.run}}
#'
#' @export
wrap_single <- function(dataset, outcome_name, response_type, washout = TRUE) {
  data = dataset$data[[as.name(outcome_name)]]
  metadata = dataset$metadata

  # getting our data read and formated
  read_data <- tryCatch({
    read_dummy <- formated_read_input(data, response_type)
    if (!washout) {
      read_dummy
    } else {
      read_dummy <- washout(read_dummy, metadata)
      read_dummy
    }
  }, error = function(error) {
    return(paste("input read error: ", error))
  })

  # initializing some values
  data_names <- names(data)
  nof_responses <- length(data)
  nof_treat <- length(unique(unlist(data[, 1]$treat)))

  # determining what index the outcome is
  for (i in 1:nof_responses) {
    if (data_names[i] == outcome_name) {
      index = i
    }
  }

  # running our algorithm
  result <- wrap_helper(read_data, response_type)

  # checking that the number of treatments is correct for each observation
  # correct_treat <-
  # check_nof_treatments(read_data[[as.name(outcome_name)]]$treatment,
  # read_data[[as.name(outcome_name)]]$result, nof_treat)

  # listing some info about algorithm run system_info <- list(enough_data =
  # correct_treat[[1]], user_id = metadata$user_id, trigger = metadata$trigger,
  # design = metadata$design, timestap_completion = Sys.time())

  system_info <- list(user_id = metadata$user_id, trigger = metadata$trigger, design = metadata$design,
    nof_treat = nof_treat, timestap_completion = Sys.time())

  # in final output, print system_info, the data file, and result file
  final <- list(system_info = system_info, model_results = result)

  return(final)
}

#' Helper function used to run the models
#'
#' Allows the user to run an anaylsis on a specific data set.
#'
#' @param specific_data The specific part of the dataset the analysis will be conducted on.
#' @param response_type The type of model we want to simulate. Can be normal,
#' binomial, poisson, or ordinal.
#' @return The function returns a list of two objects.
#' \item{nof1_out}{This the the data file that was constructed using \code{nof1.data}}
#' \item{result_out}{This is the result of \code{nof1.run}. It is the result of the simulations}
#'
wrap_helper <- function(specific_data, response_type) {
  data_out <- list(Treat = specific_data$treatment[[1]], Y = specific_data$result[[1]])
  nof1_out <- with(data_out, {
    nof1.data(Y, Treat, response = response_type)
  })
  result_out <- nof1.run(nof1_out)
  return(list(nof1_out, result_out))
}

# Old Helper function for our wrap function. Creates the neccesary objects needed
# to run the nof1 algorithim, runs the algorithim, and then summarizes the
# result.  wrap_helper2 <- function(specific_data, response_type, nof_treat) {
# data_out <- list(Treat = specific_data$treatment[[1]], Y =
# specific_data$result[[1]]) nof1_out <- with(data_out, { nof1.data(Y, Treat,
# response = response_type) }) result_out <- nof1.run(nof1_out)
# print(str(nof1_out)) print(str(result_out)) summarize_nof1(nof1_out,
# result_out, nof_treat) }
