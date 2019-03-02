#' time series plot across different interventions
#'
#' @param nof1 nof1 object created using nof1.data
#' @param timestamp time of the nof1 event occurring
#' @param timestamp.format format of the timestamp
#' @param Outcomes.name used to label y-axis outcome variable
#' @examples
#' Y <- laughter$Y
#' Treat <- laughter$Treat
#' nof1 <- nof1.data(Y, Treat, ncat = 11, baseline = 'Usual Routine', response = 'ordinal')
#' timestamp <- seq(as.Date('2015-01-01'),as.Date('2016-01-31'), length.out = length(Y))
#' time_series_plot(nof1, timestamp = timestamp, timestamp.format = '%m-%d-%Y', Outcome.name = 'Stress')

# changed '#ffb380' to '#ff8080' and added '#80ff8c'. Issue was that there were
# not enough color options. Added na.rm=TRUE for geom_hline. number of categories
# is only used for ordinal data. so maybe time_series_plot only works for
# ordinal.
time_series_plot <- function(nof1, date_start = NULL, date_end = NULL, date.format = "%m/%d/%Y %H:%M",
  title = NULL) {
  if (nof1$response %in% c("normal", "poisson")){
    period <- seq(date_start, date_end, length.out = length(nof1$Y))
    date <- as.Date(period, date.format)
    data <- data.frame(Y = nof1$Y, date = date, Treatment = factor(nof1$Treat))
    ggplot(data, aes(x = date, Y, color = Treatment)) + geom_point(na.rm = TRUE) +
      facet_wrap(.~ Treatment) + theme_bw() +
      labs(x = "Date", y = "Outcomes", title = title, subtitle = "Time Series Plot")
      # geom_smooth(method ="lm", na.rm = TRUE)
  }
  else{
    date <- as.Date(timestamp, timestamp.format)

    data <- data.frame(Y = as.numeric(nof1$Y), Treatment = gsub("\\_", " ", nof1$Treat),
      date = date)
    data2 <- aggregate(nof1$Y, list(Treatment = gsub("\\_", " ", nof1$Treat)), mean)

    ggplot(data,
      aes(x = date, Y, fill = Treatment)) +
      geom_bar(stat = "identity", na.rm = TRUE) + # use y-values for the height of the bar
      facet_grid(. ~ Treatment) + # form panels for each treatment
      theme_bw() + # makes theme black and white
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      labs(x = "Date", y = Outcome.name) +
      geom_hline(data = data2, aes(yintercept = x, linetype = "Mean"), color = "black") + # adds horizontal line in the data
      scale_y_continuous(breaks = 0:nof1$ncat, oob = rescale_none, label = c("Low", rep("", length = nof1$ncat - 1), "High")) +
      # above adds in the lines on the y-axis and the low and high notation
      scale_fill_manual(values = c("#adc2eb", "#ffb380")) +
      scale_linetype_manual(name = "", values = 1, guide = guide_legend(override.aes = list(color = c("black"))))
  }
}

#' Frequency plot for raw data
#'
#' @param nof1 nof1 object created using nof1.data
#' @param title The title of the figure
#' @param bins Used for continuous data. Specifies the number of bins. The
#' default value is 10.
#' @examples
#' Y <- laughter$Y
#' Treat <- laughter$Treat
#' nof1 <- nof1.data(Y, Treat, ncat = 11, baseline = 'Usual Routine', response = 'ordinal')
#' frequency_plot(nof1)
frequency_plot <- function(nof1, title = NULL, bins = 10) {

  if (nof1$response %in% c("binomial")) {
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data = data, aes(x = factor(Y, levels = 0:1), y = x, fill = Treat)) + geom_bar(stat = "identity",
      position = "dodge", width = 0.8, na.rm = TRUE, alpha = 0.9) +
      labs(title = title, subtitle = "Frequency Plot", x = "Outcomes", y = "Count", fill = "Treatment") +
      scale_x_discrete(labels = c("Low", "High")) +
      theme_bw()
  } else if (nof1$response %in% c("ordinal")) {
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data = data, aes(x = Y, y = x, fill = Treat)) + geom_bar(stat = "identity",
      position = "dodge", na.rm = TRUE) + labs(title = title, x = "Outcomes", y = "Count",
      fill = "Outcomes") + xlim(0.5, nof1$ncat + 0.5) + theme_bw()
  } else if (nof1$response %in% c("normal", "poisson")) {
    data <- data.frame(Y = nof1$Y, Treatment = nof1$Treat)
    ggplot(data, aes(x = Y, fill = Treatment, color = Treatment)) +
      # position dodge puts them next to each other, na.rm gets rid of NA data,
      # bins is, alpha is how see through the colors are
      geom_histogram(position = "dodge", na.rm = TRUE, bins = bins, alpha = 0.9) +
      labs(title = title, subtitle = "Frequency Plot", x = "Outcomes", y = "Count") +
      theme_bw()
  }
}

#' Stacked_percent_barplot for raw data (for ordinal or binomial data)
#'
#' @param nof1 nof1 object created using nof1.data
#' @param title The title of the figure
#' @examples
#' Y <- laughter$Y
#' Treat <- laughter$Treat
#' nof1 <- nof1.data(Y, Treat, ncat = 11, baseline = 'Usual Routine', response = 'ordinal')
#' stacked_percent_barplot(nof1)
stacked_percent_barplot <- function(nof1, title = NULL) {

  if (nof1$response %in% c("binomial")) {
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data, aes(fill = factor(Y, levels = 0:1, labels = c("Low", "High")), y = x, x = Treat)) + geom_bar(stat = "identity",
      position = "fill", na.rm = TRUE) + scale_y_continuous(labels = percent_format()) +
      theme_bw() +
      labs(title = title, subtitle = "Stacked Percent Barplot", x = "Treatment", y = "Percentage", fill = "Outcomes") +
      scale_fill_manual(values = c("light blue", "dark blue"))
  } else if (nof1$response %in% c("ordinal")) {
    data <- aggregate(nof1$Y, list(Y = nof1$Y, Treat = nof1$Treat), length)
    ggplot(data, aes(fill = factor(Y, levels = 1:nof1$ncat), y = x, x = Treat)) +
      geom_bar(stat = "identity", position = "fill") + scale_y_continuous(labels = percent_format()) +
      theme_bw() + labs(title = title, x = "Treatment", y = "Percentage", fill = "Outcomes") +
      scale_fill_manual(values = 4:(3 + nof1$ncat), labels = 1:nof1$ncat, drop = FALSE)
  } else {
    stop("only works for binomial and ordinal data")
  }
}

#' Summary data table
#'
#' Provides a summary data table for the particular outcome in a particular dataset.
#'
#' @param nof1 nof1 object created using nof1.data
#' @return Gives a comprhensive table with several statistical values.
#' Each column indicates the value given. For a normal or poisson
#' response type the following are given:
#' \item{Treat}{The treatment recieved}
#' \item{mean}{The average value of the outcome}
#' \item{sd}{The standard deviation for the outcome}
#' \item{2.5\%}{2.5\% of the data are equal to or less than this value}
#' \item{50\%}{50\% of the data are equal to or less than this value}
#' \item{97.5\%}{97.5\% of the data are equal to or less than this value}
#'
#' For a binomial or ordinal response type, returns a table where first row
#' is each treatment and the following rows are the the number of data points
#' taken at each possible value.
raw_table <- function(nof1) {

  if (nof1$response %in% c("binomial", "ordinal")) {
    raw_table <- table(nof1$Y, nof1$Treat, useNA = "ifany")
  } else if (nof1$response %in% c("normal", "poisson")) {
    raw_table <- aggregate(nof1$Y, list(Treat = nof1$Treat), mean, na.rm = TRUE)
    colnames(raw_table)[2] <- "mean"
    raw_table <- cbind(raw_table, sd = aggregate(nof1$Y, list(Treat = nof1$Treat), sd, na.rm = TRUE)[,
      -1], aggregate(nof1$Y, list(Treat = nof1$Treat), quantile, na.rm = TRUE,
      c(0.025, 0.5, 0.975))[, -1])
  }
  return(raw_table)
}

#' Raw Graphs and Tables
#'
#' \code{raw_graphs} allows the user to create a specific table or graph
#' for an outcome. Possible graphs include: time_series_plot, frequency_plot,
#' stacked_percent_barplot, and raw_table.
#'
#' @param graph A string which specifies the specific table or graph to construct.
#'  Options include: time_series_plot, frequency_plot, stacked_percent_barplot, and raw_table.
#' @param model_result The analysis we want to make the graphs for. It is
#' the output of \code{wrap_single}
#' @param multiple A boolean that indicates if the \code{model_result} input has multiple
#'  possible outcome. In other words, was \code{model_result} input created \code{\link{from wrap_all}}
#'  or \code{\link{wrap_single}}? The default is FALSE meaning we only have one outcome in the
#'  \code{model_result} input.
#' @param outcome_name A string that indicates the name of the outcome we wish to graph
#' @param ... Specific paramaters depending on table or graph to constuct
#' @return Returns one of the following graphs: time series plot, frequency plot,
#' stacked percent barplot, or raw table for the inputed data.
#' @examples
#' raw_graphs("raw_table", all_result_afib, TRUE, "afib_episode_yn")
#' raw_graphs("frequency_plot", result_afib)
#' @export
raw_graphs <- function(graph, result, multiple = FALSE, outcome_name = NULL,
                       date_start = NULL, date_end = NULL, date.format = "%m-%d-%Y",
                       title = NULL, bins = 10) {

  treatment_names <- result$system_info$treatments
  # getting correct model data
  if (!multiple) {
    nof1_out <- result$model_results[[1]]
  }
  else {
    if (is.null(outcome_name)) {
      stop("Need to specify outcome name when have result input with multiple possible outcome")
    }
    else {
      nof1_out <- result$model_results[[as.name(outcome_name)]][[1]]
      title = outcome_name
    }
  }

  # changing the values of the Treat vector
  curr_treats <- unique(nof1_out$Treat)
  for (i in 1:length(curr_treats)){
    nof1_out$Treat[nof1_out$Treat == curr_treats[i]] = treatment_names[i]
  }

  if (graph == "time_series_plot") {
    time_series_plot(nof1_out, date_start = date_start, date_end = date_end, date.format = date.format, title = title)
  } else if (graph == "frequency_plot") {
    frequency_plot(nof1_out, title, bins)
  } else if (graph == "stacked_percent_barplot") {
    stacked_percent_barplot(nof1_out, title)
  } else if (graph == "raw_table") {
    raw_table(nof1_out)
  } else {stop("Not a viable graph or table")}
}

#######################

#' Kernel density estimation plot
#'
#' Creates a kernel density estimation plot for a specific outcome
#'
#' @param result An object with information about the simulation. The object is
#' derived from the output of nof1.run
#' @param bins The number of bins the histogram will contain. Default is 30.
#' @param x_min The lower limit of the x-axis. Default is to set to zero
#' @param x_max The upper limit of the x-axis. Default is to set the upper limit
#' to the maximum value of the data inputed
#' @param title The title of the graph
kernel_plot <- function(result, bins = 30, x_min = NULL, x_max = NULL, title = NULL) {
  samples <- do.call(rbind, result$samples)
  beta_variable <- exp(samples[, grep("beta", colnames(samples))])
  data <- as.data.frame(beta_variable)
  data <- melt(data, id.vars = NULL, variable.name = "beta", value.name = "odds_ratio")

  if (is.null(x_min)){x_min <- 0}
  if (is.null(x_max)){x_max <- max(data$odds_ratio)}

  response_type = result$nof1$response
  if (response_type == "binomial") {xlab = "Odds Ratio"}
  else if (response_type == "poisson") {xlab = "Relative Risk"}
  else if (response_type == "normal") {xlab = "Mean Difference"}
  else if (response_type == "ordinal") {xlab = "Odds Ratio"}

  ggplot(data, aes(x = odds_ratio, color = beta)) + geom_density(na.rm = TRUE, size = 2) +
    geom_histogram(aes(y = ..density..), bins = bins, col = "gray", alpha = 0.7, na.rm = TRUE) + theme_bw() +
    facet_wrap(. ~ beta, scales = "free") + xlim(0, x_max) +
    labs(title = title, subtitle = "Kernel Density Estimation Plot", x = xlab, y = "Density")
}

#' Odds ratio plot
#'
#' Creates a odds ratio plot for all the outcomes
#'
#' @param result.list A list of objects with information about the simulation.
#' The objects are derived from the output of nof1.run
#' @param level The level defines the quantiles we want to examine.
#' @param title The title of the graph. Default is just to be: "Odds Ratio Plot"
odds_ratio_plot <- function(result.list, level = 0.95, title = "Odds Ratio Plot") {

  num_coef <- length(result.list[[1]][[1]]$Treat.name)
  odds_ratio <- matrix(NA, nrow = num_coef * length(result.list), ncol = 3)
  # odds_ratio <- matrix(NA, nrow = length(result.list), ncol = 3)
  beta_names <- list()

  for (i in 1:length(result.list)) {
    result <- result.list[[i]][[2]]
    samples <- do.call(rbind, result$samples)
    df_samples <- as.data.frame(samples[, grep("beta", colnames(samples))])
    if (num_coef == 1) {colnames(df_samples) <- "beta_A"}
    beta_names <- names(df_samples)
    for (j in 1:num_coef) {
      odds_ratio[i + length(result.list) * (j-1), 1:3] <-
        exp(quantile(df_samples[[j]], c((1 - level)/2, 0.5, 1 - (1 - level)/2)))
    }
    # odds_ratio[i, ] <- exp(quantile(samples[, grep("beta", colnames(samples))], c((1 - level)/2, 0.5, 1 - (1 - level)/2)))
  }

  odds <- as.data.frame(odds_ratio)
  names(odds) <- c("lower", "OR", "upper")
  odds$outcomes <- names(result.list)
  odds$beta <- (sort(rep(beta_names, length(result.list))))

  ticks <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100)
  ggplot(odds, aes(x = OR, y = factor(outcomes), color = outcomes)) + geom_point(size = 3) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, size = 1) +
    scale_x_log10(breaks = ticks, labels = ticks) +
    geom_vline(xintercept = 1, linetype = "dashed") + facet_wrap(. ~ beta, scales = "free") +
    labs(y = "Outcomes", x = "Odds Ratio", color = "Outcomes", title = title) + theme_bw()
}


probability_barplot <- function(result.list, title = "Probability Barplot") {

  num_coef <- length(result.list[[1]][[1]]$Treat.name)
  probability <- matrix(NA, nrow = length(result.list) * 2 * num_coef, ncol = 2)
  beta_names <- list()
  # print(probability)

  treatment_names <- unique(result.list[[1]][[1]]$Treat)
  # print(treatment_names)

  for (i in 1:length(result.list)) {
    result <- result.list[[i]][[2]]
    samples <- do.call(rbind, result$samples)

    df_samples <- as.data.frame(samples[, grep("beta", colnames(samples))])
    # str(df_samples)
    if (num_coef == 1) {colnames(df_samples) <- "beta_A"}
    beta_names <- names(df_samples)
    # print(beta_names)
    for (j in 1:num_coef) {
      probability[(i - 1) * 2 + 1 + num_coef * length(result.list) * (j-1), 1] <-
        mean(exp(df_samples[[j]]) > 1)
      probability[2 * i + num_coef * length(result.list) * (j-1), 1] <-
        1 - as.numeric(probability[(i - 1) * 2 + 1 + num_coef * length(result.list) * (j-1), 1])
      probability[(i - 1) * 2 + 1 + num_coef * length(result.list) * (j-1), 2] <- treatment_names[j + 1]
      probability[2 * i + num_coef * length(result.list) * (j-1), 2] <- treatment_names[1]

      # print(as.data.frame(probability))
    }
  }

  probability <- as.data.frame(probability)
  names(probability) <- c("probability", "Treat")
  probability$probability <- as.numeric(levels(probability$probability))[probability$probability]
  probability$outcomes <- rep(names(result.list), each = 2)
  probability$beta <- (sort(rep(beta_names, num_coef * length(result.list))))
  print(probability)


  #   probability[(i - 1) * 2 + 1] <- mean(exp(samples[, grep("beta", colnames(samples))]) > 1)
  #   str(samples[, grep("beta", colnames(samples))])
  #   str(exp(samples[, grep("beta", colnames(samples))]))
  #   probability[i * 2] <- 1 - probability[(i - 1) * 2 + 1]
  # }

  # result.name <- rep(names(result.list), each = 2)
  #
  # treatment_names <- unique(result.list[[1]][[1]]$Treat)
  # print(treatment_names)
  # data <- data.frame(probability = probability, result.name = result.name,
  #   Treat = rep(rev(c("baseline", "A")), length(result.list)))
  # print(data)

  ggplot(probability, aes(fill = factor(Treat), y = probability, x = outcomes)) + geom_bar(stat = "identity",
                                                                                           position = "fill") + scale_y_continuous(labels = percent_format()) + facet_wrap(. ~ beta, scales = "free") +
    labs(x = "Variables", y = "Percentages", fill = "Treatment", title = title) +
    coord_flip() + theme_bw()
}

#' Result Graphs
#'
#' \code{result_graphs} allows the user to create a specific table or graph
#' for an outcome. Possible graphs include: kernel density plot, odds ratio plot,
#' and probability barplot.
#'
#' @param graph A string which specifies the specific table or graph to construct.
#'  Options include: kernel_density, odds_ratio_plot, probability_barplot.
#' @param model_result The analysis we want to make the graphs for. It is
#' the output of \code{wrap_single}
#' @param multiple A boolean that indicates if the \code{model_result} input has multiple
#'  possible outcome. In other words, was \code{model_result} input created \code{\link{from wrap_all}}
#'  or \code{\link{wrap_single}}? The default is FALSE meaning we only have one outcome in the
#'  \code{model_result} input.
#' @param outcome_name A string that indicates the name of the outcome we wish to graph
#' @param ... Specific paramaters depending on table or graph to constuct
#' #' @return Returns one of the following graphs: kernel density plot, odds ratio plot,
#' and probability barplot for the model data.
#' @export
result_graphs <- function(graph, model_result, multiple = TRUE, outcome_name = NULL,
                          result.name = NULL, title = NULL, bins = 30, x_min = NULL,
                          x_max = NULL, level = 0.95) {
  if (!multiple && graph == "kernel_plot"){
    kernel_plot(model_result[[2]][[2]], bins = bins, x_max = x_max, title = outcome_name)
  }
  else if (!multiple){
    stop("Can only create a kernel plot if input one outcome")
  }
  else{
    if (is.null(outcome_name) && graph == "kernel_plot"){
      stop("Need to input the specific outcome for a kernel plot with multiple outcomes")
    }
    else if (graph == "kernel_plot"){
      kern_out <- model_result[[2]][[as.name(outcome_name)]][[2]]
      kernel_plot(kern_out, bins = bins, x_min = x_min, x_max = x_max, title = outcome_name)
    }
    else if (graph == "odds_ratio_plot"){
      if (is.null(title)){title = "Odds Ratio Plot"}
      odds_ratio_plot(model_result[[2]], level = level, title = title)
    }
    else if (graph == "probability_barplot"){
      if (is.null(title)){title = "Probability Barplot"}
      probability_barplot(model_result[[2]], title = title)
    } else {stop("Not a viable graph")}
  }
}

# data = dataset$data
# metadata = dataset$metadata
#
# # getting our data read and formated
# read_data <- tryCatch({
#   read_dummy <- formated_read_input(data, metadata)
#   if (metadata$washout == "FALSE" || is.null(metadata$washout)) {
#     read_dummy
#   } else {
#     read_dummy <- washout(read_dummy, metadata)
#     read_dummy
#   }
# }, error = function(error) {
#   return(paste("input read error: ", error))
# })
#
# # Define some variables to be used in constructing nof1 object
# names <- names(data)
# nof_responses <- length(data)
# read_len <- length(read_data)
# nof_treat <- length(unique(unlist(data[, 1]$treat)))
#
# if (graph == "kernel_plot") {
#   response_type = metadata[length(metadata) - nof_responses + kern_index]
#   data_out <- list(Treat = read_data[, kern_index]$treatment[[1]], Y = read_data[, kern_index]$result[[1]])
#   nof1_out <- with(data_out, {nof1.data(Y, Treat, response = response_type)})
#   result_out <- nof1.run(nof1_out)
#   # print(str(result_out))
#   kernel_plot(result_out, title = title)
# }
# else {
#   returns = list()
#   for (i in 1:nof_responses) {
#     returns[[i]] <- result_graphs_helper(read_data[, i], names[i], metadata[length(metadata) -
#                                                                               nof_responses + i], nof_treat)
#   }
#   names(returns) <- as.list(names)
#   # print(str(returns))
#   if (graph == "odds_ratio_plot") {
#     odds_ratio_plot(returns, result.name = result.name, title = title)
#   } else if (graph == "probability_barplot") {
#     probability_barplot(returns, result.name = result.name)
#   } else {"Not a viable graph or table"
#   }
# }
