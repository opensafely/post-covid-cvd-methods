# Rounding function for redaction ----

roundmid_any <- function(x, to = 6) {
  # centers on (integer) midpoint of the rounding points
  x <- as.numeric(x)
  ceiling(x / to) * to - (floor(to / 2) * (x != 0))
}

# Function to make display numbers ----

display <- function(x, to = 1) {
  ifelse(
    x >= 100,
    sprintf("%.0f", x),
    ifelse(x >= 10, sprintf("%.1f", x), sprintf("%.2f", x))
  )
}

# Function for describing data ----

describe_data <- function(df, name) {
  fs::dir_create(here::here("output/describe/"))
  sink(paste0("output/describe/", name, ".txt"))
  print(Hmisc::describe(df))
  sink()
  message(paste0("output/describe/", name, ".txt written successfully."))
}

# Function for creating a median (iqr_low-iqr_high) string ----

create_median_iqr_string <- function(x)
  return(paste0(
    quantile(x)[3],
    " (",
    quantile(x)[2],
    "-",
    quantile(x)[4],
    ")"
  ))

# Function to convert numerical data to categorical data, following chosen bounds

numerical_to_categorical <- function(
  x,
  bounds = c(1, 100),
  zero_flag = FALSE,
  lower_limit = FALSE,
  upper_limit = FALSE,
  inclusive_bounds = FALSE
) {
  # x <- the numeric input vector
  # bounds <- a vector of bounds (must be ordered low->high)
  # zero_flag <- if TRUE, include an additional category for zero-values
  # lower_limit <- if TRUE, the first value in bounds is a hard lower bound
  #                if FALSE, create a category between 0 and the first value
  # upper_limit <- if TRUE, the last value in bounds is a hard upper bound
  #                if FALSE, create a category for greater than the last value
  # inclusive_bounds <- whether the bounds are inclusive or exclusive (assuming discrete values)
  #                     N.B. will assign borderline cases to upper boundary
  N <- length(bounds)
  gap <- ifelse(inclusive_bounds, 0, 1)
  y <- x

  if (!lower_limit) {
    y <- ifelse(x <= bounds[1] - gap, sprintf("<=%d", bounds[1] - gap), y)
  }
  if (zero_flag) {
    y <- ifelse(x == 0, sprintf("0"), y)
  }
  for (i1 in 1:(N - 1)) {
    y <- ifelse(
      x >= bounds[i1] & x <= bounds[i1 + 1] - gap,
      sprintf("%d-%d", bounds[i1], bounds[i1 + 1] - gap),
      y
    )
  }
  if (!upper_limit) {
    y <- ifelse(x >= bounds[N], sprintf("%d+", bounds[N]), y)
  }
  return(y)
}


fill_in_blanks <- function(p_values = NULL, labels = NULL) {
  for (label in labels) {
    # if a given variable doesn't exist, create as NaN
    if (!(label %in% names(p_values))) {
      p_values[label] <- NaN
    }
  }
  # assert variable ordering
  p_values <- p_values[order(factor(names(p_values), levels = labels))]
  
  return (p_values)
}


make_exposure_formula <- function(vars_selected = NULL) {
  vars_no_exposure <- vars_selected[!vars_selected %in% c("cov_bin_covid")]

  if (length(vars_no_exposure) == 0) {
    formula_string   <- "cov_bin_covid ~ 1"

  } else if (length(vars_no_exposure) == 1) {
    formula_string   <- paste0("cov_bin_covid ~ ", vars_no_exposure[1])

  } else {
    formula_string   <- "cov_bin_covid ~ "
    for (var in vars_no_exposure) {
      formula_string <- paste0(formula_string, var, " + ")
    }
    formula_string <- stringr::str_sub(formula_string, end = -3)
  }

  return (formula_string)
}


make_outcome_formula <- function(vars_selected = NULL, outcome = NULL) {
  vars_no_outcome <- vars_selected[!vars_selected %in% c(outcome, "outcome_cox_dates", "cens_status")]

  if (length(vars_no_outcome) == 0) {
    formula_string   <- paste0("Surv(time  = as.numeric(outcome_cox_dates), event = cens_status, type  = 'right')", " ~ 1")

  } else if (length(vars_no_outcome) == 1) {
    formula_string   <- paste0("Surv(time  = as.numeric(outcome_cox_dates), event = cens_status, type  = 'right')", " ~ ", vars_no_outcome[1])

  } else {
    formula_string   <- paste0("Surv(time  = as.numeric(outcome_cox_dates), event = cens_status, type  = 'right')", " ~ ")
    for (var in vars_no_outcome) {
      formula_string <- paste0(formula_string, var, " + ")
    }
    # remove final ' + '
    formula_string <- stringr::str_sub(formula_string, end = -4)
  }

  return (formula_string)
}


convert_terms_to_vars <- function(terms = NULL, all_var_names = NULL) {
  vars <- c()

  for (term in terms) {
    # split by capital letter, extract first term
    # covariate names always all lower case
    # Factor Level Names always begin with upper case
    new_term <- sapply(strsplit(x = term, split = '([[:upper:]])'), `[`, 1)
    vars     <- c(vars, new_term)
  }

  # remove duplicates (i.e. two levels are significant)
  vars <- unique(vars)

  # remove any trailing . (leftover from levels)
  vars <- gsub('.', '', vars)

  return (vars)
}


generate_weights <- function(sample_size = NULL, num_imps = NULL) {
  weights <- rep((1 / num_imps), length.out = sample_size)
  return (weights)
}
