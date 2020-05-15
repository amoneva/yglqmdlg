# ==============================================================================
# Project: CACC R package
# Script purpose: To develop functions for an automatic application of CACC
# Date: 2020-04-15
# Authors: Esteve, M., Moneva, A., & Hart, T. C.
# R version 4.0.0 (2020-04-24) -- "Arbor Day"
# ==============================================================================


# ==============================================================================
# Load packages
# ==============================================================================
library(tidyr)
library(dplyr)
library(stats)
library(ggplot2)

# ==============================================================================
# `cacc()` function with the "dplyr" package
#     dataset = data
#     x = independent variables (IV)
#     y = dependet variable (DV)
# !diagnostics off
# ==============================================================================
cacc <- function (dataset, x, y) {
  
  # If the CACC matrix is already calculated, stop the execution
  if(("N_Break" %in% names(dataset)) == TRUE){
    print("The CACC matrix is already calculated.")
    return(dataset)
  }
  
  # Dependent variable
  x = colnames(dataset[, - ncol(dataset)])
  # Independent variable
  y = colnames(dataset[, ncol(dataset)])
  # ------------------------------ Preprocessing -------------------------------
  # Check the DV:
  #   if it is not numeric, convert its values to numeric and replace them with
  #     0 / 1;
  #   if it is not binary, the function returns an ERROR;
  #   if it is binary:
  # ----------------------------------------------------------------------------
  if (length(unique(dataset[[y]])) != 2) {
    stop ("ERROR. The dependent variable must be binary.")
  } else if (!is.numeric(dataset[[y]])) {
    print("ERROR. The dependent variable must be numeric and binary.")
    print("Preprocessing...")
    
    # Convert the variable into factor
    dataset[[y]] <- as.factor(dataset[[y]])
    
    # Replace its categories with 0 / 1
    levels(dataset[[y]]) <- c(0, 1)
    print("Done!")
    
  } else if (is.double(dataset[[y]])) {
    
    # First, the variable must be converted into integer
    dataset[[y]] <- as.integer(dataset[[y]])
    
    # Second, the variable must be converted into factor
    dataset[[y]] <- as.factor(dataset[[y]])
    
    # Third, its categories must be replaced with 0 / 1
    levels(dataset[[y]]) <- c(0, 1)
  }
  
  # --------- Handle dominant profiles depending on sample size ---------
  if (nrow(dataset) < 1000) {
    dom_pro = 5
  } else {
    dom_pro = 10
  }
  
  # ------------------------- Generate the CACC matrix -------------------------
  # Generate a matrix with total frequencies
  matrixT <- dataset %>%
    dplyr::count(.dots = x) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename(N_Break = "n")
  
  # Generate a matrix with DV frequencies based on the positive class (1)
  matrix1 <- dataset %>%
    dplyr::filter(get(y) == 1) %>%
    dplyr::count(.dots = x) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename(N_1 = "n")
  
  # Calculate the DV probabilities for each dominant profile
  cacc_matrix <- dplyr::full_join(matrixT,
                                  matrix1,
                                  by = x) %>%
    dplyr::mutate(p = N_1 / N_Break) %>%
    dplyr::filter(N_Break >= dom_pro) %>%
    dplyr::arrange(desc(p)) %>%
    dplyr::select(- one_of("N_1"))
  
  # Return the CACC matrix
  return(cacc_matrix)
}

# ==============================================================================
# `main_effect()` function in base R
# !diagnostics off
# ==============================================================================
### Create a function that requests the values of each IV as DV ###
my_scan <- function(list_values, column_name){
  # In ascending order
  list_values <- t(list_values[order(list_values[[1]]), ])
  cat("Enter a value for variable ", column_name, " (values: ", list_values[1, ], ") (or enter -1 to skip this variable): \n")
  value <- scan(nmax = 1)
  
  # If the user does not enter a value, repeat the question
  if (length(value) == 0)
  {
    cat("ERROR in the value entered. Please, enter a valid value for: column ", column_name, " (values: ", list_values[1, ], ") (or enter -1 to skip this variable): \n")
    value <- scan(nmax = 1)
    
    while(is.na(match(value, list_values)) & value != -1)
    {
      cat("ERROR in the value entered. Please, enter a valid value for: column ", column_name, " (values: ", list_values[1, ], ") (or enter -1 to skip this variable): \n")
      value <- scan(nmax = 1)
    }
  } else
    while(is.na(match(value, list_values)) & value != -1)
    {
      cat("ERROR in the value entered. Please, enter a valid value for: column ", column_name, " (values: ", list_values[1, ], ") (or enter -1 to skip this variable): \n")
      value <- scan(nmax = 1)
    }
  
  # Return the value entered  
  return(value)
}

### `main_effect()` function ###
main_effect <- function(dataset, x, y){
  # First, calculate the CACC matrix
  # Search for the `N_Break` in the data. If `N_Break` does not exist, it means that the CACC matrix was already calculated
  if(("N_Break" %in% names(dataset)) == FALSE){
    # DV
    x = colnames(dataset[, - ncol(dataset)])
    # IV
    y = colnames(dataset[, ncol(dataset)])
    # The CACC matrix
    cacc_matrix <- cacc(dataset)
  } else {
    cacc_matrix <- dataset
    # DV
    x = colnames(dataset[, !names(dataset) %in% c("N_Break", "p")])
    # IV
    y = colnames(dataset[, ncol(dataset)])
  }
  
  # Replace NA values with 0 to fill the CACC matrix
  cacc_matrix[is.na(cacc_matrix)] <- 0
  
  # Create a data frame contatining the vector differences of all DV
  columns <- length(x) # Number of IV
  
  # Create a list with any IV that were grouped together
  col_names <- list()
  
  list_total = list() # List of lists
  
  for(i in 1:columns){
    # Select and check values
    list_values <- unique(cacc_matrix[i])
    
    # If any IV is constant, continue with the next variable
    if(nrow(list_values) < 2){
      cat("\nWARNING. The function ignored the variable ", x[i], " because its values are constant.\n\n")
      next # Continue with the next variable
    }
    
    value <- my_scan(list_values, x[i])
    
    # If the user decides to skip this variable, continue with the next one
    if(value == -1){
      x <- x[-i]
      columns <- length(x) # Updated number of IV
      next # Continue with the next variable
    }
    
    # Grouping IVs with x[i] variable as DV
    PDI <- cacc_matrix %>%
      dplyr::group_by_at(dplyr::vars(-c(i, N_Break, p))) %>%
      dplyr::filter(dplyr::n() > 1)
    
    # If this variable cannot be grouped, it is ignored
    if(nrow(PDI) == 0){
      cat("WARNING. The function ignored the variable ", x[i], " with value ", value, " because it cannot be grouped.\n\n")
      next # Continue with the next variable
    }
    else {
      PDI <- PDI  %>%
        dplyr::arrange(.[[i]], .by_group = TRUE) %>%
        dplyr::mutate(diff = if_else(
          ((dplyr::nth(p, which(.[[i]] == value)[1])) != p),
          ((dplyr::nth(p, which(.[[i]] == value)[1])) - p ),
          (dplyr::nth(p, which(.[[i]] == value)[1]))
        )
        )
      col_names <- c(col_names, x[i])
    }
    
    # Add the list into the list of lists
    list_total[length(list_total) + 1] <- list(PDI$diff)
  }
  
  # Create a data frame
  dataF <- data.frame()
  for(i in seq(along = list_total))
    for(j in seq(list_total[[i]]))
      dataF[j, i] <- list_total[[i]][j]
  
  # Enter the grouped column names
  colnames(dataF) <- col_names
  
  # Print some descriptive statistics for the selected variables
  print(summary(dataF))
  
  # Visualize the output with a boxplot
  #   Pair key-value
  dataF <- dataF %>%
    tidyr::gather()
  
  # Plot the main effects
  print(
    ggplot2::ggplot(data = na.omit(dataF),
                    # Order the main effects according to the median valuue of their distribution
                    ggplot2::aes(x = stats::reorder(x = key,
                                                    X = stats::na.omit(dataF$value),
                                                    FUN = stats::median),
                                 y = value)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(yintercept = 0,
                          linetype = 2) +
      ggplot2::xlab("Variables") +
      ggplot2::ylab("Main effect") +
      ggplot2::theme_bw()
  )
}


# ==============================================================================
# `cacc_xsq()` function in base R
# !diagnostics off
# ==============================================================================
cacc_xsq <- function (dataset, x, y){
  # First, calculate the CACC matrix
  # Search for the `N_Break` in the data. If `N_Break` does not exist, it means that the CACC matrix was already calculated
  if(("N_Break" %in% names(dataset)) == FALSE){
    # DV
    x = colnames(dataset[, - ncol(dataset)])
    # IV
    y = colnames(dataset[, ncol(dataset)])
    # The CACC matrix
    cacc_matrix <- cacc(dataset)
  } else {
    cacc_matrix <- dataset
    # DV
    x = colnames(dataset[, !names(dataset) %in% c("N_Break", "p")])
    # IV
    y = colnames(dataset[, ncol(dataset)])
  }
  
  # Declare the variable containing the number of times each dominant profile is observed in the sample
  obs <- cacc_matrix$N_Break
  
  # Count the dominant profiles observed
  N_obs <- nrow(data.frame(obs))
  
  # Obtain the expected count vector by weighting the total amount of dominant observations by the amount of dominant profiles observed
  exp <- rep(sum(obs) / N_obs, N_obs)
  
  # Perform the Chi-square test
  # Set `rescale.p = TRUE` because probabilities must sum 1
  xsq <- chisq.test(x = obs, p = exp, rescale.p = TRUE)
  
  # Show the result
  xsq <- data.frame("X-squared" = xsq$statistic,
                    "df" = xsq$parameter,
                    "p" = xsq$p.value)
  rownames(xsq) <- NULL
  
  return (xsq)
}

# ==============================================================================
# `gg_lorenz_curve` function in base R
# A function that, after preparing the CACC matrix, calculates its SCI and plots the results
# ==============================================================================
### Create a function that prepares the CACC matrix properly for further analysis ###
data_prepare <- function(cacc_matrix){
  # Total number of observations
  N <- nrow(cacc_matrix)
  
  # `N_Break` ordered from lowest to highest
  cacc_matrix <- cacc_matrix[order(cacc_matrix$N_Break, decreasing = TRUE), ]
  
  # Insert a new row with the total sum of the `N_Break` variable
  vector <- c(rep(0, length(cacc_matrix)))  # Create a vector of zeros
  cacc_matrix <- rbind(vector, cacc_matrix) # Insert the vector in the data frame
  cacc_matrix$N_Break[1] <- sum(cacc_matrix$N_Break) # Sum `N_Break`
  
  # Calculate the cumulative sum of `N_Break`
  cacc_matrix$N_Break_D <- 0 # Initialise column
  
  for(i in 1:N){
    if(i == 1){
      cacc_matrix$N_Break_D[i] <- cacc_matrix$N_Break[i]
      next
    }
    cacc_matrix$N_Break_D[i] <- cacc_matrix$N_Break_D[i - 1] - cacc_matrix$N_Break[i]
  }
  
  # %CFD of N_Break_D
  maxAcumulative <- cacc_matrix$N_Break[1]
  cacc_matrix$p_N_Break_D <- cacc_matrix$N_Break_D/maxAcumulative
  
  # Create a column for the case configurations
  vector <- seq(from = 1, to = 0, length.out = N + 1)  # Create a vector of zeros
  cacc_matrix$Config <- vector
  
  # Number of case configurations
  vector <- N:0  # Create a vector for the total number of observations
  cacc_matrix$num_Config <- vector
  
  # %CDF of case configurations
  cacc_matrix$p_Configs <- cacc_matrix$num_Config/N
    
  # Calculate the area under the Lorenz Curve
  cacc_matrix$L_Curve <- 0 # Initialise column
  
  for (i in 1:N){
    cacc_matrix$L_Curve[i] <- ((cacc_matrix$p_N_Break_D[i + 1] + cacc_matrix$p_N_Break_D[i]) / 2) * (1 / N)
  }
    
  return (cacc_matrix)
}

### Create a function to calculate the SCI ###
sci <- function(dataset, x, y){
  # First, calculate the CACC matrix
  # Search for the `N_Break` in the data. If `N_Break` does not exist, it means that the CACC matrix was already calculated
  if(("N_Break" %in% names(dataset)) == FALSE){
    # DV
    x = colnames(dataset[, - ncol(dataset)])
    # IV
    y = colnames(dataset[, ncol(dataset)])
    # The CACC matrix
    cacc_matrix <- cacc(dataset)
  } else {
    cacc_matrix <- dataset
    # DV
    x = colnames(dataset[, !names(dataset) %in% c("N_Break", "p")])
    # IV
    y = colnames(dataset[, ncol(dataset)])
  }
  
  # If the dataset used does not have the necessary variables to calculate the Lorenz Curve, the following variables are calculated
  if(("N_Break_D" %in% names(cacc_matrix)) == FALSE){
    # Prepare the CACC matrix to calculate the Lorenz Curve
    cacc_matrix <- data_prepare(cacc_matrix)
  }
  
  # Calculate the area under the curve
  # Sum `L_Curve`
  area_L_Curve <- sum(cacc_matrix$L_Curve)
  
  # Area A
  A <- 0.5 - area_L_Curve
  
  # Area B
  B <- area_L_Curve / 0.5
  
  SCI <- 1 - B
  
  return (SCI)
}

### Create a function to plot the Lorenz Curve and display the SCI ###
gg_lorenz_curve <- function(dataset, x, y){
  # First, calculate the CACC matrix
  # Search for the `N_Break` in the data. If `N_Break` does not exist, it means that the CACC matrix was already calculated
  if(("N_Break" %in% names(dataset)) == FALSE){
    # DV
    x = colnames(dataset[, - ncol(dataset)])
    # IV
    y = colnames(dataset[, ncol(dataset)])
    # The CACC matrix
    cacc_matrix <- cacc(dataset)
  } else {
    cacc_matrix <- dataset
    # DV
    x = colnames(dataset[, !names(dataset) %in% c("N_Break", "p")])
    # IV
    y = colnames(dataset[, ncol(dataset)])
  }
  
  # If the dataset used does not have the necessary variables to calculate the Lorenz Curve, the following variables are calculated
  if(("N_Break_D" %in% names(cacc_matrix)) == FALSE){
    # Prepare the CACC matrix to calculate the Lorenz Curve
    cacc_matrix <- data_prepare(cacc_matrix)
  }
  
  # Plot the Lorenz Curve that displays the SCI
  ggplot2::ggplot(data = cacc_matrix,
                  mapping = aes(x = Config,
                                y = p_N_Break_D)) +
    # ggplot2::ggtitle("Lorenz Curve") +
    ggplot2::geom_area() +
    ggplot2::scale_x_continuous(name = "Proportion of observations",
                                limits = c(0, 1),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "Proportion of dominant profiles",
                                limits = c(0, 1),
                                expand = c(0, 0)) +
    ggplot2::geom_abline() +
    ggplot2::annotate(geom = "text",
                      x = min(cacc_matrix$Config) + 0.2,
                      y = max(cacc_matrix$p_N_Break_D) - 0.1,
                      label = paste("SCI = ", round(sci(cacc_matrix), digits = 3)),
                      size = 5) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
                   plot.margin = unit(x = c(.15, .2, .15, .15),
                                      units = "in"))
}
