#Calculate p-values for tables
#https://cran.r-project.org/web/packages/tryCatchLog/vignettes/tryCatchLog-intro.html
#https://www.r-bloggers.com/2020/10/basic-error-handing-in-r-with-trycatch/

calculate_p_value <- function(factor_var, response_var, data) {
  response_var<- data[[response_var]]
  factor_var<-data[[factor_var]]
  
  print(length(response_var))
  print(length(factor_var))
  # Contingency table
  contingency_table <- table(factor_var, response_var)
  
  # Chi-square test
  chi_square_result <- tryCatch({
    chisq.test(contingency_table)
  }, error = function(e) {
    NULL
  })
  
  # Fisher's exact test
  fisher_result <- tryCatch({
    fisher.test(contingency_table)
  }, error = function(e) {
    NULL
  })
  
  # If chi-square test fails or expected cell counts are less than 5, use Fisher's exact test
  if (!is.null(chi_square_result)) {
    return(chi_square_result$p.value)
  } else if (!is.null(fisher_result)) {
    return(fisher_result$p.value)
  } else {
    # Simulate chi-square test
    sim_chi_square_result <- tryCatch({
      chisq.test(contingency_table, simulate.p.value = TRUE)
    }, error = function(e) {
      NULL
    })
    if (!is.null(sim_chi_square_result)) {
      return(sim_chi_square_result$p.value)
    } else {
      return(NA) #couldn't get a p-value-- something else if happening --> check
    }
  }
}



