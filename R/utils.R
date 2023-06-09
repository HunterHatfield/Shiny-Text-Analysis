
#################### utils.R - contains helper functions ####################

### join_secondary function ####
# Perform a join on a primary and secondary tibbles, given specified column names to join on in each and a join type. 
# Datasets are cleaned so that the common columns are named "Primary Common" and "Secondary Common". Only allows joining on one pair of columns. 

join_secondary <- function(content_primary, content_secondary, 
                           col_primary, col_secondary, join_type = "inner"){
  
  # If either of the common columns specified don't exist, return NULL
  # This is kind of impossible given that only colnames can be present, 
  # but good to cover a potential vulnerability
  if(is.null(which(names(content_primary) == col_primary)) ||
     is.null(which(names(content_secondary) == col_secondary))){
    print("Cols not present")
    return(NULL)
  }
  
  # First clean column names
  # Set the name of the common cols selected to Primary Common 
  # and Secondary Common
  # This is done due to the by argument not receiving 
  # objects/variables in lieu of a column name.
  colnames(content_primary)[which(names(content_primary) == col_primary)] <- "Primary Common"
  colnames(content_secondary)[which(names(content_secondary) == col_secondary)] <- "Secondary Common"
  
  # Perform join on Primary Common and Secondary Common columns
  # based on the join type passed to function arguments
  content_joined <- {
    if(join_type == "inner"){
      
      inner_join(content_primary, content_secondary, 
                 by = c('Primary Common' = 'Secondary Common'))
      
    } else if(join_type == "full"){
      
      full_join(content_primary, content_secondary, 
                by = c('Primary Common' = 'Secondary Common'))
      
    } else if(join_type == "left"){
      
      left_join(content_primary, content_secondary, 
                by = c('Primary Common' = 'Secondary Common'))
      
    } else if(join_type == "right"){
      
      right_join(content_primary, content_secondary, 
                 by = c('Primary Common' = 'Secondary Common'))
      
    }
  }
  
  # Finally resetting the column name "Primary Common" back to original
  colnames(content_joined)[which(names(content_joined) == "Primary Common")] <- col_primary
  
  return(content_joined)
  
}


#### clear_files function ####
# Function to clear reactive value lists when user clears files 
# Resets rv list values to NULL inorder to reset downstream graphs & widgets
clear_reactives <- function(){
  
  rv$files <- NULL
  rv$csvtsv_file <- NULL
  rv$content <- NULL
  rv$numFiles <- nrow(rv$content) # obviously going to be 0 
  
  rv$content_secondary <- NULL
  rv$secondary_file <- NULL
  
  rv$content_stop_rm <- NULL
  rv$content_parameterised <- NULL
  
  rv$content_freq <- NULL
  rv$content_tf_idf <- NULL
  rv$content_zipf <- NULL
  
  rv$content_stats <- NULL
  
  mini_rv <- NULL
  report_rv <- NULL
  
}



#####################################
###### Advanced mutate function #####
#####################################

advanced_mutate <- function(content_prepared, 
                            in_datatable,
                            mutate_option = "mutate_new",
                            mutate_update_col,
                            mutate_advanced_update_col,
                            mutate_advanced_condition = "is equal to",
                            mutate_advanced_condition_input,
                            mutate_advanced_equals_true = TRUE,
                            mutate_advanced_equals_false = FALSE
) {
  
  content_prepared <- content_prepared[which(in_datatable$ID %in% 
                           content_prepared$ID),]
  
  if(mutate_option == "mutate_update"){
    
    
    # Less than & greater than tests
    if(mutate_advanced_condition == "is less than" ||
       mutate_advanced_condition == "is greater than"){
      
      print("Mutate updated advanced: attempting numeric conv")
      # Attempting to convert col to numeric
      convert_attempt_col <- try(
        content_prepared[mutate_advanced_update_col] <- 
          as.numeric(content_prepared[[mutate_advanced_update_col]])
      )
      convert_attempt_condition <- try(
        mutate_advanced_condition_input <- 
          as.numeric(mutate_advanced_condition_input)
      )
      
      # If the try-catch produced error (class try-error) in trying to convert
      # the column or condition input to numeric, show alert
      if("try-error" %in% class(convert_attempt_col) ||
         "try-error" %in% class(convert_attempt_condition)){
        
        # produce alert
        print("something wasn't numeric")
        
      } else { # else perform the check & update col accordingly
        
        print("Mutate update advanced: numeric conversion success")
        print("Mutate update advanced: going to for loop to update...")
        
        for(i in 1:nrow(content_prepared[mutate_update_col])){
          
          # If checking if less than...
          if(mutate_advanced_condition == "is less than"){
            
            print("Mutate update advanced: checking is less than...")
            
            content_prepared[i, mutate_update_col] <- 
              if_else(content_prepared[i, mutate_advanced_update_col] < 
                        mutate_advanced_condition_input, 
                      mutate_advanced_equals_true, 
                      mutate_advanced_equals_false)
            
            # Else if checking if greater than...
          } else if(mutate_advanced_condition == "is greater than"){
            content_prepared[i, mutate_update_col] <- 
              if_else(content_prepared[i, mutate_advanced_update_col] > 
                        mutate_advanced_condition_input, 
                      mutate_advanced_equals_true, 
                      mutate_advanced_equals_false)
          }
          
        } # end for loop
        
        print("Result of advanced_update function:")
        print(content_prepared)
        return(content_prepared[mutate_update_col])
        #return(content_prepared)
        
      } # end else statement
      
    } # end less or greater than mutations
    
  } # end if mutate update
  
} # end advanced function



#### apply_transformation function ####
# This function takes in a variable from a dataset and transformation type to 
# returns the transformed variable. 
# transformation argument can include: log, sqrt, arcsine, recip
apply_transformation <- function(var, transformation){
  
  transformed_var <- switch(transformation,
                        "none" = var,
                        "log" = log(var + 1), 
                        "sqrt" = sqrt(var), 
                        "arcsine" = asin(sqrt(var)),
                        "recip" = 1/var
                      )
  
  return(transformed_var)
  
}

#### formula function ####
# Takes a list of dependent and list of independent variables and returns a formula
# ready to pass to lm/glm 
# Old formula making code
# formula <- paste0(input$dependent_linear, " ~ ")
# for(i in input$independent_linear){
#   if(i == input$independent_linear[[1]]){
#     formula <- paste0(formula, input$independent_linear[[1]])
#   } else {
#     formula <- paste0(formula, " + ", i)
#   }
# }
# formula
formula <- function(regression_type, dependent, independent, 
                    interactions = NULL, random_slope_1 = NULL, 
                    random_intercept_1 = NULL, 
                    random_slope_2 = NULL, random_intercept_2 = NULL){

  # First create formula with dependent in front with "~"
  formula <- paste0(dependent, " ~ ")
  
  # Combining list of independents into a string 
  independents <- paste(independent, collapse = " + ")
  
  # Adding independents to the formula
  formula <- paste0(formula, independents)

  # if interactions are present then add them to formula before returning
  if(!is.null(interactions)){
    # Combining list of interactions into a string 
    interactions_formula <- paste(interactions, collapse = "*")
    formula <- paste0(formula, " + ", interactions_formula)
  } 
  
  # If mixed regression selected, first check if random slopes added, 
  # if not just 1 | intercept for the random formula
  if(regression_type == "mixed"){
    randoms_formula_1 <- ifelse(length(random_slope_1) == 0, 
                              paste("1"),
                              paste(random_slope_1, collapse = " + "))
    mixed_formula_1 <- paste("(", randoms_formula_1, "|", random_intercept_1, ")")
    
    # Checking if a secondary term selected
    if(length(random_intercept_2) > 0){
      randoms_formula_2 <- ifelse(length(random_slope_2) == 0, 
                                  paste("1"),
                                  paste(random_slope_2, collapse = " + "))
      mixed_formula_2 <- paste("(", randoms_formula_2, "|", random_intercept_2, ")")
      
      return(paste(formula, " + ", mixed_formula_1, " + ", mixed_formula_2))
      
    } else {
      
      return(paste(formula, " + ", mixed_formula_1))
    }

  } 
  
  return(formula)
}

### perform_regression function ####
# A function to perform regression and return the regression object, given 
# a regression formula and type of regression to perform. Regression types
# can be 'linear', 'logistic', or 'poisson' 
perform_regression <- function(formula, regression_type, data){
  
  # depending on regression type, perform either linear, logisitic or poisson
  if(regression_type == "linear"){
    res <- lm(formula, data)
  } else if(regression_type == "logistic"){
    res <- glm(formula, family = binomial(link = "logit"), data)
  } else if(regression_type == "poisson"){
    res <- glm(formula, family = poisson(link = "log"), data)
  } else if(regression_type == "mixed"){
    res <- lme4::lmer(formula, data)
  }

  return(res)
}




