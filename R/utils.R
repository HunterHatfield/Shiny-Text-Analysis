                               
                               ############
                      ############################
          #################################################### 
           # utils.R - helper functions for Text Analysis App 
          ####################################################
                       ###########################
                               ############

### join_secondary function ####
# Perform a join on a primary and secondary tibbles, given specified column names to join on in each and a join type. 
# Datasets are cleaned so that the common columns are named "Primary Common" and "Secondary Common". Only allows joining on one pair of columns. 
join_secondary <- function(content_primary, content_secondary, 
                           col_primary, col_secondary, 
                           join_type = "inner"){
  
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
  
  # Note when checking: could use {} to eval dynamic col names
  colnames(content_primary)[which(names(content_primary) == col_primary)] <- "Primary Common"
  colnames(content_secondary)[which(names(content_secondary) == col_secondary)] <- "Secondary Common"
  
  # Perform join on Primary Common and Secondary Common columns
  # based on the join type passed to function arguments
  content_joined <- {
    if(join_type == "inner"){
      
      inner_join(content_primary, content_secondary, 
                 by = c("Primary Common" = "Secondary Common"))
      
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
  
  req(rv$content) 

  rv$files <- NULL
  rv$csvtsv_file <- NULL
  rv$content <- NULL
  rv$numFiles <- nrow(rv$content) # obviously going to be 0 
  
  rv$content_primary <- NULL

  rv$content_secondary <- NULL
  rv$secondary_file <- NULL

  rv$content_stop_rm <- NULL
  rv$content_parameterised <- NULL
  rv$content_prepared <- NULL
  rv$content_edited <- NULL
  rv$is_tokenised <- NULL
  rv$is_stop_rm <- NULL

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

advanced_mutate <- function(content_prepared_in, 
                            in_datatable,
                            mutate_option = "mutate_new",
                            mutate_new_col_name = NULL,
                            mutate_col_to_update = NULL,
                            mutate_advanced_condition_col,
                            mutate_advanced_condition = "is equal to",
                            mutate_advanced_condition_numeric_input = NULL,
                            mutate_advanced_condition_text_input = NULL,
                            mutate_advanced_equals_true = TRUE,
                            mutate_advanced_equals_false = NULL
) {
  
  # Saving only rows which are in the datatable to be edited
  # Need to join rows not in DT at end 
  content_prepared <- content_prepared_in[which(in_datatable$ID %in% 
                           content_prepared_in$ID),]
  
  
  # Doing all numeric or string checking & conversion first, 
  # then actually updating after

  # If condition is for numeric checking, attempt conversion of
  # column and condition input to numeric
  # Could just convert all to strings, however later if statistical
  # testing to be perform columns must retain numeric states
  if(mutate_advanced_condition %in% c("is less than", 
                                      "is greater than",
                                      "is equal to (numeric)")){

    # Attempting to convert col to numeric
    convert_attempt_col <- try(
      content_prepared[mutate_advanced_condition_col] <- 
        as.numeric(content_prepared[[mutate_advanced_condition_col]])
    )
    convert_attempt_condition <- try(
      mutate_advanced_condition_numeric_input <- 
        as.numeric(mutate_advanced_condition_numeric_input)
    )
    
    print("Converted numeric condition:")
    print(mutate_advanced_condition_numeric_input)

    # If the try-catch produced error (class try-error)  or
    # produced NAs by coercion when trying to convert column or 
    # condition input to numeric, show alert
    if("try-error" %in% class(convert_attempt_col) ||
       "try-error" %in% class(convert_attempt_condition) ||
       is_empty(mutate_advanced_condition_numeric_input) ||
       is.na(mutate_advanced_condition_numeric_input) ||
       NA %in% convert_attempt_col){
      
      # Alert if something isn't numeric / NAs produced
      shinyalert(
        title = "Update failed: conversion error",
        text = "Updating based on numeric operators (<, >, =) takes a numeric value and compares it to the selected column. \n \n Ensure both your selected column and inputted condition are numeric values. \n \n Alternatively, try string matching or contains to compare characters/strings.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
        )
      
      return(content_prepared_in)
      
    }
    
    # end numeric conversion checking
  } else { # else ensure string methods like string matching and contains 
    # have condition input and condition column convertible to strings
    
    # Attempting to convert col to char
    print("Printing content_prepared[mutate_advanced_condition_col]")
    print(content_prepared[mutate_advanced_condition_col])
    
    convert_attempt_col_text <- try(
      content_prepared[mutate_advanced_condition_col] <- 
        as.character(content_prepared[[mutate_advanced_condition_col]])
        #as.character(content_prepared[mutate_advanced_condition_col])
      )
    print("Printing content_prepared[mutate_advanced_condition_col] after conversion")
    print(content_prepared[mutate_advanced_condition_col])
    
    print("Printing mutate_advanced_condition_text_input")
    print(mutate_advanced_condition_text_input)
    convert_attempt_condition_text <- try(
      mutate_advanced_condition_text_input <- 
        as.character(mutate_advanced_condition_text_input)
      )
    
    print("converted string condition:")
    print(mutate_advanced_condition_text_input)
    
    # If the try-catch produced error (class try-error)  or
    # produced NAs by coercion when trying to convert column or 
    # condition input to char, show alert
    if("try-error" %in% class(convert_attempt_col_text) ||
       "try-error" %in% class(convert_attempt_condition_text) ||
       is_empty(mutate_advanced_condition_text_input) ||
       is.na(mutate_advanced_condition_text_input) ||
       NA %in% convert_attempt_col_text){
      
      # Alert if something isn't convertible / NAs produced
      shinyalert(
        title = "Update failed: conversion error",
        text = "Updating using strings takes a string and compares it to strings in the selected column. \n \n Ensure both your selected column and inputted condition are convertible to strings/character vectors.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      
      return(content_prepared_in)
    } # end alert check
    
  } # end string conversion checking
    

  #### Actually mutating #####
  
  # Where mutate update option is selected
  if(mutate_option == "mutate_update") { 
    
    # for each row in data to update
    # for(i in seq_len(nrow(content_prepared[mutate_col_to_update]))){

      # If checking if less than...
      if(mutate_advanced_condition == "is less than"){

        print("Mutate update advanced: checking is less than...")

        # For each row in mutate_advanced_condition_col:
          # if condition true: input true value provided by user, 
          # else condition false: input false value *if* it is 
          # provided by user. 
        # Thus if condition fails and no false value provided, row not 
        # changed
        # Same logic for each numeric case
        
        # for each row in data to update
        for(i in seq_len(nrow(content_prepared[mutate_col_to_update]))){

          # If condition true, input true value provided by user
          if(content_prepared[[i, mutate_advanced_condition_col]] <
             mutate_advanced_condition_numeric_input){
            
            content_prepared[[mutate_col_to_update]][[i]] <-
              mutate_advanced_equals_true
            
          } else if(!is_empty(mutate_advanced_equals_false) && # || 
                    mutate_advanced_equals_false != ""){ 
            # if condition fails, input false value if not it is 
            # not empty - so if false value is empty then do nothing 
              content_prepared[[mutate_col_to_update]][[i]] <-
                mutate_advanced_equals_false
          }
          
        } # end for loop
          
        # Else if checking if greater than...
      } else if(mutate_advanced_condition == "is greater than"){
        # content_prepared[i, mutate_col_to_update] <- 
        #   if_else(content_prepared[i, mutate_advanced_condition_col] > 
        #             mutate_advanced_condition_numeric_input, 
        #           mutate_advanced_equals_true, 
        #           mutate_advanced_equals_false)
        
        # for each row in data to update
        for(i in seq_len(nrow(content_prepared[mutate_col_to_update]))){
        
          if(content_prepared[[i, mutate_advanced_condition_col]] >
             mutate_advanced_condition_numeric_input){
            
            content_prepared[[mutate_col_to_update]][[i]] <-
              mutate_advanced_equals_true
            
          } else if(!is_empty(mutate_advanced_equals_false) && 
                    mutate_advanced_equals_false != ""){ 
            # if condition fails and false input is not empty
              content_prepared[[mutate_col_to_update]][[i]] <-
                mutate_advanced_equals_false
          }
          
        } # end for loop
          
      } else if(mutate_advanced_condition == "is equal to (numeric)"){
        
          print("Mutate update advanced: checking is equal to...")
        
        # for each row in data to update
        for(i in seq_len(nrow(content_prepared[mutate_col_to_update]))){
  
          # If condition true, input true value provided by user
          if(content_prepared[[i, mutate_advanced_condition_col]] ==
             mutate_advanced_condition_numeric_input){
            
            content_prepared[[mutate_col_to_update]][[i]] <-
              mutate_advanced_equals_true
            
          } else if(!is_empty(mutate_advanced_equals_false) &&
                    mutate_advanced_equals_false != "") {
            content_prepared[[mutate_col_to_update]][[i]] <-
                mutate_advanced_equals_false
          }
          
        } # end for loop
        
       # end is equal to numeric checking
        
      } else if(mutate_advanced_condition == "matches string"){
        
          print("Matches string case")
        
        # for each row in data to update
        for(i in seq_len(nrow(content_prepared[mutate_col_to_update]))){
          
          # If condition true, input true value provided by user
          # Used == for exact and quick string matching
          if(content_prepared[[i, mutate_advanced_condition_col]] ==
             mutate_advanced_condition_text_input){
            
            content_prepared[[mutate_col_to_update]][[i]] <-
              mutate_advanced_equals_true
            
          } else if(!is_empty(mutate_advanced_equals_false) && 
                    mutate_advanced_equals_false != "") {
            
            # Else input the false value provided by user if 
            # false condition isn't empty
            content_prepared[[mutate_col_to_update]][[i]] <-
              mutate_advanced_equals_false
          }
          
        } # end for loop
        
      } else if(mutate_advanced_condition == "contains"){
        
        print("Contains string case")
        
        # for each row in data to update
        for(i in seq_len(nrow(content_prepared[mutate_col_to_update]))){
          
         # If condition true, input true value provided by user
          # Using grepl to check if any instances of the input string is
          # found in each row of the selected data
          # grepl returns a logical vector (match or not for 
          # each element)
          if(grepl(mutate_advanced_condition_text_input,
                    content_prepared[[i,mutate_advanced_condition_col]])){
            
            content_prepared[[mutate_col_to_update]][[i]] <-
              mutate_advanced_equals_true
            
          } else if(!is_empty(mutate_advanced_equals_false) || 
                    mutate_advanced_equals_false != "") {
            
              content_prepared[[mutate_col_to_update]][[i]] <-
                mutate_advanced_equals_false
            
        } # end else case
        
        } # end for loop
        
      } # end mutate update contains logic

    # } # end for loop

    print("Result of advanced_update function:")
    print(content_prepared)
    # return(content_prepared[mutate_col_to_update])
    return(content_prepared)
    
  } # end if mutate_update logic
  
  # Where advanced add new col selected
  if(mutate_option == "mutate_new"){
    
    print("advanced add new selected")
    
    # For each row in mutate_advanced_condition_col, if condition true
    # input true value provided by user, else is condition fails input
    # false value
  
    # Using vector-based comparisons to generate vector of boolean 
    # indicating if condition met for each row, which is then used 
    # to mutate
    if(mutate_advanced_condition == "is less than"){
      
      is_condition_met <- 
        content_prepared[[mutate_advanced_condition_col]] <
        mutate_advanced_condition_numeric_input
      
    } else if(mutate_advanced_condition == "is greater than"){
      
      is_condition_met <- 
        content_prepared[[mutate_advanced_condition_col]] >
        mutate_advanced_condition_numeric_input
      
    } else if(mutate_advanced_condition == "is equal to (numeric)"){
      
      is_condition_met <- 
        content_prepared[[mutate_advanced_condition_col]] ==
        mutate_advanced_condition_numeric_input
      
    } else if(mutate_advanced_condition == "matches string"){
      
      is_condition_met <- 
        content_prepared[[mutate_advanced_condition_col]] ==
        mutate_advanced_condition_text_input
      
      
    } else if(mutate_advanced_condition == "contains"){
      
      is_condition_met <- 
        grepl(mutate_advanced_condition_text_input,
              content_prepared[[mutate_advanced_condition_col]])
      
    }
    
    # Adding new column named as user input
    content_prepared <- content_prepared %>%
      mutate("{mutate_new_col_name}" := 
               if_else(is_condition_met,
                       mutate_advanced_equals_true,
                       mutate_advanced_equals_false))
      

    print("Result of advanced_update function:")
    print(content_prepared)
    return(content_prepared)
    
  } # end mutate option add new logic
  
  
} # end advanced function



#######################################
#### apply_transformation function ####
#######################################
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




