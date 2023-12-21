
                          ############
                  ############################
      #################################################### 
       # utils.R - helper functions for Text Analysis App 
      ####################################################
                  ###########################
                          ############

                      
                              
################################
### join_secondary function ####
################################
# Perform a join on a primary and secondary tibbles, given specified column names to join on in each and a join type. 
# Datasets are cleaned so that the common columns are named "Primary Common" and "Secondary Common". Only allows joining on one pair of columns. 
join_secondary <- function(content_primary, content_secondary, 
                           col_primary, col_secondary, 
                           join_type = "inner"){
  
  # If either of the common columns specified don't exist, return NULL
  # This is kind of impossible given that only colnames can be present, 
  # but good to cover a potential vulnerability
  # if(is.null(which(names(content_primary) == col_primary)) ||
  #    is.null(which(names(content_secondary) == col_secondary))){
  #   print("Cols not present")
  #   return(NULL)
  # }
  
  # First clean column names
  # Set the name of the common cols selected to Primary Common 
  # and Secondary Common
  # This is done due to the by argument not receiving 
  # objects/variables in lieu of a column name.
  
  # Perform join on Primary Common and Secondary Common columns
  # based on the join type passed to function arguments
  content_joined <- {
    if(join_type == "inner"){
      inner_join(content_primary, content_secondary, 
                 by = setNames(col_secondary, col_primary))
    } else if(join_type == "full"){
      full_join(content_primary, content_secondary, 
                by = setNames(col_secondary, col_primary))
    } else if(join_type == "left"){
      left_join(content_primary, content_secondary, 
                by = setNames(col_secondary, col_primary))
    } else if(join_type == "right"){
      right_join(content_primary, content_secondary, 
                 by = setNames(col_secondary, col_primary))
      
    }
  }
  
  return(content_joined)
  
}

#############################
### clear_files function ####
#############################
# Function to clear reactive value lists when user clears files 
# Resets rv list values to NULL inorder to reset downstream graphs & widgets
clear_reactives <- function(){
  
  rv$files <- NULL
  rv$csvtsv_file <- NULL
  rv$content <- NULL
  rv$numFiles <- nrow(rv$content) # obviously going to be 0 
  
  rv$content_primary <- NULL
  rv$content_to_visualise <- NULL
  rv$content_to_visualise_DT <- NULL
  
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
                            mutate_advanced_condition = "is equal to (numeric)",
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
    
    convert_attempt_col_text <- try(
      content_prepared[mutate_advanced_condition_col] <- 
        as.character(content_prepared[[mutate_advanced_condition_col]])
    )
    convert_attempt_condition_text <- try(
      mutate_advanced_condition_text_input <- 
        as.character(mutate_advanced_condition_text_input)
    )
    
    # If the try-catch produced error (class try-error) show alert
    if("try-error" %in% class(convert_attempt_col_text) ||
       "try-error" %in% class(convert_attempt_condition_text) ||
       is_empty(mutate_advanced_condition_text_input) ||
       is.na(mutate_advanced_condition_text_input) ||
       NA %in% convert_attempt_col_text){
      
      shinyalert(
        title = "Update failed: conversion error",
        text = "Updating with strings compares the strings in the selected column and inputted condition. \n \n Ensure both your selected column and inputted condition are convertible to strings/character vectors.",
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
    
    # If checking if less than...
    if(mutate_advanced_condition == "is less than"){
      
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

    return(content_prepared)
    
  } # end if mutate_update logic
  
  
  #### Where advanced add new col selected
  if(mutate_option == "mutate_new"){
    
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

    return(content_prepared)
    
  } # end mutate option add new logic
} # end advanced function


####################################
#### remove_stop_words function ####
####################################
# Function which takes a dataset, column to remove stop-words from and 
# final stop words and returns stop-removed dataset
# Removing stop words from content by first unnesting into words
# then anti-joining with the stop words final tibble.
# Then grouped by ID to stitch words back together.
# Doing separately to other columns in content so they are not lost in grouping
remove_stop_words <- function(data, col_name, stop_words, 
                              is_tokenised = FALSE){
  
  # Check that supplied column exists in data and is chars
  req(c("ID", col_name) %in% colnames(data))
  req(is.character(data[[col_name]]))
  
  # Creating subset of un-needed cols
  # data_other_cols <- data %>%
  #   dplyr::select(-{col_name})

  # If inputted data and data un-nested as words are same length, 
  # inputted data was already word-tokenised, so doesn't
  # need to be flattened and can just anti-join and return
  data_words <- data %>%
    # dplyr::select(ID, {col_name}) %>%
    unnest_tokens(!!col_name, {col_name}, token = "words")

  if(nrow(data_words) == nrow(data)){
    print("Data is already word-tokenised")
    
    # changing colnames of stop-words df to col_name supplied
    # since anti_join wouldn't accept default 'word' name
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    col_name_random <- sprintf("column_%s", timestamp)
    colnames(stop_words) <- col_name_random
    colnames(data_words)[which(colnames(data_words) == {col_name})] =
      col_name_random
    
    # Anti-join with stop-word list
    data_stop_rm <- dplyr::anti_join(data_words, stop_words, 
                                     by = {col_name_random})
    
    colnames(data_stop_rm)[
      which(colnames(data_stop_rm) == {col_name_random})
      ] = col_name

    return(data_stop_rm)
    
    # else the data was not word-tokenised
  } else {
    
    # Convert stop words to list with as.list & flatten to 1D with unlist
    stop_words_list <- unlist(as.list(stop_words[['word']]), 
                              recursive = TRUE)
    
    # Ensuring data is UTF-8 encoded
    data[[{col_name}]] <- iconv(data[[{col_name}]], to = "UTF-8", sub = "byte")
    
    # converted selected col to a Corpus object using the VectorSource 
    # function, use tm_map to transform to lower and remove puncuation
    # then remove stop-words and clean whitespace etc.
    corpus <- Corpus(VectorSource(data[[{col_name}]]))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus_stop_rm <- tm_map(corpus, removeWords, stop_words_list)
    
    cleaned_text <- sapply(corpus_stop_rm, function(x) {
      PlainTextDocument(stripWhitespace(as.character(x)))
    })
    
    # Then replacing original text with stop-removed text
    data_stop_rm <- data
    for(i in 1:nrow(data_stop_rm)){
      data_stop_rm[[i, {col_name}]] <- cleaned_text[, i]$content
    }

    return(data_stop_rm)
  }
  
  # Reverting text data back into paragraphs by flattening, 
  # otherwise data will return as word-tokenised.
  # data_stop_rm <- data_stop_rm %>%
  #   group_by(ID) %>%
  #   summarize("{col_name}" := 
  #               str_flatten(!!sym(col_name_random), 
  #                           collapse = " ")) %>%
  #   ungroup()
  
  # Then joining stop-removed column and rest of cols
  # data_stop_rm <- inner_join(data_stop_rm,
  #                           data_other_cols, by = c("ID", "Contents"))
  
  # If data not tokenised, select just ID and column, unnest to words, 
  # anti-join with stop_word data, and re-flatten data to be chunk of 
  # text per row. Finally, add other extra columns with full_join
  # if(!is_tokenised){
  #   print("Data not tokenised")
  # 
  # }
  # Else if tokenised, tokenise into words and anti-join
  # entire dataset with stop_words provided
  # provided by user
  # else {

    # data <- data %>%
    #   unnest_tokens(!!col_name, {col_name}, token = "words")


  # }
  
  # Final stop-removed dataset
  return(data_stop_rm)
}

################################
#### tokenise data function ####
################################
# Function which takes a data, column to tokenise, 
# and a token and returns the dataset with the 
# selected column tokenised
tokenise_data <- function(data, col_name, token, 
                          custom_token = NULL, n_grams = 2){
  
  # Check that supplied column exists in data and is chars
  req(c("ID", col_name) %in% colnames(data))
  
  ##### In try catch loop
  # If contents uploaded are not character, attempt to convert
  if (!is.character(data[[col_name]])) {
    print("attempting to convert to char to tokenise")
    data[[col_name]] <- as.character(data[[col_name]])
    print(data[col_name])
  }
  
  # First need to mash together
  
  # Actually tokenising
  if (token == "words" || token == "sentences") {
    
    data_tokenised <- data %>%
      unnest_tokens(!!col_name, {col_name}, token = token)
    
  } else if (token == "bigrams") {
    
    data_tokenised <- data %>%
      unnest_ngrams(!!col_name, {col_name}, n = 2)
    
  } else if (token == "ngrams") {
    
    data_tokenised <- data %>%
      unnest_ngrams(!!col_name, {col_name}, n = n_grams)
    
  } else if (token == "other") {
    
    # unnest on the user inputted regex
    data_tokenised <- data %>%
      unnest_regex(!!col_name, {col_name},
                   pattern = custom_token)
  }
  
  data_tokenised <- data_tokenised %>%
    relocate({col_name}, .after = ID)
  
  return(data_tokenised)
}


################################
#### stemming data function ####
################################
# Function which takes a dataset, column to stem, 
# which is assumed tokenised, and returns dataset with the 
# selected column stemmed
stem_data <- function(data, col_name){
  
  # Check that supplied column exists in data and is chars
  req(col_name %in% colnames(data))
  req(is.character(data[[col_name]]))
  
  # Perform stemming on selected column
  # hunspell_stem gives possible list of stems,
  # separating these and deselcting Token column
  data_stemmed <- data %>%
    mutate(Stem = hunspell_stem(data[[col_name]])) %>%
    separate(Stem, c("Stem_1", "Stem_2"),
             sep = ",") %>%
    mutate(Stem_1 = unlist(Stem_1)) %>%
    filter(Stem_1 != "character(0)") %>%
    mutate(Stem_1 = gsub('c\\(\\"|\\"', "", Stem_1)) %>%
    mutate("{col_name}" := Stem_1) %>%
    select(-Stem_1, -Stem_2)
  
  return(data_stemmed)

}


#######################################
#### check sufficiently tokenised #####
#######################################
# Returns a logical value indicating whether or not
# a supplied dataset has 1-3 tokens/words per row.
# Takes a dataset and column name to check, specified column is then
# tokenised into words to contain one token per row. 
# If this one-token-per-row dataset is greater than 3 times the length
# of the original dataset, the original dataset must have more than 3
# words in one or more rows.
# Also can check if tokenised into words
tokenised_enough <- function(data, col_name, 
                             check_if_words = FALSE){
  
  data_tokenised <- data %>%
    unnest_tokens(!!col_name, {col_name}, token = "words")
  
  if(check_if_words){
    ifelse(nrow(data_tokenised) != nrow(data), 
           return(FALSE), return(TRUE))
  }
  
  ifelse(nrow(data_tokenised) > 3 * nrow(data), 
         return(FALSE), return(TRUE))
}


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



