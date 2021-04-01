require(tidyverse)
require(glue)

new_directory <- function(new_dir) {if(!dir.exists(file.path(getwd(),new_dir))){dir.create(file.path(getwd(),new_dir),recursive = TRUE)}}

amalgated_df <- function(input_list,output_file) {
  output_file = file.path(getwd(),output_file)
  if (file.exists(output_file)){
    file.remove(output_file)
  }
  for (name in names(input_list)){
    cat(name,file = output_file,sep="\n", append = TRUE)
    write_csv(input_list[[name]],file = output_file,append = TRUE,col_names = TRUE)
  }
}

squish <- function(input_list,output_file = NULL){
  for(i in names(input_list)){
      #Check if label present. TRUE = Yes
      label <- ifelse(class(input_list[[i]][[1]]) == "character",TRUE,FALSE)
      #Check the number of rows
      col_num <- ncol(input_list[[i]])

      #Glue based on the prior values
      fixed_col_num <- ifelse(label == TRUE, col_num - 1, col_num)
      if (fixed_col_num == 2){
        input_list[[i]] <- mutate(input_list[[i]], squished = glue("{input_list[[i]][[col_num - 1]]} ({input_list[[i]][[col_num]]})"))
      } else if (fixed_col_num > 2){
        input_list[[i]] <- mutate(input_list[[i]], squished = glue("{input_list[[i]][[col_num - 2]]} ({input_list[[i]][[col_num - 1]]} - {input_list[[i]][[col_num]]})"))
      } else {
        input_list[[i]] <-mutate(input_list[[i]], squished = NA)
      }
      
      #Extract the names
      if (label == TRUE){
        input_list[[i]] <- select(input_list[[i]],1,squished)
      } else {
        input_list[[i]] <-select(input_list[[i]],squished)
      }
      
      #Make the list name a column
      if (label == TRUE){
        input_list[[i]] <- mutate(input_list[[i]], name = as.character(i), .before = 1)
      } else {
        input_list[[i]] <- mutate(input_list[[i]], blank = "", .before = 1)
        input_list[[i]] <- mutate(input_list[[i]], name = as.character(i), .before = 1)
      }
  }
  print(input_list)
  
  #Save the new glued object if there is an output_file defined
  if (!is.null(output_file)){
    output_file = file.path(getwd(),output_file)
    if (file.exists(output_file)){
      file.remove(output_file)
    }
    for (name in names(input_list)){
      write_csv(input_list[[name]],file = output_file,append = TRUE,col_names = FALSE)
    }
  }
  
  #Return the input
  return(input_list)
}

