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