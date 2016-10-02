#' Read in multiple files.
#'
#' Based on code from: http://www.brodrigues.co/2016/07/26/read-a-lot-of-datasets-at-once-with-r
#'
#' @param list_of_datasets A list of file names with paths
#' @param read_func The import function
#' @param output Can be "list" or "global". If set to list the output is a list of objects, if "global" the output tables are created in the global namespace. Default is list.
#'
#' @export
#' @return
#' A list of, or multiple data.frames of the imported files.
#'
#' @examples
#' read_list(file_list, read.csv())
#' read_list(file_list, read.csv(), output = "list")
#' read_list(file_list, read.csv(), output = "global")
read_list <- function(list_of_datasets, read_func, output = "list"){

  read_and_assign <- function(dataset, read_func, output){
    if (output == "list") {
      dataset_name <- as.name(dataset)
      dataset_name <- read_func(dataset)
    } else if (output <- "global"){
      assign(dataset, read_func(dataset), envir = .GlobalEnv)
    } else {
      return("Wrong output type, set `list` or `global`")
    }
  }

  # invisible is used to suppress the unneeded output
  output <- invisible(
    sapply(list_of_datasets,
           read_and_assign, read_func = read_func, simplify = FALSE, USE.NAMES = TRUE))

  # Remove the extension at the end of the data set names
  names_of_datasets <- c(unlist(strsplit(list_of_datasets, "[.]"))[c(T, F)])
  names(output) <- names_of_datasets
  return(output)
}
