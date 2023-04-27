templates <- function(file) {
  #' @name templates
  #' @title Templates and examples
  #'
  #' @description Provides the path or direct access to built-in templates and examples.
  #'
  #' @param file Template/example file name. Please check the file list and description below.
  #'
  #' @note
  #'
  #' It is important to point out that the databases and simulations included as an example
  #' in the package are only intended to support the understanding and application of the
  #' package's functionalities. Therefore, the numerical structure contained in these bases
  #' should not be used for applied research.
  #'
  #' Below is the list of files and the description of their contents:
  #'
  #'  -"oranig_example.har" - ORANIG CGE model database for the 2015 Brazilian economy.
  #'
  #'
  #'
  #'
  #'
  #'
  #' @examples
  #' # example code
  #'
  #' path_to_oranig <- gtaptools::templates("oranig_example.har")
  #' path_to_oranig
  #'
  #'
  #'
  #' @export


  files <- list.files(system.file("extdata", package = "gtaptools"), recursive = T)
  file <- grep(file, files)

  path <-
    file.path(system.file("extdata", package = "gtaptools"), files[file])


  return(path)
}