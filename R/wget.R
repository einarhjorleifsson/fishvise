#' @title wget_sam_run
#' 
#' @description Fetches the results from the run directory on stockassessment.org
#' and stored it locally.
#' 
#' @export
#' 
#' @param dir Name of the directory on stockassessment.org
#' 
#' @note #' Currently the directory structure is also copied, see datadisk/stock...
#' 
#' TO DO: Get rid of the long directory path in the stuff returned to the
#' local computer.

wget_sam_run <- function(dir) {
  cmd_prefix <- "wget -r - l1 -nH --no-parent --user='guest' --password=guest https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/"
  cmd <- paste(cmd_prefix,dir,sep="")
  system(cmd)
}