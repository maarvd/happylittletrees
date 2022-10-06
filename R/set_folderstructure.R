#' Set folder structure
#'
#' Create folders within working directory if not existing

#' @param folders Vector of folders to create
#' @examples
#' set_folderstructure(c("data", "dev", "output", "scripts"))
#'
#' @export

#set folder structure
set_folderstructure <- function(folders){
  #loop through folders, create if not present
  for(i in folders){
    if(!dir.exists(i)){
      dir.create(i)
    }
  }
}
