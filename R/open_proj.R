##' Open project directory based on name
#'
#' @param name name of project as defined in 'projects' object
#' @import data.table
#' @importFrom RecordLinkage levenshteinSim
#'
#' @examples
#' open_proj(name = "hdsr automatisatie")
#'
#' @export
#'
open_proj <- function(name){
  #copy projects
  proj <- copy(projects)

  #add simularity index
  proj$sim <- levenshteinSim(name, proj$name)

  #select one with highest similarity, open dir
  browseURL(proj[sim == max(sim)]$dir)
}
