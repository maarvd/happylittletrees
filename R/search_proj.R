##' Search projectfolders for text in order to find the right projectnumber
#'
#' @param range range in the projectnr according to structure in Projecten (full = all)
#' @param text text used in grepl, collapses with |
#'
#' @examples
#' search_proj(range = "1800-1900", text = c("borium", "mangaan"))
#'
#' @export
#'
search_proj <- function(range, text){
  #extract user info
  username <- Sys.info()[["user"]]

  if (range != "full") {
    #tidy range
    range <- tolower(gsub(" |O|o", "", range))
    range_tidied <-
      paste0("O ", sapply(strsplit(range, "-"), `[`, 1), " - O ", sapply(strsplit(range, "-"), `[`, 2))

    #select right directory
    rel_dict <-
      paste0(
        "C:/Users/",
        username,
        "/SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI/NMISite - Documenten/Projecten/",
        range_tidied
      )

    #list dirs
    dirlist <- list.dirs(rel_dict, recursive = FALSE)

    #tidy dirlist
    dirlist2 <-
      list.dirs(rel_dict, full.names = FALSE, recursive = FALSE)
    dirlist2 <- sapply(strsplit(dirlist2, "\\/"), `[`, 1)

    #remove some projectfolders if fill
    dirlist <-
      dirlist[!grepl("Blgg|M M F|Intern|PZ|Standaard structuur", dirlist, ignore.case = TRUE)]
    dirlist2 <-
      dirlist2[!grepl("Blgg|M M F|Intern|PZ|Standaard structuur", dirlist2, ignore.case =TRUE)]

    #loop through dirs, check for text
    dir.search <- lapply(
      dirlist,
      FUN = function(x) {
        #list files
        filelist <- list.files(x, recursive = TRUE) |> tolower()

        #only select files with text
        filelist <-
          filelist[grepl(tolower(paste0(text, collapse = "|")), filelist, ignore.case = TRUE)]

        #return
        return(filelist)
      }
    )

    #add name of projectnr to list
    names(dir.search) <- dirlist2

    #remove projects where no text is found
    rel.dirs <- Filter(function(x) length(x) > 0, dir.search)
  } else if(range == "full"){
    #list all directories
    projdirs <- list.dirs(paste0("C:/Users/", username, "/SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI/NMISite - Documenten/Projecten"), recursive = FALSE)
    projdirs.short <- list.dirs(paste0("C:/Users/", username, "/SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI/NMISite - Documenten/Projecten"), recursive = FALSE, full.names = FALSE)

    #grepl rel
    projdirs <- projdirs[!grepl("Blgg|Intern|M M F|PZ|Standaard structuur", projdirs, ignore.case = TRUE)]
    projdirs.short <- projdirs.short[!grepl("Blgg|Intern|M M F|PZ|Standaard structuur", projdirs.short, ignore.case = TRUE)]

    #loop through projdirs
    rel.dirs <- lapply(1:length(projdirs), FUN = function(i){
      #list directories
      dirlist <- list.dirs(projdirs[i], recursive = FALSE, full.names = TRUE)
      dirlist.short <- list.dirs(projdirs[i], recursive = FALSE, full.names = FALSE)

      #lapply through directories
      rel.files <- lapply(dirlist, FUN = function(x){
        #list files
        filelist <- list.files(x, full.names = FALSE, recursive = TRUE) |> tolower()

        #grepl
        filelist <- filelist[grepl(tolower(paste0(text, collapse = "|")), filelist, ignore.case = TRUE)]

        #return
        return(filelist)
      })

      #add names
      names(rel.files) <- dirlist.short

      #return
      return(rel.files)
    })

    #add names
    names(rel.dirs) <- projdirs.short

    #filter
    rel.dirs <- lapply(rel.dirs, FUN = function(x) Filter(function(x) length(x) > 0, x))
    rel.dirs <- Filter(function(x) length(x) > 0, rel.dirs)
  }

  #return
  return(rel.dirs)
}
