#' Clone of magrittr's pipe
#' 
#' @param lhs left hand side
#' @param rhs right hand side
"%>%" <- function (lhs, rhs) 
{
  parent <- parent.frame()
  env <- new.env(parent = parent)
  chain_parts <- split_chain(match.call(), env = env)
  pipes <- chain_parts[["pipes"]]
  rhss <- chain_parts[["rhss"]]
  lhs <- chain_parts[["lhs"]]
  env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
                                                                              pipes[[i]], parent))
  env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value, 
                                                                 `_function_list`)), env, env), c("fseq", "function"))
  env[["freduce"]] <- freduce
  if (is_placeholder(lhs)) {
    env[["_fseq"]]
  }
  else {
    env[["_lhs"]] <- eval(lhs, parent, parent)
    result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, 
                               env))
    if (is_compound_pipe(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent, 
           parent)
    }
    else {
      if (result[["visible"]]) 
        result[["value"]]
      else invisible(result[["value"]])
    }
  }
}

#' Download a bullet land from NIST's NBTRD
#' 
#' @param study_link
#' @param filename
#' @export
#' @importFrom xml2 read_html
#' @return This function does not return anything but leaves downloaded files in the file structure. 
download_NIST <- function(study_link, directory, mirrorFileStructure = T) {
  stopifnot(dir.exists(directory))
  
  if (!grepl(study_link, "http")) {
    studyUrl <- paste0("https://tsapps.nist.gov/NRBTD/Studies/Studies/Details/", study_link)
  } else {
    studyUrl <- study_link
  }
  
  studyPage <- xml2::read_html(studyUrl)
  
  barrelLinks <- unlist(lapply(xml2::xml_find_all(studyPage, "//a[text()='Bullet / CC']"), function(x) xml2::xml_attr(x, attr = "href")))
  barrelNames <- lapply(xml2::xml_find_first())
  landLinks <- lapply(paste0("https://tsapps.nist.gov", barrelLinks), function(x) {
      pg <- xml2::read_html(x)
      barrelName <- xml2::xml_find_first(pg, '//*[@id="bodycontainer"]/div[4]/div/div/div[2]/div[2]/div[1]/dl/dd[1]/text()') 
      barrelName <- xml2::xml_text(barrelName) 
      barrelName <- gsub("^\\s{1,}|\\s{1,}$",  "", barrelName)
      # Create folder for barrel
      if (mirrorFileStructure) {
        curpath <- file.path(directory, barrelName)
        dir.create(curpath)
      } else {
        curpath <- directory
      }
      # Get all links for measurements for each bullet
      data.frame(barrelName = barrelName, barrelPath = curpath, 
                 measLink = unlist(lapply(xml2::xml_find_all(pg, "//a[text()='Measurements']"), 
                                          function(x) xml2::xml_attr(x, attr = "href"))),
                 stringsAsFactors = F)
    })
  
  landLinks <- do.call("rbind", landLinks)
  
  measLinks <- lapply(1:nrow(landLinks), function(x) {
    pg <- xml2::read_html(paste0("https://tsapps.nist.gov", landLinks[x,]$measLink))
    
    bulletName <- xml2::xml_find_first(pg, '//*[@id="bodycontainer"]/div[4]/div/div/div[2]/div[2]/div[1]/dl/dd[1]/text()') 
    bulletName <- xml2::xml_text(bulletName) 
    bulletName <- gsub("^\\s{1,}|\\s{1,}$",  "", bulletName)
    
    if (mirrorFileStructure) {
      curpath <- file.path(landLinks[x,]$barrelPath, bulletName)
      dir.create(curpath)
    } else {
      curpath <- landLinks[x,]$barrelPath
    }
    
    measLinks <- xml2::xml_find_first(pg, '//*/dd/a')
    measLinks <- xml2::xml_attr(measLinks, "href")
    download.file(paste0("https://tsapps.nist.gov", measLinks), destfile = file.path(curpath, "meas.zip"))
    
    unzip(file.path(curpath, "meas.zip"), exdir = curpath, overwrite = T)
    
    file.remove(file.path(curpath, "meas.zip"))
  })
  
}