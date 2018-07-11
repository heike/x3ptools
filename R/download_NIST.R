#' Download data from an entire study
#' 
#' Use this function to download all of the data from a single study. Locate
#' the study using the search page (\url{https://tsapps.nist.gov/NRBTD/Studies/Studies}). 
#' @param study_link Link to study page, or id value 
#'          (https://tsapps.nist.gov/NRBTD/Studies/Studies/Details/<study-id-value>)
#' @param directory Directory to download the data into. 
#' @param mirrorFileStructure Should separate folders be created for each 
#'          firearm and bullet? This may result in highly nested data structure. 
#'          Setting this to FALSE may result in metadata being overwritten.
#' @export
#' @importFrom xml2 read_html
#' @importFrom xml2 xml_find_first
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr 
#' @importFrom utils download.file
#' @section Note:
#' To instead download enough data to use for package exploration, use the 
#' \code{\link{NRBTDsample_download}} function. 
#' @seealso NRBTDsample_download
#' @examples 
#' \dontrun{
#' url <- "https://tsapps.nist.gov/NRBTD/Studies/Studies/Details/"
#' studyID <- "c09aaa86-5d60-4acb-9031-46dad2c0ad32"
#' fullurl <- paste0(url, studyID)
#' NRBTD_download(fullurl, 
#'                    file.path("data"), mirrorFileStructure = T)
#' NRBTD_download(studyID, file.path("data"), mirrorFileStructure = T)                    
#'}
NRBTD_download <- function(study_link, directory, mirrorFileStructure = T) {
  stopifnot(dir.exists(directory))
  
  if (!grepl(study_link, "http")) {
    studyUrl <- paste0("https://tsapps.nist.gov/NRBTD/Studies/Studies/Details/", study_link)
  } else {
    studyUrl <- study_link
  }
  
  studyPage <- try(xml2::read_html(studyUrl))
  if ("try-error" %in% class(studyPage)) {
    stop("Could not find study page as specified. Try copying the URL directly")
  }
  
  barrelLinks <- unlist(lapply(xml2::xml_find_all(studyPage, "//a[text()='Bullet / CC']"), function(x) xml2::xml_attr(x, attr = "href")))

  landLinks <- lapply(paste0("https://tsapps.nist.gov", barrelLinks), function(x) {
      pg <- xml2::read_html(x)
      barrelName <- xml2::xml_find_first(pg, '//*[@id="bodycontainer"]/div[4]/div/div/div[2]/div[2]/div[1]/dl/dd[1]/text()') 
      barrelName <- xml2::xml_text(barrelName) 
      barrelName <- gsub("^\\s{1,}|\\s{1,}$",  "", barrelName)
      # Create folder for barrel
      if (mirrorFileStructure) {
        curpath <- file.path(directory, barrelName)
        if (!dir.exists(curpath)) dir.create(curpath)
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
      if (!dir.exists(curpath)) dir.create(curpath)
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

#' Download data from two NIST bullets to use as sample data
#' 
#' Bullets downloaded are from the Hamby series of studies, using 9mm 
#' ammunition. This function downloads two bullets from the same barrel 
#' (Barrel 1); each bullet has 6 x3p files associated with it - one for each 
#' land. The x3p files are placed in folders corresponding to each bullet.
#' Data from additional studies can be found at 
#' \url{https://tsapps.nist.gov/NRBTD/Studies/Search} and can be downloaded with
#' the \code{\link{NRBTD_download}} function. 
#' @param directory Location to save the files
#' @importFrom utils download.file
#' @export
#' @seealso NRBTD_download
NRBTDsample_download <- function(directory) {
  stopifnot(dir.exists(directory))
  
  # Bullet 1, Barrel 1
  dl_df <- data.frame(
    name = c(paste0("Hamby252_Barrel1_Bullet1_Land", 1:6, ".x3p"), 
             paste0("Hamby252_Barrel1_Bullet2_Land", 1:6, ".x3p")),
    folder = rep(c("Bullet1", "Bullet2"), each = 6),
    url = c("https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/43567404-1611-4b40-ae74-a1e440e79f6a", 
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/a9f59fe1-f64b-487b-9f73-322ea0133a74",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/2ea4efe4-beeb-4291-993d-ae7726c624f4",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/6bb13db8-01ca-4cd4-ba5d-1c5670f1c204",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/2110e6c2-f801-458f-941a-9740804aa162",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/eaa73b31-8f9c-4b7f-a1c8-48e4da3ff9e0",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/979bf3f5-2bf4-43ab-aa14-66e79e0cbc99",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/b2b25004-364c-4468-b835-fd563b190a27",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/554c40d8-8857-4b1c-a28f-fda9b347999b",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/da019fc2-3a19-4da5-b1d7-ec059cd095f2",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/d6dfaef6-f066-4b76-bf42-f0e8c06d6241",
            "https://tsapps.nist.gov/NRBTD/Studies/BulletMeasurement/DownloadMeasurement/a172932e-121c-4bee-9477-ae2454f0b513"),
    stringsAsFactors = F
  )
  
  folders <- file.path(directory, unique(dl_df$folder))
  folders_exist <- dir.exists(folders)
  if (sum(!folders_exist) > 0) {
    lapply(folders[!folders_exist], dir.create)
  }
  # Ensure sub-folders exist
  stopifnot(dir.exists(folders))
  
  sapply(1:nrow(dl_df), 
         function(x) {
           download.file(url = dl_df$url[x], 
                         destfile = file.path(directory, dl_df$folder[x], 
                                              dl_df$name[x], ".x3p"))
         })
}