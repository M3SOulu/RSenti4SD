github.repo <- "https://github.com/collab-uniba/Senti4SD"
files <- c("modelLiblinear.Rda", "Senti4SD-fast.jar", "dsm.bin")

#' Download Senti4sd
#'
#' Downloads required Senti4SD files (LiblineaR model, jar file and
#' DSM) from GitHub.
#'
#' @param version Git version tag representing the version of
#'   Senti4SD.
#' @param overwrite If FALSE, files that are already found in
#'   RSenti4SD installation directory won't be downloaded.
#' @export
DownloadSenti4SD <- function(version="master", overwrite=FALSE) {
  root.url <- file.path(github.repo, "raw", version, "ClassificationTask")
  dest.dir <- file.path(system.file(package="RSenti4SD"), "senti4sd")
  if (!file.exists(dest.dir)) dir.create(dest.dir)
  for (f in files) {
    if (overwrite || !file.exists(file.path(dest.dir, f))) {
      logging::loginfo("Downloading %s", f)
      download.file(file.path(root.url, f), file.path(dest.dir, f))
    }
  }
}
