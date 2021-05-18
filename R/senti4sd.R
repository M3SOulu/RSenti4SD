#' Read Features
#'
#' Read feature file.
#'
#' @param filename CSV file containing features.
#' @return A data.table object with id column parsed as an integer.
ReadSenti4SDFeatures <- function(filename) {
  features <- fread(filename)
  features[, id := as.integer(sub("^t", "", id)) + 1]
  features
}

#' Features
#'
#' Call Senti4SD-fast.jar to compute features.
#'
#' @param input Input text file with a single column and no header.
#' @param output Output file containing computed features.
#' @param read.file If TRUE, read with data.table the content of
#'   output file.
#' @param use.temp.file If TRUE, output will be removed after feature
#'   computation. This also enforces read.file as TRUE.
#' @return The result of feature extraction as data.table if read.file
#'   is TRUE, stdout and stderr of the Java process as invisible
#'   object otherwise.
Senti4SDFeatures <- function(input, output, read.file=TRUE,
                             use.temp.file=FALSE) {
  senti.jar <- system.file("senti4sd", "Senti4SD-fast.jar",
                           package="RSenti4SD", mustWork=TRUE)
  dsm.bin <- system.file("senti4sd", "dsm.bin",
                         package="RSenti4SD", mustWork=TRUE)
  args <- c("-jar", senti.jar, "-F", "A", "-i", input, "-W", dsm.bin,
            "-oc", output, "-vd", "600")
  res <- invisible(system2("java", args, stdout=TRUE, stderr=TRUE))
  if (read.file || use.temp.file) {
    res <- ReadSenti4SDFeatures(output)
    if (use.temp.file) {
      file.remove(output)
    }
  }
  res
}

#' Load Model
#'
#' Load LiblineaR model.
#'
#' @return the LiblineaR model.
#' @export
Senti4SDModel <- function() {
  load(system.file("senti4sd", "modelLiblinear.Rda",
                   package="RSenti4SD", mustWork=TRUE))
  m
}

#' Predict
#'
#' Predict polarity from a set of features.
#'
#' @param model The LiblineaR model object.
#' @param features The feature data.table object.
#' @return A factor with levels positive, negative and neutral.
Senti4SDPredict <- function(model, features) {
  features <- features[, names(features) != "id", with=FALSE]
  predict(model, features)$predictions
}

#' Senti4SD Single
#'
#' Runs Senti4SD on given pieces of text.
#'
#' @param text A character vector on which to run Senti4SD.
#' @param model The LiblineaR model to use for prediction.
#' @param predict If TRUE makes prediction, otherwise return the
#'   features.
#' @param sparse.features If predict is TRUE, returns a sparseMatrix
#'   instead of a regular matrix.
#' @param tmpdir Directory where to store temporary files.
#' @return If predict is TRUE, A data.table object with text, id and
#'   (predicted) polarity columns, otherwise a matrix containing the
#'   features.
Senti4SDSingle <- function(text, model, predict=TRUE, sparse.features=TRUE,
                           tmpdir=getOption("senti4sd.tmpdir", tempdir())) {
  text.file <- tempfile(tmpdir=tmpdir)
  fwrite(data.table(gsub("\\s+", " ", text)), text.file,
         row.names=FALSE, col.names=FALSE)
  features <- Senti4SDFeatures(text.file, tempfile(tmpdir=tmpdir), TRUE, TRUE)
  file.remove(text.file)
  if (predict) {
    features <- na.omit(features)
    prediction <- Senti4SDPredict(model, features)
    result <- cbind(features[, list(id)], polarity=prediction)
    result <- result[order(id)]
    result[, text := text[id]]
    result
  } else {
    result <- as.matrix(features)[order(features[, "id"]), ]
    if (sparse.features) Matrix::Matrix(result, sparse=TRUE) else result
  }
}

#' Senti4SD
#'
#' Runs Senti4SD on given pieces of text by splitting the input in
#' multiple chunks and running Senti4SD on each chunk.
#'
#' @param text A character vector on which to run Senti4SD.
#' @param model The LiblineaR model to use for prediction.
#' @param chunk.size Maximum number of text element to consider for
#'   one single run of Senti4SD.
#' @param memory.limit Maximum amount of memory (in GB) to use for one
#'   run of Senti4SD. Overrides \code{chunk.size} by setting it to
#'   \code{500 * memory.limit}.
#' @param predict If TRUE makes prediction, otherwise return the
#'   features.
#' @param sparse.features If predict is TRUE, returns a sparseMatrix
#'   instead of a regular matrix.
#' @return If predict is TRUE, a data.table object with text, id and
#'   (predicted) polarity columns, otherwise a matrix containing
#'   the features.
#' @export
Senti4SD <- function(text, model, chunk.size=1000, memory.limit=0,
                     predict=TRUE, sparse.features=TRUE) {
  if (memory.limit > 0) {
    chunk.size <- 500 * memory.limit
  }
  chunks <- split(text, (1:length(text) - 1) %/% chunk.size)
  res <- lapply(1:length(chunks), function(i) {
    chunk <- chunks[[i]]
    logging::loginfo("Running Senti4SD on chunk %d/%d of size %d",
                     i, length(chunks), length(chunk))
    t <- system.time(res <- Senti4SDSingle(chunk, model, predict,
                                           sparse.features))
    logging::loginfo("Senti4SD run on chunk %d/%d in %.2f seconds",
                     i, length(chunks), t["elapsed"])
    res
  })
  if (predict) rbindlist(res) else do.call(rbind, res)
}

#' Run Senti4SD
#'
#' Runs Senti4SD on a text column of a data.frame. Splits the input in
#' multiple chunks and running Senti4SD on each chunk.
#'
#' @param df A data.frame or data.table containing the
#' @param model The LiblineaR model to use for prediction.
#' @param chunk.size Maximum number of text element to consider for
#'   one single run of Senti4SD.
#' @param memory.limit Maximum amount of memory (in GB) to use for one
#'   run of Senti4SD. Overrides \code{chunk.size} by setting it to
#'   \code{500 * memory.limit}.
#' @param input.column The name of the column in \code{df}
#'   containing the text to analyze with Senti4SD.
#' @param prefix Optional prefix to add to the table's column names.
#' @return A data.table object with columns from \code{df} and
#'   added polarity column.
#' @export
RunSenti4SD <- function(df, model, chunk.size=1000, memory.limit=0,
                        input.column="text", prefix=NULL) {
  if (!inherits(df, "data.table")) df <- as.data.table(df)
  text <- unique(df[[input.column]])
  res <- Senti4SD(text, model, chunk.size, memory.limit)
  res <- res[, list(text, senti4sd=polarity)]
  if (!is.null(prefix)) {
    setnames(res, "senti4sd", paste(prefix, "senti4sd", sep="."))
  }
  setnames(res, "text", input.column)
  merge(df, res, by=input.column, all=TRUE)
}
