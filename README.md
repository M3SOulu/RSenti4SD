[![Build Status](https://travis-ci.org/M3SOulu/RSenti4sd.svg?branch=master)](https://travis-ci.org/M3SOulu/RSenti4sd)

# RSenti4SD

This packages contain functions for
running [Senti4SD](https://github.com/collab-uniba/Senti4SD) from R.

## Installation

With devtools:

    devtools::install_github("M3SOulu/RSenti4SD")

## Running Senti4SD

Senti4SD requires several binary files that aren't included in the
package: the jar file for extracting features, the LiblineaR SVM model
and the word embedding (DSM). These can be downloaded and to the
package installation directory using an R function included in the
package:

    library(RSenti4SD)
    DownloadSenti4SD()

Before running Senti4SD, the LiblineaR model must be loaded:

    model <- Senti4SDModel()

Then, Senti4SD can be simply run:

    Senti4SDSingle(c("I am happy", "I am sad"), model)

This will create a temporary file containing the input text vector,
run Senti4SD's jar to extract features which are saved in a CSV
file. The function loads this CSV file and runs the LiblineaR model on
it to make a prediction. The temporary files are discarded. Their
location can be modified by changing the value of the R option
senti4sd.tmpdir.

## Limiting memory usage

However, because the text features produced by the jar have a high
dimensionality and are stored in CSV files as plain matrices, Senti4SD
tends to consume a lot of memory (and disk space) as soon as large text
documents are passed as input.

The package provides a function for dealing with this problem by
chuncking the text input as several smaller vectors (maximum 1000
elements by default) and making successive calls to Senti4SDSingle. It
is highly recommend to use this function for any data that would
require more to run Senti4SD on more than a thousand text documents:

    Senti4SD(c("I am happy", "I am sad"), model)

## Running Senti4SD on a data.frame

The package also provides a function for running Senti4SD on a column
of a data.frame and adding the result as a new column:

    RunSenti4SD(data.table(id=c(1, 2), text=c("I am happy", "I am sad")), model)

This has the advantage of running Senti4SD on unique text elements of
the input column which can save computation time on a large dataset
with many identical text documents.

## Credits

This package only contains lines of code for instrument Senti4SD. Most
of the credits go to the original Senti4SD authors:

    Calefato, F., Lanubile, F., Maiorano, F., & Novielli,
    N. (2018). Sentiment polarity detection for software
    development. Empirical Software Engineering, 23(3), 1352-1382.
