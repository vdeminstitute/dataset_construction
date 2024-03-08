#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# This script operates on the stata files created by dataset.R. It calls convert.do
# on each dataset, which in turn calls value_labels.do. The former contains 
# static file paths to the Stata dataset. The latter script contains static
# labels for each variable and response category.

# Do note that the value_labels.do script is applied to all datasets even though 
# some of the variable/label pairs in the script are not present in all datasets.

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

info("The Stata scripts contains static file paths/labels, have they been adjusted?")
db <- pg_connect()
get_globals()

home <- Sys.getenv("HOME")
system("cd " %^% home %^% "/proj/vdemds/stata_dataset/ && " %^%
    "~/.local/bin/stata -b do convert.do")
d <- readLines(home %^% "/proj/vdemds/stata_dataset/convert.log")
d <- d[-(1:21)]
d <- trimws(d, which = "both")
d <- d[d != ""]

# -- print for logs
print(shQuote(d))