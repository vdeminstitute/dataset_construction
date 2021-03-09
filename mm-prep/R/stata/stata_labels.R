#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")

home <- Sys.getenv("HOME")
system("cd " %^% home %^% "/proj/mm-prep/do/ && " %^%
    "/usr/local/stata/stata-se -b do convert.do")
d <- readLines(home %^% "/proj/mm-prep/do/convert.log")
d <- d[-(1:21)]
d <- d %>% trimws(which = "both") %>% .[. != ""]
print(shQuote(d))

update_task_status(db = db)
