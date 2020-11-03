library(tidyverse)

# This is a set of functions for formatting notes into tiddlers for upload to Locus

parse_markdown_note <- function(note, tags = list(), fields = list()) {
  
}

parse_tiddler <- function(tiddler) {
  
  tidlines <- readLines(tiddler)
  writeLines(tidlines)
  
  cutoff <- min(which(tidlines == ""))
  
  fields <- tidlines[1:(cutoff - 1)] %>%
    strsplit(": ")
  txt <- tidlines[(cutoff + 1):length(tidlines)] %>%
    paste(collapse = "\n")
  
  nms <- fields %>% transpose() %>% `[[`(1) %>% unlist()
  kk <- fields %>% transpose() %>% `[[`(2)
  
  if("tags" %in% nms) {
    ind <- which("tags" == nms)
    kk[ind] <- strsplit(kk[[ind]], ", ")
  }
  
  kk
  names(kk) <- nms
  kk
  
  list(fields = kk, txt = txt)
}

# g <- parse_tiddler("Once Upon a Time.tid")
# g$fields
# g$txt

build_tiddler <- function(title, txt, fields = list(), tags = NULL) {
  
  fields$title = title
  if(!("type" %in% names(fields))) {
    fields$type = "text/x-markdown"
  }
  if(!("created" %in% names(fields))) {
    fields$created = paste0(gsub("-|:| ", "", Sys.time()),"000")
  }
  fields$tags = paste(tags, collapse = ", ")
  print(fields)
  
  header <- paste(names(fields),fields, sep = ": ")
  
  writeLines(c(header, "", txt), paste0(title,".tid"))
  
}

txt1 <- "# It was a dark and stormy night

- There was no wood.
- There was no fire.
  - Everyone was afraid.

## First part

Everyone was afraid.

> They said there was nothing to be afraid of.

[[Getting Things Done]]

## Second part

- Eggs
- Milk
  - Skim
- Bread"

# build_tiddler("Once Upon a Time", txt1, fields = list(one = 1, typee = "threes"), tags = c("one","two"))

