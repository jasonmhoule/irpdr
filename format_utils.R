library(tidyverse)

# This is a set of functions for formatting notes into tiddlers for upload to Locus

parent <- function(lvl) {
  
  lvlseq <- seq_along(lvl)
  out <- vector("integer", max(lvlseq))
  lvl <- c(0, lvl)
  
  for(i in lvlseq) {
    out[i] <- max(which(lvl[1:(i+1)] < lvl[i+1])) - 1
  }
  
  return(out)
}

add_ids_at_level <- function(df, level) {
  
  lvlarg <- level
  
  df_lkp <- df %>% 
    select(parent = grp, parent_id = final_id)
  
  df <- df %>% 
    left_join(df_lkp, by = "parent") %>% 
    mutate(final_id = if_else(level == lvlarg, paste0(parent_id,lvl_id), final_id)) %>% 
    select(-parent_id)
  
  return(df)
  
}

add_final_id <- function(df) {
  
  df$final_id <- NA_character_
  
  df <- df %>% 
    mutate(final_id = if_else(level == 1, lvl_id, final_id))
  
  # Recurse for 2 to max
  maxlvl <- max(df$level)
  
  if(maxlvl > 1) {
    for (i in 2:maxlvl) {
      
      df <- add_ids_at_level(df, i)
      
    }
  } 
  
  return(df)
}

parse_markdown_text <- function(type, title, txt, tags = NULL, fields = list(), outfolder) {
  
  ##[] Need to build the main/header tiddler!
  ##[] Header tiddler needs to end in {{||viewLiteratureHeader}} tag
  ##[] Also add a flow where there are no section headers - note is just a single text block, perhaps with tags
  ##[] Handle convention for adding tags and fields... should combine what's entered as args with any header text so that we can specify these in the markdown too
  
  type <- "LT"
  title <- "This is my Title"
  txt <- test_txt2
  tags <- NULL
  fields <- list()
  main_tid_title <- paste(paste0(type,":"), title)
  outfolder <- "test_out"
  
  # Initial extract and organization of all text
  
  xy <- tibble(txt = strsplit(txt, "\n")[[1]]) %>% 
    mutate(txt2 = trimws(str_remove_all(txt, "#")),
           leading = gsub("([\\#]*) .*","\\1",txt),
           header = grepl("#",leading),
           prehead = lead(header),
           posthead = lag(header),
           level = nchar(leading)*header) %>% 
    group_by(header) %>% 
    mutate(grp = if_else(level == 0,NA_integer_, row_number())) %>% 
    ungroup() %>% 
    fill(grp)
  
  xy_txt <- xy %>% 
    filter(!header, (is.na(prehead) | !prehead), !posthead) %>% 
    group_by(grp) %>% 
    mutate(tid_txt = paste(txt, collapse = "\n")) %>% 
    select(grp, tid_txt) %>% 
    distinct() %>% 
    mutate(tid_txt = paste(tid_txt,"\n\n{{||viewLiterature}}"))
  
  # Create tiddler headings and hierarchy
  
  heads <- xy %>% 
    filter(level > 0)
  
  heads$parent <- parent(heads$level)
  
  heads <- heads %>%  
    group_by(parent) %>% 
    mutate(lvl_id = str_pad(as.character(dplyr::row_number(parent)), 2, "left", "0")) %>% 
    ungroup() %>% 
    add_final_id() %>% 
    mutate(tid_title = paste(main_tid_title, final_id, txt2))
  
  tag_lkp <- heads %>% 
    select(parent = grp, parent_tag = tid_title) %>% 
    bind_rows(tibble(parent = 0, parent_tag = main_tid_title))
  
  heads_full <- heads %>% 
    left_join(tag_lkp, by = "parent") %>% 
    left_join(xy_txt, by = "grp")

  heads_full$fields <- list("caption" = heads$txt2, "level" = heads$level) %>% transpose()
  
  print(heads_full)
  
  for(i in 1:nrow(heads_full)) {
    
    r <- heads_full[i,]
    print(r$tid_title)
    
    build_tiddler(title = paste(r$final_id, r$txt2),
                  txt = r$tid_txt,
                  fields = r$fields[[1]],
                  tags = r$parent_tag,
                  outfolder = outfolder
                    )
    
  }
  
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

build_tiddler <- function(title, txt, fields = list(), tags = NULL, outfolder = "tiddler_out") {
  
  if(!dir.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  fields$title <- title
  if(!("type" %in% names(fields))) {
    fields$type = "text/x-markdown"
  }
  if(!("created" %in% names(fields))) {
    fields$created = paste0(gsub("-|:| ", "", Sys.time()),"000")
  }
  fields$tags = paste(tags, collapse = ", ")
  
  header <- paste(names(fields),fields, sep = ": ")
  
  wintitle <- gsub(":", "_", title)
  
  writeLines(c(header, "", txt), paste0(outfolder,"/",wintitle,".tid"))
  
}

test_txt <- "There was a tiddler I knew. [[Podcast|http://www.google.com]] without transcript.

Most importantly are a few points made in the introduction.

# It was a dark and stormy night

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
- Bread

### Third

- Check

# One

Hey there

## Two one

- Two
  - Two two

## Two one two

> 'Hey there in the 2-1-2' "

# build_tiddler("Once Upon a Time", txt1, fields = list(one = 1, typee = "threes"), tags = c("one","two"))

parse_markdown_text("TT","My new Tiddler",test_txt, outfolder = "test_out")
