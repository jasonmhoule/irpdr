library(pdftools)
# library(curl)
library(tidyverse)
# library(tidytext)

info <- pdf_info("5758-annotations.pdf")
pda <- pdf_data("5758-annotations.pdf")
ptx <- pdf_text("5758-annotations.pdf")
# ptoc <- pdf_toc("5758-annotations.pdf")

xyz <- bind_rows(pda)[1:130,]

## ADD page numbers
## ADD differentiate underlined vs highlighted text

split_pages <- function(xyz) {
  xyz %>% 
    mutate(ct = case_when(
      grepl("Page|\\#",xyz$text) ~ 10,
      space == FALSE ~ 1,
      TRUE ~ 0
    )) %>% 
    mutate(ct2 = ct + lag(ct, default = 0)) %>% 
    mutate(p = ifelse(text == "Page", lead(text), NA)) %>% 
    fill(p) %>% 
    mutate(typ = ifelse(grepl("\\#",text), lead(text), NA)) %>% 
    fill(typ) %>% 
    mutate(ck = cumsum(ct2 == 11)) %>% 
    filter(!(ct2 == 11)) %>% 
    group_split(ck)
}

gg <- pda %>% bind_rows() %>% split_pages()

make_notes <- function(tbl) {
  rawtxt <- paste(tbl$text, collapse = " ")
  prefix <- case_when(
    tbl$typ[1] == "Highlight" ~ "\n> ",
    tbl$typ[1] == "Underline" ~ "\n## ",
    TRUE ~ "")
  suffix <- paste0(" (p",tbl$p[1],")")
  paste0(prefix, rawtxt, suffix)
}

noted <- map_chr(gg, make_notes)

write_lines(noted, "out.txt")
