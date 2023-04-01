# Download the CBA from Github website

# ---- start --------------------------------------------------------------

# Parse the CBA to a raw text file:
library(pdftools)
library(tidyverse)

# Create a directory for the data
local_dir    <- "raw"
data_source <- paste0(local_dir, "/articles")
man_dir     <- paste0(local_dir, "/manual")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source)
if (!file.exists(man_dir)) dir.create(man_dir)


# ---- download -----------------------------------------------------------

cba_url <- paste0("https://github.com/atlhawksfanatic/",
                  "atlhawksfanatic.github.io/raw/master/research/CBA/",
                  "2005-NBA-NBPA-Collective-Bargaining-Agreement.pdf")

cba_file <- paste(local_dir,
                  "2005-NBA-NBPA-Collective-Bargaining-Agreement.pdf",
                  sep = "/")
if (!file.exists(cba_file)) download.file(cba_url, cba_file)

# ---- parse --------------------------------------------------------------

cba <- pdf_text(cba_file)

# Remove the table of contents and the index, this is specifically for 2005
cba_cut <- cba[14:499]

# Key difference so far:
#  1. Page numbers at the bottom of the page
#  2. No index

# Find where an "Article" or "Exhibit" Exists.
cba_articles <- map2(cba_cut, seq(cba_cut), function(x, y) {
  print(y)
  exist_regex <- "(ARTICLE|EXHIBIT) [A-z]+\n([A-Z]|\\s|[:punct:])*\n"
  # article_exists <- str_locate_all(x, "(ARTICLE|EXHIBIT) [A-z]+\n.*\n")
  article_exists <- str_locate_all(x, exist_regex)
  
  if (is_empty(article_exists[[1]])) {
    article = NA_character_
    article_name   = NA_character_
    article_roman = NA_character_
  } else {
    article = str_sub(x, article_exists[[1]])
    
    article_type   = word(article, 1)
    article_roman  = str_trim(word(article, 2))
    # str_remove_all(article, ".*?([a-z]|[:punct:])") %>% #
    article_name   = str_remove_all(article, "\\b([A-Z]+[a-z]+|[a-z]+)\\b") %>% 
      word(3, -1) %>% 
      str_trim() %>% 
      str_remove_all("\\n") %>% 
      str_squish()
  }
  
  # Punctuation that is in a different font
  txt <- str_replace_all(x, "“|”", '"')
  txt <- str_replace_all(txt, "’", "'")
  txt <- gsub("$", "\\$", txt, fixed = TRUE)
  txt <- gsub("%", "\\%", txt, fixed = TRUE)
  
  # Get rid of page numbers on the bottom
  txt_wo_pg <- str_remove(txt, "[0-9]+\n$")
  
  if (is_empty(article_exists[[1]])) {
    cba_structure <- tibble(page = y,
                            article = article,
                            text = txt_wo_pg,
                            article_name, article_roman)
    
  } else {
    cba_structure <- tibble(page = y,
                            article = paste(article_type, article_roman),
                            text = txt_wo_pg,
                            article_name, article_roman)
  }
  
  return(cba_structure)
})

# Converting the Exhibit values to numbers
exhibit_vals <- c("A" = 44,
                  "B" = 45,
                  "C" = 46,
                  "D" = 47,
                  "E" = 48,
                  "F" = 49,
                  "G" = 50,
                  "H" = 51)

cba_texts <- cba_articles %>% 
  bind_rows() %>% 
  fill(article:article_roman) %>% 
  mutate(article_number = ifelse(grepl("article", article, ignore.case = T),
                                 as.numeric(as.roman(article_roman)),
                                 exhibit_vals[article_roman])) %>% 
  arrange(article_number)


# As simple text files and Rmd
cba_texts %>% 
  group_by(article) %>% 
  summarise(text = paste0(str_trim(text), collapse = ""),
            number = article_number[1],
            article_name = article_name[1] %>% 
              str_remove_all(",") %>% 
              str_replace_all(" ", "-")) %>% 
  as.list() %>% 
  pmap(function(article, article_name, text, number) {
    temp_num  <- str_pad(number, 2, pad = "0")
    
    temp_file <- paste0(data_source, "/", article, ".txt")
    temp_rmd  <- paste0(man_dir, "/", temp_num, "-", article_name, ".Rmd")
    print(temp_rmd)
    cat(text, file = temp_file)
    cat(text, file = temp_rmd)
  })
