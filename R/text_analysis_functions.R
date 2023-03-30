
###############################
#                              
#  read_pdf_text function            
#                              
###############################

# Author: Alexander Gamerdinger
# Version: 2023-03-14

# Libraries 
library(tabulizer)
library(dplyr)
library(pdftools)
library(tidytext)
library(stringr)
library(purrr)
library(tidyr)
library(textclean)
library(stringi)
library(igraph)
library(ggraph)
library(tesseract)

# this might cause some problems for some that have not installed the previous
library(spacyr)
library(pdftools)


#===========================================================================

# a very similar function to pdftools::pdf_ocr_data but it has an option in tesseract that reads pdfs with cols
pdf2text <- function(path, language = "eng", pages = NULL) {
  # set google's tesseract function by specified language
  engine <- tesseract(language = language, options = list(tessedit_pageseg_mode = 1))
  images <- pdftools::pdf_convert(path, format = "png", dpi = 600, pages = pages)
  # just return as text. 
  tesseract::ocr(images, engine = engine)
}


# a function that a) takes a pdf and reads the pages, and then b) transforms into uncleaned text. and outputs it as a data frame. 

read_pdf <- function(path, page_nr = FALSE, language = "eng") {
  # list files in path that have a .pdf extension and get unique doc names

  files <-
    list.files(path,
               full.names = TRUE,
               pattern = ".pdf$",
               recursive = TRUE)
  
  # creating doc names with basename function
  doc_names <- basename(files) %>% str_remove(".pdf$")
  
  
  # if page number is true, then give a data frame per doc and page
  if (page_nr) {
    
    out <- map2_df(.x = files, .y = doc_names, ~ {
      # if readable, then use the extract_text function from the tabulizer package
      
      # readable or not based ona a test if the text contains more than 15 characters
      # using the stringi package because of speed advantage tested by rbenchmark
      readab <- nchar(stri_c(pdftools::pdf_text(.x), collapse = "")) > 15
        
      if (readab) {
        # extract the text by page number
        text <-
          extract_text(.x, pages = seq_along(1:pdftools::pdf_info(.x)$pages))
      } else {
        # use pdf2text function here. Be careful, because here, confidence values are not taken into account.
        # these could be found by placing lapply(images, tesseract::ocr_data, engine = engine) into the function.
        text <-
          pdf2text(.x,
                   language = language,
                   pages = pdftools::pdf_info(.x)$pages)
      }
    
      # put each doc in a row and list the text()
      tibble(name = .y, 
             text = text %>% list(), 
             pages = pdftools::pdf_info(.x)$pages, 
             year = pdftools::pdf_info(.x)$created) %>%
        # unnest text so doc will be reoccuring
        unnest(cols = text) %>%
        # create page number
        mutate(page = row_number(), 
               #get rid of weid n's
               text = str_replace_all(text, "\\n", " "), 
               # remove line breaks
               text = str_remove_all(text, "- ")) 
      
    })
    
    # if not, then just give a data frame per doc
  } else {
    
    out <- map2_df(.x = files, .y = doc_names, ~ {
      # if readable, then use the extract_text function from the tabulizer package
      
      # readable or not based ona a test if the text contains more than 15 characters
      # using the stringi package because of speed advantage tested by rbenchmark
      readab <- nchar(stri_c(pdftools::pdf_text(.x), collapse = "")) > 15
      
      if (readab) {
        # extract the text by page number
        text <-
          extract_text(.x, pages = seq_along(1:pdftools::pdf_info(.x)$pages))
      } else {
        # use pdf2text function here. Be careful, because here, confidence values are not taken into account.
        # these could be found by placing lapply(images, tesseract::ocr_data, engine = engine) into the function.
        text <-
          pdf2text(.x,
                   language = language,
                   pages = pdftools::pdf_info(.x)$pages)
      }
      
      # put each doc in a row and list the text()
      tibble(name = .y, 
             text = paste(text, collapse = " "), 
             pages = pdftools::pdf_info(.x)$pages, 
             year = pdftools::pdf_info(.x)$created) %>%
        mutate(
               #get rid of weid n's
               text = str_replace_all(text, "\\n", " "), 
               # remove line breaks
               text = str_remove_all(text, "- ")) 
  
    })
    
  }
  
  # remove non-UTF8 characters
  out$text <- stringi::stri_trans_general(out$text, "latin-ascii")
  out$text <- textclean::replace_non_ascii(out$text)
  
  return(out)
  
  # improvement: 
  
  # - getting year from this text
  # - getting author from this text

} 

#===========================================================================

# a function that a) cleans text and c) tokenizes it. 

tokenize_text <- function(data,
                          txt_col = "text",
                          model = "en_core_web_sm",
                          token = c("word", "sentence")) {
  # init spacy
  spacy_initialize(model = model)
  
  # clean and tokenize with spacy
  data <- data %>%
    mutate(
      text = spacy_tokenize(
        data[[txt_col]],
        what = token,
        remove_punct = FALSE,
        remove_url = TRUE,
        remove_numbers = TRUE,
        remove_separators = TRUE,
        remove_symbols = TRUE
      )
    ) %>%
    unnest(cols = text)
  
  # create line number if sentence
  if (token == "sentence") {
    data <- data %>%
      mutate(line_nr = row_number(), 
             text = str_replace_all(text, "\\n", " "))
    
  }
  
  return(data)
  
}

#===========================================================================

clean_string <- function(input_string, remove_stopwords = TRUE, remove_numerics = TRUE, remove_punct = TRUE,
                         remove_symbols = TRUE, remove_single_characters = TRUE, remove_two_characters = TRUE,
                         remove_whitespace = TRUE, remove_chars = NULL, to_lower = TRUE,
                         replace_linebreaks = FALSE, replace_emptystrings = TRUE) {
  
  # Replace line breaks
  if (replace_linebreaks) {
    input_string <- stri_replace_all_regex(input_string, "[\r\n]", " ")
  }
  
  # Remove punctuation
  if (remove_punct) {
    input_string <- stri_replace_all_regex(input_string, "\\p{P}", "")
  }
  
  # Remove numerics
  if (remove_numerics) {
    input_string <- stri_replace_all_regex(input_string, "\\d+", "")
  }
  
  # Remove symbols
  if (remove_symbols) {
    input_string <- stri_replace_all_regex(input_string, "\\p{S}", "")
  }
  
  # Remove single characters
  if (remove_single_characters) {
    input_string <- stri_replace_all_regex(input_string, "\\b[a-zA-Z]\\b", "")
  }
  
  # Remove two-character words if specified
  if (remove_two_characters) {
    input_string <- stri_replace_all_regex(input_string, "\\b\\w{1,2}\\b", " ")
  }
  
  # Remove whitespace
  if (remove_whitespace) {
    input_string <- stri_replace_all_regex(input_string, "\\s+", " ")
    input_string <- stri_trim_both(input_string)
  }
  
  # Remove specific characters
  if (!is.null(remove_chars)) {
    remove_chars_regex <- paste0("[", paste0(remove_chars, collapse = ""), "]")
    input_string <- stri_replace_all_regex(input_string, remove_chars_regex, "")
  }
  
  # Convert text to lowercase
  if (to_lower) {
    input_string <- stri_trans_tolower(input_string)
  }
  
  # Remove stopwords
  if (remove_stopwords) {
    input_tibble <- data.frame(text = input_string, stringsAsFactors = FALSE)
    input_tibble$text <- stri_split_regex(input_tibble$text, pattern = "\\s+")
    input_tibble$text <- lapply(input_tibble$text, setdiff, stop_words$word)
    input_tibble$text <- sapply(input_tibble$text, paste, collapse = " ")
    input_string <- input_tibble$text
  }
  
  # Replace empty strings with NA values
  if (replace_emptystrings) {
    input_string[input_string == ""] <- NA
  }
  
  # Return the cleaned text as a character vector
  cleaned_text <- as.character(input_string)
  
  # Return the cleaned text
  return(cleaned_text)
}


#===========================================================================
#' spacy_parse_advance
#'
#' Parses text using the spacyr package and returns a list containing the raw output
#' and a cleaned version of the parsed text.
#'
#' @param df A data frame containing the text to be parsed.
#' @param txt_col The name of the column in df that contains the text to be parsed.
#' @param dictionary Whether to use a dictionary to filter the parsed text. If TRUE,
#' a dictionary list must be provided in the dict_list parameter.
#' @param dict_list A character vector containing the words to filter by if dictionary is TRUE.
#' @param pos A character vector indicating which parts of speech to include in the cleaned version
#' of the parsed text. Possible values are "nouns", "nounphrase", and "all".
#' @param model The spacyr model to use for parsing. Default is "en_core_web_sm".
#'
#' @return A list containing two data frames: text_ready (the cleaned version of the parsed text)
#' and raw_text (the raw output from spacy_parse).
#'
#' @examples
#' data("crude")
#' spacy_parse_advance(crude, txt_col = "text", pos = "nouns", dictionary = TRUE,
#'                     dict_list = c("oil", "barrel"), model = "en_core_web_lg")
#'
#' @import spacyr
#' @import dplyr
#' @import magrittr
#' @importFrom tidyr unnest
#' @importFrom textclean clean_string
#'
#' @export
#' 
spacy_parse_advance <-
  function(df,
           txt_col = "text",
           dictionary = FALSE,
           dict_list = NULL,
           pos = c("nouns", "nounphrase", "all"), 
           model = "en_core_web_sm") {
    
    # check if spacyr is loaded and install it if necessary
    if (!requireNamespace("spacyr", quietly = TRUE)) {
      stop("Please install the spacyr package before running this function. You can use install.packages('spacyr') to install it. Please use spacy_install() to run the model in the R environment")
    }
    
    # start spacyr session with specified model
    spacy_initialize(model = model)
  
    # validate input parameters
    stopifnot(is.data.frame(df))
    stopifnot(txt_col %in% colnames(df))
    stopifnot(pos %in% c("nouns", "nounphrase", "all"))
  
    if (dictionary) {
      
      stopifnot(!is.null(dict_list))
      
      # parsing everything through spacy
      text <- spacy_parse(df[[txt_col]], dependency = TRUE, nounphrase = TRUE)
      
      #using dictionary
      d1 <- text %>%
        #collapse dictionary and filter in d1
        filter(grepl(paste0(dict_list, collapse = "|"), lemma, ignore.case = TRUE)) %>%
        select(doc_id, sentence_id) %>%
        distinct(doc_id, sentence_id) 
      
      # only selecting lines for dictionary
      text <- d1 %>%
        left_join(raw_text,
                  by = c("doc_id", "sentence_id"),
                  multiple = "all")
      
    } else {
      
      # parsing everything through spacy
      text <- spacy_parse(df[[txt_col]], dependency = TRUE, nounphrase = TRUE)
    
    }
    
    switch(pos,
           # if nouns
           "nouns" = {
             text_ready <- text %>%
               #filtering only for noun and pronoun
               filter(pos %in% c("PROPN", "NOUN")) %>%
               # using default clean string version
               mutate(lemma = clean_string(lemma)) %>%
               filter(!is.na(lemma)) %>%
               group_by(doc_id) %>%
               count(lemma, sort = T) %>%
               ungroup()
           },
           # if nounphrase
           "nounphrase" = {
             text_ready <- text %>%
               # consolidating nounphrases
               nounphrase_consolidate(concatenator = " ") %>%
               #filtering out some of the dislinked ones
               filter(
                 !pos %in% c(
                   "PUNCT",
                   "CCONJ",
                   "AUX",
                   "ADP",
                   "SYM",
                   "SPACE",
                   "SCONJ",
                   "PART",
                   "INTJ",
                   "X",
                   "DET",
                   "NUM"
                 )
               ) %>%
               # using default clean string version
               mutate(lemma = clean_string(lemma)) %>%
               filter(!is.na(lemma)) %>%
               group_by(doc_id) %>%
               count(lemma, sort = T) %>%
               ungroup()
             
           },
           # if all
           "all" = {
             text_ready <-  text %>%
               # consolidating nounphrases
               nounphrase_consolidate(concatenator = " ") %>%
               # using default clean string version
               mutate(lemma = clean_string(lemma)) %>%
               filter(!is.na(lemma)) %>%
               group_by(doc_id) %>%
               count(lemma, sort = T) %>%
               ungroup()
             
           })
    
    return(list(text_ready = text_ready, text = as_tibble(text)))
    
  }


#===========================================================================

make_network <-
  function(text_ready_df,
           by_words = TRUE,
           filter_n = TRUE,
           alpha = 0.25) {
    stopifnot(c("doc_id", "lemma", "n") %in% colnames(text_ready_df))
    
    if (filter_n) {
      # get rid of all words that only appear once (also if it appears once per doc)
      text_ready_df <-
        text_ready_df %>%
        filter(n > 1)
    }
    
    for_adjacency <- text_ready_df %>%
      # calculate tfidf
      bind_tf_idf(lemma, doc_id, n) %>%
      # sort on lemma
      arrange(lemma) %>%
      # remove lemmas used by only one author
      group_by(lemma) %>%
      filter(n() > 1) %>%
      ungroup()
    
    incidence <-
      cast_sparse(for_adjacency,
                  row = doc_id,
                  col = lemma,
                  value = tf_idf)
    
    if (by_words) {
      # adj for words
      adjacency <-  Matrix::t(incidence) %*% incidence
    } else {
      # adj for text
      adjacency <- incidence %*% Matrix::t(incidence)
    }
    
    #creating graph
    graph <-
      graph_from_adjacency_matrix(adjacency, mode = "undirected", weighted = TRUE) %>%
      igraph::simplify(remove.multiple = FALSE, remove.loops = TRUE)
    
    #create network backbone
    e <-
      cbind(igraph::as_data_frame(graph)[, 1:2], weight = E(graph)$weight)
    
    # in
    w_in <- graph.strength(graph, mode = "in")
    w_in <-
      data.frame(to = names(w_in), w_in, stringsAsFactors = FALSE)
    k_in <- degree(graph, mode = "in")
    k_in <-
      data.frame(to = names(k_in), k_in, stringsAsFactors = FALSE)
    
    e_in <- e %>%
      left_join(w_in, by = "to") %>%
      left_join(k_in, by = "to") %>%
      mutate(alpha_in = (1 - (weight / w_in)) ^ (k_in - 1))
    
    # out
    w_out <- graph.strength(graph, mode = "out")
    w_out <-
      data.frame(from = names(w_out), w_out, stringsAsFactors = FALSE)
    k_out <- degree(graph, mode = "out")
    k_out <-
      data.frame(from = names(k_out), k_out, stringsAsFactors = FALSE)
    
    e_out <- e %>%
      left_join(w_out, by = "from") %>%
      left_join(k_out, by = "from") %>%
      mutate(alpha_out = (1 - (weight / w_out)) ^ (k_out - 1))
    
    e_full <- left_join(e_in, e_out, by = c("from", "to", "weight"))
    
    e_full <- e_full %>%
      mutate(alpha = ifelse(alpha_in < alpha_out, alpha_in, alpha_out)) %>%
      select(from, to, alpha)
    
    E(graph)$alpha <- e_full$alpha
    
    pruned <- delete.edges(graph, which(E(graph)$alpha >= alpha))
    pruned <- delete.vertices(pruned, which(degree(pruned) == 0))
    
    # make degree for labelling most popular nodes
    V(pruned)$degree <- degree(pruned)
    
    # remove isolates
    isolates <- V(pruned)[degree(pruned) == 0]
    pruned <- delete.vertices(pruned, isolates)
    
    # calculate modularity for coloring
    communities <- cluster_louvain(pruned)
    V(pruned)$modularity <- communities$membership
    
    return(pruned)
    
  }
    
