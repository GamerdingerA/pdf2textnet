# pdf2textnet

pdf2textnet is collection of R functions that allows you to create networks based on pdf text. It is very much inspired by the [textnets](https://github.com/cbail/textnets) package, but uses the [spacyR](https://github.com/quanteda/spacyr) wrapper for NLP instead of the [updipe](https://github.com/quanteda/spacyr) package. This collection, which is still in development, currently provides functions for reading in PDF files, tokenizing text, cleaning text data, preprocessing text with the spacyr library, and creating networks based on preprocessed text data.

## Getting started
To download this developmental collection of R functions, use the ´source´ function. 

```
source("https://raw.githubusercontent.com/GamerdingerA/pdf2textnet/main/R/text_analysis_functions.R")

```

## Functions

### read_pdf
read_pdf is a function that reads in PDF files and returns their text content as a data frame. It takes a file path as input and can optionally specify the page number to read in the PDF. If page_nr is set to TRUE, the function will return a data frame per page of the PDF file. The output of the function includes the file name, the page number, and the text content of the page.

### tokenize_text
tokenize_text is a function that tokenizes text by word or by sentence using the spacyr package. It takes a data frame and the name of the column containing the text as input, and returns a data frame with each token as a separate row. If the token parameter is set to "sentence", it also creates a line number for each sentence.

### clean_string
clean_string is a function that cleans text data by removing unwanted characters and formatting. It can remove stopwords, numerics, punctuation, symbols, single characters, and two-character words. It can also remove specified characters, convert text to lowercase, remove whitespace, replace line breaks, and replace empty strings. The function takes an input string and several parameters that determine which cleaning operations are performed. It returns the cleaned string as output.

### spacy_parse_advance
This function is used for text preprocessing using the spacy library. It takes a dataframe df as input, which should have a column with the name specified in txt_col containing the text to be processed. The dictionary parameter specifies whether to use a dictionary for filtering or not. If set to TRUE, the dict_list parameter must also be provided as a list of words to filter the text. The pos parameter is used to select whether to only extract nouns, noun phrases, or all parts of speech. The function returns a dataframe of parsed and filtered text.

### make_network
This function is used to create a network from a dataframe of preprocessed text using the spacy_parse_advance function. The text_ready_df parameter is the preprocessed dataframe, and by_words specifies whether to create a network based on individual words or based on whole text segments. The filter_n parameter can be used to remove words that appear only once. The alpha parameter specifies a damping factor for the network calculation. The function returns a dataframe with the edges and weights of the network. 

