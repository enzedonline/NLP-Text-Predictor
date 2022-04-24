# read English common usage Dictinary
common_words <- readLines("./data/dict/en_common.dic") %>% gsub("/.+", "", .)

# filter DFM by common English words
toks_common <- toks_text %>%
    tokens_keep(common_words, valuetype = "fixed")

common_ngrams <- tokens_ngrams(toks_common, n = 1:4)

dfmat_common_ngram <- dfm(common_ngrams)
topfeatures(dfmat_common_ngram,50,decreasing = F)

tbl_common_ngrams <- as.data.frame(textstat_frequency(dfmat_common_ngram)) %>% 
    select(feature, frequency) %>%
    separate(feature, c("word1", "word2", "word3", "word4"), sep = "_") 

common_ngrams2 <- tokens_ngrams(toks_common, n = 4)

dfmat_common_ngram2 <- dfm(common_ngrams2)

tbl_common_ngrams2 <- as.data.frame(textstat_frequency(dfmat_common_ngram2)) %>% 
    select(feature, frequency) %>%
    separate(feature, c("word1", "word2", "word3", "word4"), sep = "_") 
