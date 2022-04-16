require(tidyverse)
require(tidytext)
require(ggplot2)
require(wordcloud)
require(cowplot)
require(quanteda)
require(quanteda.textplots)
require(igraph)
require(ggraph)

news <- as_tibble(read_lines('./data/en_US/en_US.blogs.txt', skip_empty_rows = T)) %>%
    rename(text = value) %>%
    mutate(source = 'news')
blogs <- as_tibble(read_lines('./data/en_US/en_US.blogs.txt', skip_empty_rows = T)) %>%
    rename(text = value) %>%
    mutate(source = 'blog')
tweets <- as_tibble(read_lines('./data/en_US/en_US.blogs.txt', skip_empty_rows = T)) %>%
    rename(text = value) %>%
    mutate(source = 'twitter')

all_text <- rbind(news, blogs, tweets)
rm(news, blogs, tweets)

profanityList <- as_tibble(read_lines('./data/final/en_US/profanity.txt')) %>%
    rename(word = value)

data(stop_words)
tokens <- all_text %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_replace(word, "’", "'")) %>%
    filter(is.na(suppressWarnings(as.numeric(word)))) %>%
    anti_join(rbind(profanityList, select(stop_words, word)))

count_tokens_by_source <- tokens %>%
    group_by(source) %>%
    count(word, sort = TRUE)

count_tokens_by_source %>%
    slice_head(n=40) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) +
    geom_col() +
    labs(y = NULL)

par(mfrow=c(1,3))

wc_news <- count_tokens_by_source %>%
    filter(source=='news') %>%
    slice_head(n=400) %>%
    with(wordcloud(words = word, freq = n, max.words = 400, random.order =F, 
                   max_font_size=80, colors = brewer.pal(9, 'Set1'), 
                   margin=0))

wc_blog <- count_tokens_by_source %>%
    filter(source=='blog') %>%
    slice_head(n=400) %>%
    with(wordcloud(words = word, freq = n, max.words = 400, random.order =F, 
                   max_font_size=80, colors = brewer.pal(9, 'Set1'), 
                   margin=0))

wc_twitter <- count_tokens_by_source %>%
    filter(source=='twitter') %>%
    slice_head(n=400) %>%
    with(wordcloud(words = word, freq = n, max.words = 400, random.order =F, 
                   max_font_size=80, colors = brewer.pal(9, 'Set1'), 
                   margin=0))

text_sample <- sample_n(all_text, round(nrow(all_text) * 0.05))

quan_tokens <- tokens(text_sample$text, remove_punct=T)
toks_ngram <- tokens_ngrams(quan_tokens, n = 2)
head(toks_ngram[[1]], 30)
tail(toks_ngram[[1]], 30)

bigram_tokens <- text_sample %>%
    unnest_tokens(word, text, token = "ngrams", n=2)
bigram_tokens %>% 
    count(word, sort = T)

bigram_tokens_sep_filtered <- bigram_tokens %>%
    filter(!is.na(word)) %>%
    separate(word, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(is.na(suppressWarnings(as.numeric(word1)))) %>%
    filter(is.na(suppressWarnings(as.numeric(word2))))
    

bigram_counts <- bigram_tokens_sep_filtered %>% 
    count(word1, word2, sort = TRUE)

bigram_counts

bigram_graph <- bigram_counts %>%
    filter(n > 60) %>%
    graph_from_data_frame()

bigram_graph

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()


trigram_tokens <- text_sample %>%
    unnest_tokens(word, text, token = "ngrams", n=3)
trigram_tokens %>% 
    count(word, sort = T)

trigram_tokens_sep_filtered <- trigram_tokens %>%
    filter(!is.na(word)) %>%
    separate(word, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word) %>%
    filter(is.na(suppressWarnings(as.numeric(word1)))) %>%
    filter(is.na(suppressWarnings(as.numeric(word2)))) %>%
    filter(is.na(suppressWarnings(as.numeric(word3))))


trigram_counts <- trigram_tokens_sep_filtered %>% 
    count(word1, word2, word3, sort = TRUE)

trigram_counts

trigram_graph <- trigram_counts %>%
    filter(n > 10) %>%
    graph_from_data_frame()

trigram_graph

ggraph(trigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()


corp_text <- corpus(text_sample$text, docvars = data.frame(Source=text_sample$source))
toks_text <- tokens(corp_text, remove_punct = T, remove_symbols = T, 
                    remove_numbers = T, remove_url = T)
toks_text <- tokens_select(toks_text, pattern = profanityList$word, selection = "remove")
toks_text_no_stop <- tokens_select(toks_text, pattern = stopwords("en"), selection = "remove")

toks_ngram <- tokens_ngrams(toks_text, n = 2:3)
head(toks_ngram[[1]],)

dfmat <- dfm(toks_text)
topfeatures(dfmat,20)

fcmat <- fcm(dfmat)
topfeatures(fcmat,20)

feat <- names(topfeatures(fcmat, 50))
fcmat_select <- fcm_select(fcmat, pattern = feat, selection = "keep")
size <- log(colSums(dfm_select(dfmat, feat, selection = "keep")))
set.seed(144)
textplot_network(fcmat_select, min_freq = 0.8, vertex_size = size / max(size) * 3)

textplot_wordcloud(dfmat, max_words = 400, random_order = F, adjust = 1, 
                   color = brewer.pal(9, 'Set1'), random_color = T)

as.data.frame(word_freq) %>% 
    select(feature, frequency) %>%
    slice_max(order_by = frequency, n = 200) %>%
    ggplot(aes(label = feature, size = frequency, colour = frequency)) + 
    geom_text_wordcloud_area(eccentricity = 2, shape = 'square') +
    scale_size_area(max_size = 20) +
    theme_void() +
    scale_colour_viridis_c()

all_text %>% 
    filter(source=='twitter') %>%
    summarise(
        count = n(),
        words = mean(nchar(text))
        )

median(nchar(news$text))
median(nchar(blogs$text))
hist(nchar(tweets$text))

corp_text <- corpus(text_sample$text, docvars = data.frame(Source=text_sample$source))
summ <- summary(corp_text, nrow(text_sample)) %>% 
    rename(Words=Tokens)
summ %>%
    group_by(Source) %>%
    select(-Text, -Types) %>%
    skim()

g <- summ %>% ggplot(aes(x = Words)) +
    geom_histogram(bins = 100, fill="orange", colour="#ff8c00", alpha = 0.8) +
    theme_minimal() +
    ylab(NULL) +
    ggtitle("Distribution of Word Count") 

g + geom_smooth(
        aes(y = 4200*(..density..), colour = "red"), method = "loess", span=5,
        stat = 'density', 
        size = 1.5, 
        alpha = 0.8) + 
    theme(legend.position = "none") +
    scale_x_log10()

for (i in 1:10) {
    print(head(toks_ngram[[i]],8))
    }

dfmat_ns <- dfm(toks_text_no_stop)
word_freq <- textstat_frequency(dfmat_ns)
as.data.frame(word_freq) %>% 
    select(feature, frequency) %>%
    slice_max(order_by = frequency, n=40) %>%
    ggplot(aes(x=frequency, y=fct_reorder(feature, frequency))) +
    geom_col(fill="orange", colour="#ff8c00", alpha = 0.8) +
    theme_minimal() +
    ylab(NULL) +
    ggtitle("40 Most Common Words in the Sample Data Set (Stop Words Removed)") 

collocation_freq <- toks_text %>% textstat_collocations(min_count = 200)
collocation_freq %>% slice_max(order_by = count, n=40)

collocation_freq_ns <- toks_text_no_stop %>% textstat_collocations(min_count = 200)
collocation_freq_ns %>% slice_max(order_by = count, n=40)


toks_comp <- tokens_compound(toks_text_no_stop, pattern = collocation_freq_ns[collocation_freq_ns$z > 3])
for (i in 1:10) {
    print(head(toks_comp[[i]],8))
}

toks_ngram_ns <- tokens_ngrams(toks_text_no_stop, n = 2:3)
dfmat_ngram_ns <- dfm(toks_ngram_ns)

dfmat_ngram <- dfm(toks_ngram)
topfeatures(dfmat_ngram,20)

ngram_freq <- as.data.frame(textstat_frequency(dfmat_ngram)) %>% 
    filter(frequency > 40) %>% 
    select(feature, frequency) %>%
    separate(feature, c("word1", "word2", "word3"), sep = "_") 

ngram_freq_ns <- as.data.frame(textstat_frequency(dfmat_ngram_ns)) %>% 
    filter(frequency > 20) %>% 
    select(feature, frequency) %>%
    separate(feature, c("word1", "word2", "word3"), sep = "_") 

cbind(
    ngram_freq %>% filter(is.na(word3)) %>% slice_max(frequency, n=40, with_ties = F) %>% select(-word3),
    ngram_freq_ns %>% filter(is.na(word3)) %>% slice_max(frequency, n=40, with_ties = F) %>% select(-word3),
    ngram_freq %>% filter(!is.na(word3)) %>% slice_max(frequency, n=40, with_ties = F),
    ngram_freq_ns %>% filter(!is.na(word3)) %>% slice_max(frequency, n=40, with_ties = F)
) %>%
    kbl() %>%
    column_spec (c(1,4,7,11), extra_css = "padding-left: 20px;border-left-style: solid;border-color: lightgrey;") %>%
    column_spec (c(3,6,10,14), extra_css = "padding-right: 20px;") %>%
    column_spec(1:14, width_min = "6em") %>%
    kable_styling(full_width = T, font_size = 11, bootstrap_options = c("striped", "condensed")) %>%
    add_header_above(
            c(
                "Bigrams with Stopwords" = 3, 
                "Bigrams w/o Stopwords" = 3,
                "Trigrams with Stopwords" = 4, 
                "Trigrams w/o Stopwords" = 4
            )
        )

ngram_graph <- ngram_freq %>%
    filter(frequency > 2000) %>%
    graph_from_data_frame()
ngram_graph

set.seed(1234)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(ngram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = frequency), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()

data(stop_words)
tokens <- text_sample %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_replace(word, "’", "'")) %>%
    filter(is.na(suppressWarnings(as.numeric(word)))) %>%
    anti_join(profanityList)

tokens_ns <- tokens %>%
    anti_join(stop_words)

source_words <- tokens %>% count(source, word, sort = T)
source_words_ns <- tokens_ns %>% count(source, word, sort = T)

source_total <- source_words %>% group_by(source) %>% summarise(total = sum(n))
source_total_ns <- source_words_ns %>% group_by(source) %>% summarise(total = sum(n))

source_words <- left_join(source_words, source_total)
source_words_ns <- left_join(source_words_ns, source_total_ns)

ggplot(source_words, aes(n/total, fill = source)) +
    geom_histogram(show.legend = FALSE, bins=25) +
    facet_wrap(~source, ncol = 3, scales = "free_y") +
    scale_x_log10(limits=c(NA, 0.0009)) +
    ggtitle("Term Frequency Distribution by Source (Log Base 10 Scale)")

ggplot(source_words_ns, aes(n/total, fill = source)) +
    geom_histogram(show.legend = FALSE, bins=25) +
    facet_wrap(~source, ncol = 3, scales = "free_y") +
    scale_x_log10(limits=c(NA, 0.0009)) + 
    ggtitle("Term Frequency without Stop Words Distribution by Source (Log Base 10 Scale)") +
    theme_minimal()

freq_by_rank <- source_words %>% 
    group_by(source) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total) %>%
    ungroup()

freq_by_rank_ns <- source_words %>% 
    group_by(source) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total) %>%
    ungroup()

freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, colour=source)) + 
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10() +
    theme_minimal()

rank_subset <- freq_by_rank %>% 
    filter(rank < 10000,
           rank > 10)

zipflm <- coef(lm(log10(`term frequency`) ~ log10(rank), data = rank_subset))
zipflm

freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, colour=source)) + 
    geom_abline(intercept = zipflm[1], slope = zipflm[2], 
                color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10() +
    theme_minimal()

source_tf_idf <- source_words %>%
    bind_tf_idf(word, source, n)
source_tf_idf %>%
    select(-total) %>%
    slice_max(tf_idf, n=20) %>%
    kbl() %>%
    kable_styling(font_size = 10, bootstrap_options = "condensed", full_width = F)

source_tf_idf %>%
    group_by(source) %>%
    slice_max(tf_idf, n = 25, with_ties = F) %>%
    ungroup() %>%
    ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, ncol = 3, scales = "free") +
    labs(x = "tf-idf", y = NULL)




test<- source_tf_idf %>%
    group_by(source) %>%
    slice_max(tf_idf, n = 25, with_ties = F) %>%
    mutate(ranking = row_number()) %>%
    ungroup()


# read US English Dictinary
english_words <- readLines("./data/dict/en_US.dic") %>% gsub("/.+", "", .)
n_dict <- length(english_words)

dfmat_en <- toks_text %>%
    tokens_keep(english_words, valuetype = "fixed") %>% 
    dfm()

word_freq <- textstat_frequency(dfmat)
word_freq_en <- textstat_frequency(dfmat_en)

nrow(word_freq_en)/nrow(word_freq)

nrow(word_freq_en)/n_dict


token_coverage <- function(word_cover) {  
    count <- 0 
    coverage <- word_cover * sum(word_freq_common$docfreq) 
    for (n in 1:nrow(word_freq_common)) { 
        if (count >= coverage) {
            return (n)
        } else {
            count <- count + word_freq_common$docfreq[n]
        }
    }
}

# Estimate number of observations required to sample 50% of common words
token_coverage(.5)
# Estimate number of observations required to sample 90% of common words
token_coverage(.9)

token_coverage_v <- Vectorize(token_coverage)
tibble(Coverage = seq(0.05, 0.95, by = 0.05)) %>%
    mutate(Samples = token_coverage_v(Coverage)) %>%
    ggplot(aes(x=100*Coverage, y=Samples)) +
    geom_point(colour="blue", size=2) +
    geom_smooth(method = "loess", se = FALSE, span=0.4) +
    theme_minimal() +
    ylab("Required Sample Size") + xlab("Coverage (%)") +
    ggtitle("Samples Required to Meet Common Vocabulary Coverage") +
    geom_vline(xintercept = c(50,90), linetype="dashed", alpha=0.7)







