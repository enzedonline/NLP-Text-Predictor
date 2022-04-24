library(text2vec)
library(keras)
library(tidyverse)

# load('./data/processed/project.Rdata')
# data <- iconv(paste(unlist(all_text %>% sample_n(5000) %>% select(text)), collapse =" "), 
#               "UTF-8","latin1",,sub='')
# save (
#     data,
#     file = './data/processed/test.Rdata'
# )

load('./data/processed/test.Rdata')

# data <- "The capstone project class will allow students to create a usable public data product that can be used to show your skills to potential employers. Projects will be drawn from real-world problems and will be conducted with industry, government, and academic partners."
# data <- '“I would like, for once, to see politicians keep their word, 99” '
# data <- 'I would like, for once, to see politicians keep their word, '

tokenizer <- text_tokenizer(num_words = 3000, filters = NULL)
tokenizer %>% fit_text_tokenizer(data)
unlist(tail(tokenizer$word_index, 100))
num_tokens <- length(unique(tokenizer$word_index))

text_seqs <- texts_to_sequences(tokenizer, data)[[1]]

input_sequence_length <- 3

additional_row <- matrix(0L, nrow = 1, ncol = 32, dimnames = list('NA'))  
embedding_matrix_glove <- rbind(additional_row, subset(glove_embedding, rownames(glove_embedding) %in% names(tokenizer_train$word_index)))





# feature <- matrix(ncol = input_sequence_length)
# label <- matrix(ncol = 1)

# for (i in seq(input_sequence_length, length(text_seqs))){
#     if (i >= length(text_seqs)) {
#         break()
#     }
#     start_idx <- (i - input_sequence_length) + 1
#     end_idx <- i + 1
#     new_seq <- text_seqs[start_idx:end_idx]
#     feature <- rbind(feature, new_seq[1:input_sequence_length])
#     label <- rbind(label, new_seq[input_sequence_length+1])
# }
# winflashr::winflash()
# feature <- feature[-1,]
# label <- label[-1,]
# 
# label <- to_categorical(label, num_classes = tokenizer$num_words)

model <- keras_model_sequential() %>% 
    layer_embedding(input_dim = tokenizer$num_words, 
                    output_dim = 10) %>%
    layer_lstm(units=50) %>%
    # bidirectional(layer_lstm(
    #     units = 50, dropout = 0.4, recurrent_dropout = 0.4,
    #     return_sequences = TRUE
    # )) %>%
    # bidirectional(layer_lstm(
    #     units = 50, dropout = 0.4, recurrent_dropout = 0.4,
    #     return_sequences = TRUE
    # )) %>%
    # bidirectional(layer_lstm(
    #     units = 50, dropout = 0.4, recurrent_dropout = 0.4
    # )) %>%
    layer_dense(tokenizer$num_words, activation = "softmax")

summary(model)

model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(learning_rate=0.001),
    metrics = c('accuracy')
)

lstm_history <- model %>% fit(
    feature,
    label,
    #batch_size = 512,
    epochs = 500,
    verbose = T
)

plot(lstm_history)

generate_sequence <- function(model, tokenizer, input_length, seed_text, predict_next_n_words=1) {
    input_text <- seed_text
    for (i in seq(predict_next_n_words)) {
        encoded <- texts_to_sequences(tokenizer, input_text)[[1]]
        encoded <- pad_sequences(sequences = list(encoded), maxlen = input_length, padding = 'pre')
        yhat <- model %>% predict(encoded) %>% k_argmax()
        next_word <- tokenizer$index_word[[as.character(yhat)]]
        input_text <- paste(input_text, next_word)
    }
    return(input_text)
}

seed_text <- "will be conducted"

generate_sequence(model, tokenizer, input_sequence_length, seed_text, 1)




# encoded <- texts_to_sequences(tokenizer, seed_text)[[1]]
# encoded <- pad_sequences(sequences = list(encoded), maxlen = 3, padding = 'pre')
# yhat <- model %>% predict(encoded) 
# sequences_to_texts(yhat)
# keras::sequences_to_matrix(tokenizer = tokenizer, sequences = yhat)
# 
# 
# library(data.table)
# select_top_n<-function(scores,n_top){
#     d <- data.frame(
#         x   = copy(scores),
#         indice=seq(1,length(scores)))
#     
#     setDT(d)
#     setorder(d,-x)
#     n_top_indice<-d$indice[1:n_top]
#     return(n_top_indice)
# }
# select_top_n(t(yhat), 3)


