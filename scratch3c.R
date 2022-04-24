library(text2vec)
library(tidytext)
library(tidyverse)
library(keras)

attach('./data/processed/project.Rdata')

# clean the sample data set
data <- text_sample %>% 
    # split observations into sentences
    unnest_tokens(sentence, text, token = "sentences") %>%
    # strip[ trailing full stop and remove excess whitespace
    mutate(sentence = str_squish(str_remove_all(sentence, '\\.'))) %>%
    # keep only sentences with 4 or more words (since we are predicting on 3 words)
    filter(str_count(sentence, '\\w+') > 3) %>%
    rowid_to_column("sentence_id")

detach()

data_batch <- data[1:1000,]

# We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in reviews_new
it <- itoken(data_batch$sentence, 
             tokenizer = word_tokenizer,
             ids = data_batch$sentence_id,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab <- create_vocabulary(it) # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
#vocab <- prune_vocabulary(vocab, term_count_min = 5)

# Vectorize word to integers
vectorizer <- vocab_vectorizer(vocab)

# Create a Term-Count-Matrix, by default it will use a skipgram window of 5 (symmetrical)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

# maximum number of co-occurrences to use in the weighting function, we choose the entire token set divided by 100
x_max <- length(vocab$doc_count)/100

# set up the embedding matrix and fit model
glove_model <- GloVe$new(rank = 32, x_max = x_max) 
glove_embedding = glove_model$fit_transform(tcm, n_iter = 20, convergence_tol = 0.01, n_threads = 4)

# combine main embedding and context embeddings (sum) into one matrix
glove_embedding = glove_embedding + t(glove_model$components) # the transpose of the context matrix

data_batch_tokens <- data_batch %>% 
    select(sentence_id, sentence) %>%
    unnest_tokens(word, sentence)

x_train_text <- data_batch %>% select(sentence) %>% pull()

y_train <- data_batch %>% 
    select(sentence_id) %>% 
    pull() %>% as.array()

# maximum number of words for a review
max_length <- 500

# Vectorize the tokens, each token will receive a unique integer, the index of that token in a dictionary. Remember, we already restricted the corpus to 37.520 unique words.
tokenizer_train <- text_tokenizer() %>% fit_text_tokenizer(x_train_text)

# Transform text to sequences using the train tokenizer 
sequences_train <- texts_to_sequences(tokenizer_train, x_train_text)

# and make sure that every sequence has the same length (Keras requirement)
input_train_text <- pad_sequences(sequences_train, maxlen = max_length)

# define the dimensions for the glove embedding model, we trained it with 32 dimensions
glovedim <- 32

# how many words are in the index
num_tokens <- length(unique(tokenizer_train$word_index))

# Keras needs an additional row as index, text2vec did not provide this
additional_row <- matrix(0L, nrow = 1, ncol = 32, dimnames = list('NA'))  

# Add the additional row and remove words from the glove embedding that are not in the Keras word index
embedding_matrix_glove <- rbind(additional_row, subset(glove_embedding, rownames(glove_embedding) %in% names(tokenizer_train$word_index)))

# Build our model
model_glove <- keras_model_sequential() %>% 
    # Specify the maximum input length (150) and input_dim (unique tokens+1) and choose 32 dimensions
    layer_embedding(input_dim = num_tokens+1, 
                    output_dim = glovedim, 
                    input_length = max_length,
                    mask_zero = TRUE,   
                    #weights = list(embedding_matrix_glove), # add weights from our previously trained embedding model
                    trainable = TRUE # allow further training
    ) %>% 
    # We flatten the 3D tensor of embeddings into a 2D tensor of shape `(samples, max_length * word2vecdim)`
    layer_flatten() %>% 
    # add a dense layer with 32 units
    layer_dense(units = 40, activation = "relu", kernel_initializer = "he_normal", bias_initializer = "zeros", kernel_regularizer = regularizer_l2(0.05)) %>% layer_dropout(rate = 0.2) %>%
    # add a dense layer with 20 units
    layer_dense(units = 20, activation = "relu", kernel_regularizer = regularizer_l2(0.01)) %>%
    # add the classifier on top
    layer_dense(units = 1, activation = "sigmoid") 

summary(model_glove)

model_glove %>% compile(
    optimizer = "rmsprop",
    # we have a binary classification, a single unit sigmoid in the dense layer so binary_crossentropy 
    loss = "categorical_crossentropy",
    # plot accuracy against train and testset
    metrics = c("accuracy")
)



history <- model_glove %>% fit(
    input_train_text, y_train,
    # maximum number of iterations
    epochs = 40,
    # how many reviews do we offer in each batch, small batches did not work well for the GloVe embedding model
    batch_size = 1000,
    # we have little Michelin restaurants, so we need to focus more on classifying these (set weights) ()
    #class_weight = list("0"=1,"1"=weight),
    # check train results againts test data
   # validation_data = list(input_test_text, y_test)
)


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

generate_sequence(model_glove, tokenizer_train, 3, seed_text, 1)


max_length <- 150
x_train_text <- data %>% select(sentence) %>% pull()
tokenizer_train <- text_tokenizer() %>% 
    fit_text_tokenizer(x_train_text)
sequences_train <- texts_to_sequences(tokenizer_train, x_train_text)
#input_train_text <- pad_sequences(sequences_train, maxlen = max_length)
one_hot_results <- texts_to_matrix(tokenizer = tokenizer_train, texts = x_train_text, mode = "binary")

install.packages("superml")
library(superml)

lbl <- LabelEncoder$new()
test <- x_train_text[1:10000]
tokenizer_test <- text_tokenizer() %>% 
    fit_text_tokenizer(test)
sequences_test <- texts_to_sequences(tokenizer_test, test)
one_hot_results <- texts_to_matrix(tokenizer_test, test, mode = "binary")
dim(one_hot_results)

pad_zeros <- function(x, width){c(x, rep(0, width - length(x)))}
x <- t(sapply(as.array(sequences_test), pad_zeros, max(lengths(sequences_test))))


maxlen <- 5
steps <- 2
sentence <- list()
next_word <- list()
list_words <- data.frame(word = data$sentence, stringsAsFactors = F)
j <- 1

for (i in seq(1, length(data$sentence) - maxlen - 1, by = steps)){
    sentence[[j]] <- as.character(data$sentence[i:(i+maxlen-1)])
    next_word[[j]] <- as.character(data$sentence[i+maxlen])
    j <- j + 1
}


x <- array(0, dim = c(batch_size, maxlen, length(vocab)))
y <- array(0, dim = c(batch_size, length(vocab)))
y <- to_categorical(y, num_classes = length(vocab$term))

keras::encoder

pad_zeros <- function(x, width){c(x, rep(0, width - length(x)))}
sequences <- t(sapply(as.array(sequences_train), pad_zeros, max(lengths(sequences_train))))
x = sequences[:,:-1]
y = sequences[:,-1]
y <- to_categorical(y, num_classes=length(vocab$term))
seq_length = dim(x)[1]


model <- keras_model_sequential() %>%
    layer_embedding(length(vocab$term), 50, input_length = batch_size) %>%
    layer_lstm(100, return_sequences = T) %>%
    layer_lstm(100) %>%
    layer_dense(100, activation = "relu") %>%
    layer_dense(length(vocab$term), activation='softmax') 

summary(model)

#optimizer <- optimizer_rmsprop(learning_rate = 0.01)

model %>% compile(
    loss = "categorical_crossentropy", 
    optimizer = 'adam',
    metrics = c('accuracy')
)

model %>% keras::fit(x,one_hot_results, batch_size=128, epochs=50)

###

sample_mod <- function(preds, temperature = 1){
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp(preds))
    
    rmultinom(1, 1, preds) %>% 
        as.integer() %>%
        which.max()
}

batch_size <- 128
all_samples <- 1:length(sentence)
num_steps <- trunc(length(sentence)/batch_size)

sampling_generator <- function(){
    
    function(){
        
        batch <- sample(all_samples, batch_size)
        all_samples <- all_samples[-batch]
        
        sentences <- sentence[batch]
        next_words <- next_word[batch]
        
        # vectorization
        X <- array(0, dim = c(batch_size, maxlen, length(vocab)))
        y <- array(0, dim = c(batch_size, length(vocab)))
        
        
        for(i in 1:batch_size){
            
            X[i,,] <- sapply(vocab, function(x){
                as.integer(x == sentences[i])
            })
            
            y[i,] <- as.integer(vocab == next_words[i])
            
        }
        
        # return data
        list(X, y)
    }
}


model %>% keras::fit(generator = sampling_generator(),
                        steps_per_epoch = num_steps,
                        epochs = 3)











