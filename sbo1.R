require(tidyverse)
require(stringi)
require(sbo)

data <- data %>% filter(source != "twitter") 

replace_pattern <- '[!\\p{Pd}:;\\?…]'
remove_pattern <- '[\\d+\\"""#$%&()\\*\\+,\\/<\\=>@\\[\\]\\^_`‘“”¤¦{\\|}~\\t\\n]'
profanityList <- as_tibble(read_lines('./data/final/en_US/profanity.txt')) %>%
    rename(word = value)

clean_text <- function(dataset) {
    dataset %>% 
        mutate(sentence = str_remove_all(tolower(sentence), str_c(profanityList$word, collapse="|"))) %>%
        mutate(sentence = str_replace_all(sentence, "’", "'")) %>%
        mutate(sentence = str_replace_all(sentence, replace_pattern, '.')) %>%
        mutate(sentence = str_replace_all(sentence, "''", "'")) %>%
        mutate(sentence = str_replace_all(sentence, "^[']", "")) %>%
        mutate(sentence = str_remove_all(sentence, remove_pattern)) %>%
        mutate(sentence = iconv(sentence, "latin1", "UTF-8",sub=''))
}

data <- clean_text(data)
data <- data %>% sample_n(100000)

en_US.dic <- sbo_dictionary(readLines("./data/dict/en_US.dic") %>% gsub("/.+", "", .))

gc()
sbo.model <- sbo_predtable(object = (unlist(data[, 'sentence'])), 
                           N = 4, # Train a 4-gram model
                           dict = en_US.dic, # use the en_us dictionary for vocabulary
                           .preprocess = sbo::preprocess, # Preprocessing transformation 
                           EOS = ".?!:;", # End-Of-Sentence tokens
                           lambda = 0.3, # Back-off penalization in SBO algorithm
                           L = 30L, # Number of predictions for input
                           filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> token from predictions
)

save(sbo.model, file = './data/model/sbo.model.rData')

sbo.pred <- sbo_predictor(sbo.model)
(evaluation <- eval_sbo_predictor(sbo.pred, test = unlist(data2[, 'sentence'] )))
evaluation %>% 
    filter(true != "<EOS>", input != " ") %>%
    summarise(
    accuracy = sum(correct)/n(), 
    uncertainty = sqrt(accuracy * (1 - accuracy) / n()
    )
)
pred <- as.data.frame(predict(sbo.pred, "information about the")) %>%
    rename(word = 1)

clean_input <- function(input) {
        input = str_replace_all(input, "’", "'")
        input = str_replace_all(input, "''", "'")
        input = str_replace_all(input, "^[']", "")
        input = str_remove_all(input, remove_pattern)
        input = iconv(input, "latin1", "UTF-8",sub='')
        return(input)
}

predict3 <- function(input){
    input <- clean_input(input)
    last_char <- str_sub(input, start= -1)
    if (str_detect(last_char, "[.?!:;]")) {
        input <- paste0(input, " ")
    }
    
    sentences <- str_split(input, regex("[.?!:;] "))
    active_sentence <- tail(sentences[[1]],1)
    active_word = ""
    
    last_char <- str_sub(active_sentence, start= -1)
    
    if (!active_sentence=="" & !last_char %in% c("", " ")){
        last_space <- stri_locate_last(active_sentence, regex = "[ ]")[1]
        if (!is.na(last_space)) {
            active_word <- str_sub(active_sentence, last_space+1)
            active_sentence <- str_sub(active_sentence, 1, last_space-1)
        } else {
            active_word <- active_sentence
            active_sentence <- ""
        }
    }
    
    pred <- as.data.frame(predict(sbo.pred, active_sentence)) %>%
        rename(word = 1)
    
    if (active_word == "") {
        output <- pred %>% slice_head(n=3)
    } else {
        output <- rbind(
            pred %>% filter(str_starts(word, active_word)),
            pred
        ) %>% slice_head(n=3)
    }
    
    if (active_sentence=="") {
        output <- output %>% mutate(word=str_to_title(word))
    }
    return(output)
}

test<-predict3('i want to go forward and see which c')
 
predict3("")



update_text <- function(input_text, word){
    last_char <- str_sub(input_text, start= -1)
    last_space <- stri_locate_last(input_text, fixed = " ")[1]
    if (last_char == "" | is.na(last_space)){
        return(paste(word, ""))
    }
    active_input <- str_sub(input_text, 1, last_space-1)
    return(paste(active_input, word, ""))
}

update_text("hghg ", "Click")










