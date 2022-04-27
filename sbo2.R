en_us.dic <- readLines("./data/dict/en_US.dic") %>% gsub("/.+", "", .)
dict <- sbo_dictionary(en_us.dic)

trainData <- data %>% sample_n(80000)
testData <- data %>% sample_n(5000)

sbo.model.t1 <- sbo_predtable(object = (unlist(trainData[, 'sentence'])), # preloaded example dataset
                           N = 4, # Train a 3-gram model
                           dict = target ~ 0.9, # cover 95% of training corpus
                           .preprocess = sbo::preprocess, # Preprocessing transformation 
                           EOS = ".?!:;", # End-Of-Sentence tokens
                           lambda = 0.4, # Back-off penalization in SBO algorithm
                           L = 20L, # Number of predictions for input
                           filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> token from predictions
)

sbo.model.t2 <- sbo_predtable(object = (unlist(trainData[, 'sentence'])), # preloaded example dataset
                              N = 4, # Train a 3-gram model
                              dict = dict, # cover 95% of training corpus
                              .preprocess = sbo::preprocess, # Preprocessing transformation 
                              EOS = ".?!:;", # End-Of-Sentence tokens
                              lambda = 0.3, # Back-off penalization in SBO algorithm
                              L = 30L, # Number of predictions for input
                              filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> token from predictions
)

sbo.pred.t1 <- sbo_predictor(sbo.model.t1)
sbo.pred.t2 <- sbo_predictor(sbo.model.t2)

(evaluation.t1 <- eval_sbo_predictor(sbo.pred.t1, test = unlist(testData[, 'sentence'] )))
(evaluation.t2 <- eval_sbo_predictor(sbo.pred.t2, test = unlist(testData[, 'sentence'] )))

evaluation.t1 %>% 
    filter(true != "<EOS>", input != " ") %>%
    summarise(
        accuracy = sum(correct)/n(), 
        uncertainty = sqrt(accuracy * (1 - accuracy) / n()
        )
    )
evaluation.t2 %>% 
    filter(true != "<EOS>", input != " ") %>%
    summarise(
        accuracy = sum(correct)/n(), 
        uncertainty = sqrt(accuracy * (1 - accuracy) / n()
        )
    )
#=========================================================

sbo.model.t2.l2 <- sbo_predtable(object = (unlist(trainData[, 'sentence'])), # preloaded example dataset
                              N = 4, # Train a 3-gram model
                              dict = dict, # cover 95% of training corpus
                              .preprocess = sbo::preprocess, # Preprocessing transformation 
                              EOS = ".?!:;", # End-Of-Sentence tokens
                              lambda = 0.2, # Back-off penalization in SBO algorithm
                              L = 30L, # Number of predictions for input
                              filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> token from predictions
)

sbo.model.t2.l3 <- sbo_predtable(object = (unlist(trainData[, 'sentence'])), # preloaded example dataset
                              N = 4, # Train a 3-gram model
                              dict = dict, # cover 95% of training corpus
                              .preprocess = sbo::preprocess, # Preprocessing transformation 
                              EOS = ".?!:;", # End-Of-Sentence tokens
                              lambda = 0.3, # Back-off penalization in SBO algorithm
                              L = 30L, # Number of predictions for input
                              filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> token from predictions
)

sbo.model.t2.l4 <- sbo_predtable(object = (unlist(trainData[, 'sentence'])), # preloaded example dataset
                                 N = 4, # Train a 3-gram model
                                 dict = dict, # cover 95% of training corpus
                                 .preprocess = sbo::preprocess, # Preprocessing transformation 
                                 EOS = ".?!:;", # End-Of-Sentence tokens
                                 lambda = 0.4, # Back-off penalization in SBO algorithm
                                 L = 30L, # Number of predictions for input
                                 filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> token from predictions
)

sbo.model.t2.l5 <- sbo_predtable(object = (unlist(trainData[, 'sentence'])), # preloaded example dataset
                                 N = 4, # Train a 3-gram model
                                 dict = dict, # cover 95% of training corpus
                                 .preprocess = sbo::preprocess, # Preprocessing transformation 
                                 EOS = ".?!:;", # End-Of-Sentence tokens
                                 lambda = 0.5, # Back-off penalization in SBO algorithm
                                 L = 30L, # Number of predictions for input
                                 filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> token from predictions
)


sbo.pred.t2.l2 <- sbo_predictor(sbo.model.t2.l2)
sbo.pred.t2.l3 <- sbo_predictor(sbo.model.t2.l3)
sbo.pred.t2.l4 <- sbo_predictor(sbo.model.t2.l4)
sbo.pred.t2.l5 <- sbo_predictor(sbo.model.t2.l5)

evaluation.t2.l2 <- eval_sbo_predictor(sbo.pred.t2.l2, test = unlist(testData[, 'sentence'] ))
evaluation.t2.l3 <- eval_sbo_predictor(sbo.pred.t2.l3, test = unlist(testData[, 'sentence'] ))
evaluation.t2.l4 <- eval_sbo_predictor(sbo.pred.t2.l4, test = unlist(testData[, 'sentence'] ))
evaluation.t2.l5 <- eval_sbo_predictor(sbo.pred.t2.l5, test = unlist(testData[, 'sentence'] ))

evaluation.t2.l2 %>% 
    filter(true != "<EOS>", input != " ") %>%
    summarise(
        accuracy = sum(correct)/n(), 
        uncertainty = sqrt(accuracy * (1 - accuracy) / n()
        )
    )
evaluation.t2.l3 %>% 
    filter(true != "<EOS>", input != " ") %>%
    summarise(
        accuracy = sum(correct)/n(), 
        uncertainty = sqrt(accuracy * (1 - accuracy) / n()
        )
    )
evaluation.t2.l4 %>% 
    filter(true != "<EOS>", input != " ") %>%
    summarise(
        accuracy = sum(correct)/n(), 
        uncertainty = sqrt(accuracy * (1 - accuracy) / n()
        )
    )
evaluation.t2.l5 %>% 
    filter(true != "<EOS>", input != " ") %>%
    summarise(
        accuracy = sum(correct)/n(), 
        uncertainty = sqrt(accuracy * (1 - accuracy) / n()
        )
    )


evaluation.t2 %>%
    filter(correct, true != "<EOS>") %>%
    select(true) %>%
    transmute(rank = match(true, table = attr(sbo.pred.t2, "dict"))) %>%
    ggplot(aes(x = rank)) + 
    geom_histogram(fill="orange", colour="#ff8c00", alpha = 0.8) +
    scale_x_log10() +
    theme_minimal() +
    ggtitle("Correct Prediction by Word Rank")

(coverage <- word_coverage(sbo.pred.t2, unlist(trainData[, 'sentence'] )))
summary(coverage)
plot(coverage)