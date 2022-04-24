require(sbo)
require(tidyverse)
require(tidytext)

attach('./data/processed/project.Rdata')

# clean the sample data set
data <- text_sample %>% 
    # split observations into sentences
    unnest_tokens(sentence, text, token = "sentences") %>%
    # strip[ trailing full stop and remove excess whitespace
    mutate(sentence = str_squish(str_remove_all(sentence, '\\.'))) %>%
    # keep only sentences with 4 or more words (since we are predicting on 3 words)
    filter(str_count(sentence, '\\w+') > 3) 

detach()

sbo.model <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                           N = 4, # Train a 3-gram model
                           dict = target ~ 0.9, # cover 90% of training corpus
                           .preprocess = sbo::preprocess, # Preprocessing transformation 
                           EOS = ".?!:;", # End-Of-Sentence tokens
                           lambda = 0.4, # Back-off penalization in SBO algorithm
                           L = 3L, # Number of predictions for input
                           filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

save(sbo.model, file = './sbo.model.rData')
rm(data)
gc()
#########


require(sbo)
require(tidyverse)
load('./sbo.model.rData')

sbo.pred <- sbo_predictor(sbo.model)

predict(sbo.pred, "information about the")

(evaluation <- eval_sbo_predictor(sbo.pred, test = unlist(data[, 'sentence'] )))

evaluation %>% summarise(
    accuracy = sum(correct)/n(), 
    uncertainty = sqrt(accuracy * (1 - accuracy) / n()
    )
)

evaluation %>%
    filter(correct, true != "<EOS>") %>%
    select(true) %>%
    transmute(rank = match(true, table = attr(sbo.pred, "dict"))) %>%
    ggplot(aes(x = rank)) + 
    geom_histogram(bins=50, fill="orange", colour="#ff8c00", alpha = 0.8) +
    theme_minimal() 

evaluation %>%
    filter(correct, true != "<EOS>") %>%
    select(true) %>%
    transmute(rank = match(true, table = attr(sbo.pred, "dict"))) %>%
    ggplot(aes(x = rank)) + 
    geom_histogram(fill="orange", colour="#ff8c00", alpha = 0.8) +
    scale_x_log10() +
    theme_minimal()

# Word coverage

(coverage <- word_coverage(sbo.pred, unlist(data[, 'sentence'] )))
summary(coverage)
plot(coverage)

# Object size in memory
data.frame('object' = ls()) %>%
    dplyr::mutate(size_unit = object %>%sapply(. %>% get() %>% object.size %>% format(., unit = 'auto')),
                  size = as.numeric(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[1])),
                  unit = factor(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[2]), levels = c('Gb', 'Mb', 'Kb', 'bytes'))) %>% 
    dplyr::arrange(unit, dplyr::desc(size)) %>% 
    dplyr::select(-size_unit)

# system time
system.time(predict(sbo.pred, "information about the"), gcFirst = TRUE)

library(rbenchmark)
library(kableExtra)

benchmark(
    "1. Unigram" = {
        X <- predict(sbo.pred, "information")
    },
    "2. Bigram" = {
        X <- predict(sbo.pred, "information about")
    },
    "3. Trigram" = {
        X <- predict(sbo.pred, "information about the")
    },
    replications = 1000,
    columns = c("test", "replications", "elapsed",
                "relative", "user.self", "sys.self")
) %>% 
    kbl() %>%
    kable_styling(full_width = F, bootstrap_options = "condensed")


#########################
# Lambda testing

sbo.model.L5 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                           N = 4, # Train a 3-gram model
                           dict = target ~ 0.9, # cover 90% of training corpus
                           .preprocess = sbo::preprocess, # Preprocessing transformation 
                           EOS = ".?!:;", # End-Of-Sentence tokens
                           lambda = 0.5, # Back-off penalization in SBO algorithm
                           L = 3L, # Number of predictions for input
                           filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

sbo.model.L3 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                              N = 4, # Train a 3-gram model
                              dict = target ~ 0.9, # cover 90% of training corpus
                              .preprocess = sbo::preprocess, # Preprocessing transformation 
                              EOS = ".?!:;", # End-Of-Sentence tokens
                              lambda = 0.3, # Back-off penalization in SBO algorithm
                              L = 3L, # Number of predictions for input
                              filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

sbo.model.L2 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                              N = 4, # Train a 3-gram model
                              dict = target ~ 0.9, # cover 90% of training corpus
                              .preprocess = sbo::preprocess, # Preprocessing transformation 
                              EOS = ".?!:;", # End-Of-Sentence tokens
                              lambda = 0.2, # Back-off penalization in SBO algorithm
                              L = 3L, # Number of predictions for input
                              filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

sbo.model.L6 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                              N = 4, # Train a 3-gram model
                              dict = target ~ 0.9, # cover 90% of training corpus
                              .preprocess = sbo::preprocess, # Preprocessing transformation 
                              EOS = ".?!:;", # End-Of-Sentence tokens
                              lambda = 0.6, # Back-off penalization in SBO algorithm
                              L = 3L, # Number of predictions for input
                              filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

create.sbo.predtable <- function(coverage, lambda){ 
    sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                  N = 4, # Train a 4-gram model
                  dict = target ~ as.numeric(coverage), # target coverage of training corpus
                  .preprocess = sbo::preprocess, # Preprocessing transformation 
                  EOS = ".?!:;", # End-Of-Sentence tokens
                  lambda = as.numeric(lambda), # Back-off penalization in SBO algorithm
                  L = 3L, # Number of predictions for input
                  filtered = "<UNK>" # Exclude the <UNK> token from predictions
    )
}

sbo.pred.L2 <- sbo_predictor(sbo.model.L2)
sbo.pred.L6 <- sbo_predictor(sbo.model.L6)
sbo.pred.L5 <- sbo_predictor(sbo.model.L5)
sbo.pred.L3 <- sbo_predictor(sbo.model.L3)

(evaluation <- eval_sbo_predictor(sbo.pred, test = unlist(data[, 'sentence'] )))
(evaluation.L5 <- eval_sbo_predictor(sbo.pred.L5, test = unlist(data[, 'sentence'] )))
(evaluation.L3 <- eval_sbo_predictor(sbo.pred.L3, test = unlist(data[, 'sentence'] )))
(evaluation.L2 <- eval_sbo_predictor(sbo.pred.L2, test = unlist(data[, 'sentence'] )))
(evaluation.L6 <- eval_sbo_predictor(sbo.pred.L6, test = unlist(data[, 'sentence'] )))

cbind(
    data.frame(Lambda = c(0.2, 0.3, 0.4, 0.5, 0.6)),
    rbind(
        evaluation.L2 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        ),
        evaluation.L3 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        ),
        evaluation %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        ),
        evaluation.L5 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        ),
        evaluation.L6 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        )
    )    
)

# Use lambda=3, examine effect of coverage

sbo.model.c50 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                               N = 4, # Train a 3-gram model
                               dict = target ~ 0.5, # cover 90% of training corpus
                               .preprocess = sbo::preprocess, # Preprocessing transformation 
                               EOS = ".?!:;", # End-Of-Sentence tokens
                               lambda = 0.3, # Back-off penalization in SBO algorithm
                               L = 3L, # Number of predictions for input
                               filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

sbo.model.c75 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                               N = 4, # Train a 3-gram model
                               dict = target ~ 0.75, # cover 90% of training corpus
                               .preprocess = sbo::preprocess, # Preprocessing transformation 
                               EOS = ".?!:;", # End-Of-Sentence tokens
                               lambda = 0.3, # Back-off penalization in SBO algorithm
                               L = 3L, # Number of predictions for input
                               filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

sbo.model.c90 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                               N = 4, # Train a 3-gram model
                               dict = target ~ 0.9, # cover 90% of training corpus
                               .preprocess = sbo::preprocess, # Preprocessing transformation 
                               EOS = ".?!:;", # End-Of-Sentence tokens
                               lambda = 0.3, # Back-off penalization in SBO algorithm
                               L = 3L, # Number of predictions for input
                               filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

sbo.model.c95 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                               N = 4, # Train a 3-gram model
                               dict = target ~ 0.95, # cover 90% of training corpus
                               .preprocess = sbo::preprocess, # Preprocessing transformation 
                               EOS = ".?!:;", # End-Of-Sentence tokens
                               lambda = 0.3, # Back-off penalization in SBO algorithm
                               L = 3L, # Number of predictions for input
                               filtered = "<UNK>" # Exclude the <UNK> token from predictions
)

sbo.model.c100 <- sbo_predtable(object = (unlist(data[, 'sentence'])), # preloaded example dataset
                               N = 4, # Train a 3-gram model
                               dict = target ~ 1, # cover 90% of training corpus
                               .preprocess = sbo::preprocess, # Preprocessing transformation 
                               EOS = ".?!:;", # End-Of-Sentence tokens
                               lambda = 0.3, # Back-off penalization in SBO algorithm
                               L = 3L, # Number of predictions for input
                               filtered = "<UNK>" # Exclude the <UNK> token from predictions
)
sbo.pred.c100 <- sbo_predictor(sbo.model.c100)
evaluation.c100 <- eval_sbo_predictor(sbo.pred.c100, test = unlist(data[, 'sentence'] ))



sbo.pred.c50 <- sbo_predictor(sbo.model.c50)
sbo.pred.c75 <- sbo_predictor(sbo.model.c75)
sbo.pred.c90 <- sbo_predictor(sbo.model.c90)
sbo.pred.c95 <- sbo_predictor(sbo.model.c95)

evaluation.c50 <- eval_sbo_predictor(sbo.pred.c50, test = unlist(data[, 'sentence'] ))
evaluation.c75 <- eval_sbo_predictor(sbo.pred.c75, test = unlist(data[, 'sentence'] ))
evaluation.c90 <- eval_sbo_predictor(sbo.pred.c90, test = unlist(data[, 'sentence'] ))
evaluation.c95 <- eval_sbo_predictor(sbo.pred.c95, test = unlist(data[, 'sentence'] ))

perf_table <- cbind(
    data.frame(Coverage = c(50, 75, 90, 95, 100)),
    rbind(
        evaluation.c50 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        ),
        evaluation.c75 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        ),
        evaluation.c90 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        ),
        evaluation.c95 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        ),
        evaluation.c100 %>% summarise(
            accuracy = sum(correct)/n(), 
            uncertainty = sqrt(accuracy * (1 - accuracy) / n()
            )
        )
    ),
    data.frame('object' = ls()) %>%
        filter(grepl("sbo.model.c", object)) %>% 
        mutate(size_unit = object %>% sapply(. %>% get() %>% object.size %>% format(., unit = 'auto')),
               size = as.numeric(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[1])),
               unit = factor(sapply(
                   strsplit(size_unit, split = ' '), 
                   FUN = function(x) x[2]), 
                   levels = c('Gb', 'Mb', 'Kb', 'bytes'))
        ) %>% 
        arrange(unit, size) %>% 
        select(-size_unit, -object)
) %>%
    mutate(weight = 100-Coverage) 

perf_table %>% 
    ggplot(aes(x=Coverage, y=accuracy)) + 
    geom_smooth(method="lm", se = F, span = 0.99) 

perf_table <- perf_table %>%
    add_row(Coverage = 30,accuracy=0,uncertainty=0,size=1.8,unit="Mb",weight=75) %>%
    add_row(Coverage = 20,accuracy=0,uncertainty=0,size=1,unit="Mb",weight=80) %>%
    add_row(Coverage = 10,accuracy=0,uncertainty=0,size=0.5,unit="Mb",weight=90) %>%
    add_row(Coverage = 0,accuracy=0,uncertainty=0,size=0,unit="Mb",weight=100)

perf_table %>% 
    ggplot(aes(x=Coverage, y=size)) + 
    geom_smooth(aes(weight=weight), se = F, span = 0.7) 
    
