library(shiny)

require(dplyr)
require(stringr)
require(stringi)
require(sbo)

replace_pattern <- '[!\\p{Pd}:;\\?…]'
remove_pattern <- '[\\d+\\"""#$%&()\\*\\+,\\/<\\=>@\\[\\]\\^_`‘“”¤¦{\\|}~\\t\\n]'
load('./data/sbo.model.rData')
sbo.pred <- sbo_predictor(sbo.model)

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

update_text <- function(input_text, word){
    last_char <- str_sub(input_text, start= -1)
    last_space <- stri_locate_last(input_text, fixed = " ")[1]
    if (last_char == "" | is.na(last_space)){
        return(paste(word, ""))
    }
    active_input <- str_sub(input_text, 1, last_space-1)
    return(paste(active_input, word, ""))
}

suggested_text <- predict3("")

ui <- fluidPage(

    tags$head(
        tags$script("
          Shiny.addCustomMessageHandler('selectText', function(message) {
            $('#txt_input').select();
          });
    ")
    ),
    
    theme = bslib::bs_theme(bootswatch = "pulse"),
    
    title = "Text Prediction Demonstration Using k-gram with Stupid Back-off Smoothing",
    
    mainPanel(
        titlePanel(
            "Text Prediction Demonstration Using k-gram with Stupid Back-off Smoothing"
        ),
        width = "100%",
        class = "mx-4"
    ),
    
    mainPanel(
        tags$script(HTML("
            $(document).ready(function() {
                $('.btn').on('click', function(){$(this).blur()});
            })
            $(document).on('shiny:connected', function(){
                Shiny.setInputValue('loaded', 1);
                Shiny.addCustomMessageHandler('focus', function(x){
                    $('#txt_input ~ .selectize-control > .selectize-input').click();
                });
            });
        ")),
        fluidRow(
            textAreaInput("txt_input", "Type text here:", width = "100%", rows=3)
        ),
        fluidRow(
            actionButton("btn_pred1", suggested_text[1,1], width = "31%", class="me-1 me-md-2 mb2"),
            actionButton("btn_pred2", suggested_text[2,1], width = "31%", class="mx-1 mx-md-2 mb2"),
            actionButton("btn_pred3", suggested_text[3,1], width = "31%", class="ms-1 ms-md-2 mb2"),
            class="mx-auto"
        ), 
        fluidRow(
            column(12,
            hr(),
            h4("Instructions"),
            HTML("<ul>"),
            HTML("<li>Start typing or click a suggested word to begin.</li>"),
            HTML("<li>Words are predicted up on up to a maximum of the 3 previous 
                 words in any sentence. The three predicted words with highest probability
                 are shown in the buttons above.</li>"),
            HTML("<li>Prediction will start over when an end-of-sentence character 
                 (.?!:;) followed by a space is detected.</li>"),
            HTML("<li>If a partial match is found to the last word as you type, 
                 the suggested words will update with those matches. If there are
                 less than three matches, the remaining words will be made up from
                 most probable default predictions for the previous three words.</li>"),
            HTML("<li>Due to memory constraints during model training, the match 
                 list is limited to 30 words.</li>"),
            HTML("<li>When no matches are found for a partially typed word, the 
                 suggested words will revert to most probable predicted values for the 
                 previous three completed words.</li>"),
            HTML("<li>Click a word to add it to the text input. </li>"),
            HTML("<li>When the last character is a space (or new line), it will 
                 be appended to the input, otherwise the last word is considered 
                 to be a partial word and replaced by the clicked suggested word.</li>"),
            HTML("<li>Due to limitations of the demo, the prediction will always be on 
                 the end of the input rather than the cursor position. Clicking on a 
                 suggested word will add it to the end of the input only.</li>"),
            HTML("</ul>"),
            hr()
            )
        ),
        width = "100%",
        class = "mx-5"
    ),
    

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    timer <- reactiveTimer(100)
    
    observe( {
        suggested_text <<- predict3(input$txt_input)
        
        updateActionButton(session, "btn_pred1",
                           label = suggested_text[1,1]
        )
        
        updateActionButton(session, "btn_pred2",
                           label = suggested_text[2,1]
        )
        
        updateActionButton(session, "btn_pred3",
                           label = suggested_text[3,1]
        )
        
    })
    
    observeEvent(input$btn_pred1, {
        updateTextAreaInput(session, "txt_input",
                        value = update_text(input$txt_input, suggested_text[1,1])
        )
        session$sendCustomMessage("selectText", "select")
    })
    
    observeEvent(input$btn_pred2, {
        updateTextAreaInput(session, "txt_input",
                        value = update_text(input$txt_input, suggested_text[2,1])
        )
        session$sendCustomMessage("selectText", "select")
    })
    
    observeEvent(input$btn_pred3, {
        updateTextAreaInput(session, "txt_input",
                        value = update_text(input$txt_input, suggested_text[3,1])
        )
        session$sendCustomMessage("selectText", "select")
    })
    
    observeEvent(input$loaded, {
        session$sendCustomMessage("selectText", "select")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
