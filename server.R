library(shiny)
library(pdftools)
library(tidyverse)
library(topicmodels)
library(ldatuning)
library(tidytext)
library(wordcloud)
#library(qdap)


shinyServer(function(input, output, session) {
 
  options(shiny.maxRequestSize = 2*1024^3)
  doCalc <- reactiveValues(do = FALSE)
  dataFile <- reactiveValues(loaded = FALSE)
  button <- reactiveValues(selected = "none")
 
  
  ## LEITURA DOS DADOS
  
  observeEvent(input$file, { 
    
    #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    
    pdfDir <- paste0(getwd(),"/PDF")
    unlink(pdfDir, recursive=TRUE)
    unzip(input$file$datapath, exdir = pdfDir)  
    pdfFolder <- "./PDF/"
    stopwords_pt <- read_delim("_resources/stopwords.csv",";", escape_double = FALSE, trim_ws = TRUE)
    replacewords_pt <- read_delim("_resources/replacewords.csv",";", escape_double = FALSE, trim_ws = TRUE)
    
    lemma_dic <- read.delim("_resources/lemmatization-pt.txt", header = FALSE, stringsAsFactors = FALSE)
    names(lemma_dic) <- c("stem", "term")
    Encoding(lemma_dic$term)= "UTF-8"
    Encoding(lemma_dic$stem)= "UTF-8"
    
    # Leitura dos dados
    pdf2txt <- function(files){ 
      sapply(paste0(pdfFolder,files),
             function(f){ return(paste(pdf_text(f),collapse=" "))},
             USE.NAMES = FALSE)}
    alltexts <- tibble(fname = list.files(pdfFolder,recursive = TRUE)) %>%
      mutate(category = sub('/[^/]*$', '', fname)) %>%
      mutate(txt = pdf2txt(fname)) 
    
    # substituir palavras 
    #alltexts$txt <- mgsub(replacewords_pt$word, replacewords_pt$replace, alltexts$txt)
    
    # extrair categorias
    categories <- unique(alltexts$category)
    
    updateSelectInput(session, "category",choices = c("todas",categories))  
    # Preparação 
    session$userData$tokens <- alltexts %>%
      mutate(linenumber = row_number()) %>%
      unnest_tokens(word,txt) %>%
      anti_join(stopwords_pt) 

    Encoding(session$userData$tokens$word)= "UTF-8"
    
    for (j in 1:length(session$userData$tokens)){
      comparacao <- session$userData$tokens[j] == lemma_dic$term
      if (sum(comparacao) == 1){
        session$userData$tokens[j] <- as.character(lemma_dic$stem[comparacao])
      } else {
        session$userData$tokens[j] <- session$userData$tokens[j]
      }
    }
    

    session$userData$bigrams <- alltexts %>%
      unnest_tokens(bigram, txt, token = "ngrams", n =2) %>%
      separate(bigram, c("word1","word2"), sep =" ") %>% 
      filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
      filter(!word2 %in% as.vector(t(stopwords_pt$word))) %>%
      unite(bigram, word1, word2, sep = " ") 

    session$userData$trigrams <- alltexts %>%
      unnest_tokens(trigram, txt, token = "ngrams", n = 3) %>%
      separate(trigram,c("word1","word2","word3"), sep = " ") %>%
      filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
      filter(!word3 %in% as.vector(t(stopwords_pt$word))) %>%
      unite(trigram, word1, word2,word3, sep = " ") 
    
    session$userData$quadrigrams <- alltexts %>%
      unnest_tokens(quadrigram, txt, token = "ngrams", n = 4) %>%
      separate(quadrigram,c("word1","word2","word3","word4"), sep = " ") %>%
      filter(!word1 %in% as.vector(t(stopwords_pt$word))) %>%
      filter(!word4 %in% as.vector(t(stopwords_pt$word))) %>%
      unite(quadrigram, word1, word2,word3,word4, sep = " ") 
    
    cat("leitura de dados ok\n")
    
    dataFile$loaded <- TRUE
    doCalc$do <- TRUE
    button$selected <- "wc"
  })
  
  
  
  ## ATENDIMENTO DE INTERFACE
  
  observeEvent(input$category, { 
    doCalc$do <- TRUE & dataFile$loaded
  }) 
  observeEvent(input$ntopics, { 
    doCalc$do <- TRUE & dataFile$loaded
  })
  observeEvent(input$wcButton, { 
    button$selected <- "wc" 
    doCalc$do <- FALSE
  }) 
  observeEvent(input$woButton, { 
    button$selected <- "wo" 
    doCalc$do <- FALSE
  }) 
  observeEvent(input$bgButton, { 
    button$selected <- "bg" 
    doCalc$do <- FALSE
  }) 
  observeEvent(input$tgButton, { 
    button$selected <- "tg" 
    doCalc$do <- FALSE
  }) 
  observeEvent(input$qgButton, { 
    button$selected <- "qg" 
    doCalc$do <- FALSE
  }) 
  observeEvent(input$tpButton, { 
    button$selected <- "tp"
    doCalc$do <- FALSE
  }) 
  observeEvent(input$ddButton, { 
    button$selected <- "dd" 
    doCalc$do <- FALSE
  }) 
  
  output$plot <- renderPlot({ 

    if(!dataFile$loaded){ return() }
    
    if(doCalc$do){
      prepareVars(input,session)
    }
    root <- sqrt(as.integer(input$ntopics))
    numRows <- if(floor(root)==ceiling(root)) root else floor(root) 
    cat("renderizando...\n")
    
    if(button$selected == "wc"){
      #word cloud
      wordcloud(session$userData$tokens_count$word,
                session$userData$tokens_count$n, random.order = FALSE,
                max.words = 200, scale = c(3,1), colors = brewer.pal(10,"Spectral"))
    }else if(button$selected == "wo"){
      #gráfico de frequência
      session$userData$tokens_count %>%
        mutate(word = reorder(word,n)) %>%
        head(15) %>%
        ggplot(aes(word,n,fill=factor(word)))+
        geom_col()+
        xlab(NULL)+
        theme(text = element_text(size=18))+
        coord_flip()
    }else if(button$selected == "bg"){
      #gráfico de frequência
      session$userData$bigrams_count %>%
        mutate(bigram = reorder(bigram,n)) %>%
        head(15) %>%
        ggplot(aes(bigram,n,fill=factor(bigram)))+
        geom_col()+
        guides(fill=FALSE)+
        xlab(NULL)+
        theme(text = element_text(size=18))+
        coord_flip()
    }else if(button$selected == "tg"){
      #gráfico de frequência
      session$userData$trigrams_count %>%
        mutate(trigram = reorder(trigram,n)) %>%
        head(15) %>%
        ggplot(aes(trigram,n,fill=factor(trigram)))+
        geom_col()+
        guides(fill=FALSE)+
        xlab(NULL)+
        theme(text = element_text(size=18))+
        coord_flip()
    }else if(button$selected == "qg"){
      #gráfico de frequência
      session$userData$quadrigrams_count %>%
        mutate(quadrigram = reorder(quadrigram,n)) %>%
        head(15) %>%
        ggplot(aes(quadrigram,n,fill=factor(quadrigram)))+
        geom_col()+
        guides(fill=FALSE)+
        xlab(NULL)+
        theme(text = element_text(size=18))+
        coord_flip()
    }else if(button$selected == "tp"){
      # Gráfico de tópicos
      session$userData$corpus_top_terms %>%
        ggplot(aes(order, beta, fill = factor(topic))) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free", nrow = numRows) +
        xlab("Termos") +
        ylab("Beta") +
        scale_x_continuous(
          breaks = session$userData$corpus_top_terms$order,
          labels = session$userData$corpus_top_terms$term,
          expand = c(0,0),
          trans = "reverse"
        )+
        theme(text = element_text(size=18),
              axis.text.x = element_text(angle = 90, hjust = 1))+
        coord_flip()
    }else if(button$selected == "dd"){
      # Distribuição dos tópicos
      session$userData$corpus_topdist %>%
        ggplot(aes(document, gamma, fill = factor(topic))) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free", nrow = numRows) +
        xlab("Documentos") +
        ylab("Gamma") +
        theme(text = element_text(size=18))+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    }
  }, height = 600, width = 800 )
})




### CÁLCULOS

prepareVars <- function(input,session){
  cat("calculando...\n")
  if(input$category=="todas"){
    tokens <- session$userData$tokens
    bigrams <- session$userData$bigrams
    trigrams <- session$userData$trigrams
    quadrigrams <- session$userData$quadrigrams
  }else{
    tokens <- session$userData$tokens %>%
      filter(str_detect(category,input$category))
    bigrams <- session$userData$bigrams %>%
      filter(str_detect(category,input$category))
    trigrams <- session$userData$trigrams %>%
      filter(str_detect(category,input$category))
    quadrigrams <- session$userData$quadrigrams %>%
      filter(str_detect(category,input$category))
  }
  session$userData$tokens_count <- tokens %>%
    count(word, sort=TRUE) 
  session$userData$bigrams_count <- bigrams %>%
    count(bigram, sort=TRUE) 
  session$userData$trigrams_count <- trigrams %>%
    count(trigram, sort=TRUE) 
  session$userData$quadrigrams_count <- quadrigrams %>%
    count(quadrigram, sort=TRUE) 
  cat("tokens ok\n")
  
  #Análise de Tópicos
  dtm <- tokens %>%
    count(linenumber,word, sort=TRUE) %>%
    cast_dtm(linenumber,word,n) 
  corpus_lda <- dtm %>%
    LDA(k=as.integer(input$ntopics),control=list(seed=1234))
  cat("lda ok\n")
  corpus_topics <- tidy(corpus_lda,matrix="beta")
  corpus_top_terms <- corpus_topics %>%
    group_by(topic) %>%
    top_n(15,beta) %>%
    arrange(topic, -beta) %>%
    do(head(., n = 15)) %>%
    ungroup() %>%
    mutate(term=reorder(term,beta)) %>%
    mutate(order = row_number())
  session$userData$corpus_top_terms <- corpus_top_terms
  cat("beta ok\n")
  corpus_topgamma <- tidy(corpus_lda, matrix = "gamma") %>%
    arrange(desc(gamma))
  corpus_topdist <- corpus_topgamma %>%
    group_by(topic) %>%
    arrange(topic, gamma) %>%
    ungroup() 
  session$userData$corpus_topdist <- corpus_topdist
  cat("gamma ok\n")
}
