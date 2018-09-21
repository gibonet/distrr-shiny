library(shiny)
library(dplyr)
library(lazyeval)
library(reshape2)

# server.R
shinyServer(function(input, output, session){
  
  dataInput <- reactive({
    if(is.null(input$rds_file))
      return(NULL)
    
    readRDS(paste0("data/", input$rds_file))
  })
  
  ##########################
  dataList <- reactive({
    dati <- dataInput()
    vars <- colnames(dati)
    input_list <- list(input$var1, input$var2, input$var3, input$var4, input$var5)
    condizioni <- vector(mode = "list", length = length(vars) - 1)
    for(i in seq_along(condizioni)){
      condizioni[[i]] <- interp(~vars_ %in% input_list_, vars_ = as.name(vars[i]), 
                                input_list_ = input_list[[i]])
    }
    
    dati_selezione <- dataInput() %>%
      filter_(.dots = condizioni)
    
    # reshape_list <- list(input$var1_reshape, input$var2_reshape, input$var3_reshape,
    #                      input$var4_reshape, input$var5_reshape)
    
    list(dati = dati, 
         vars = vars,
         input_list = input_list,
         condizioni = condizioni,
         dati_selezione = dati_selezione
         )
  })
  ##########################
  
  vars_unique <- reactive({
    # lista con i vettori dei valori unici di ogni variabile dei dataInput()
    k <- length(dataList()$vars) - 1
    cat_vars <- dataList()$vars[1:k]
    lapply(dataList()$dati %>% select_(.dots = cat_vars),
           function(x) as.character(unique(x)))
  })
  
  # output$dati <- renderTable({
  #   inFile <- input$rds_file
  #   
  #   if(is.null(inFile))
  #     return(NULL)
  #   
  #   dataList()$dati %>% head()
  # })
  
  
  output$dati_DT <- renderDataTable({
    inFile <- input$rds_file
    
    if(is.null(inFile))
      return(NULL)
    
    dataList()$dati
  })
  
  # Tab "Filtra dati"
  output$check1 <- renderUI({
    checkboxGroupInput(
      "var1",
      colnames(dataInput())[1],
      choices = as.character(unique(dataInput()[[1]])),
      selected = as.character(unique(dataInput()[[1]]))[1]
    )
  })
  
  # Seleziona/deseleziona tutti (prima prova)
  observe({
    if(input$selectall1 > 0) {
      if(input$selectall1 %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="var1",
                                 choices = as.character(unique(dataInput()[[1]])),
                                 selected = as.character(unique(dataInput()[[1]])))
      }else{
        updateCheckboxGroupInput(session=session, 
                                 inputId="var1",
                                 choices = as.character(unique(dataInput()[[1]])),
                                 selected = c())
      }}
  })
  
  
  output$check2 <- renderUI({
    checkboxGroupInput(
      "var2",
      colnames(dataInput())[2],
      choices = as.character(unique(dataInput()[[2]])),
      selected = as.character(unique(dataInput()[[2]]))[1]
    )
  })
  
  
  output$check3 <- renderUI({
    checkboxGroupInput(
      "var3",
      colnames(dataInput())[3],
      choices = as.character(unique(dataInput()[[3]])),
      selected = as.character(unique(dataInput()[[3]]))[1]
    )
  })
  
  
  output$check4 <- renderUI({
    checkboxGroupInput(
      "var4",
      colnames(dataInput())[4],
      choices = as.character(unique(dataInput()[[4]])),
      selected = as.character(unique(dataInput()[[4]]))[1]
    )
  })
  
  
  output$check5 <- renderUI({
    if(colnames(dataInput())[5] != "valore"){
      checkboxGroupInput(
        "var5",
        colnames(dataInput())[5],
        choices = as.character(unique(dataInput()[[5]])),
        selected = as.character(unique(dataInput()[[5]]))[1]
      )
    }
  })
  
  
  # Seleziona/deseleziona tutti (da var2 a var5)
  observe({
    if(input$selectall2 > 0){
      if(input$selectall2 %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="var2",
                                 choices = as.character(unique(dataInput()[[2]])),
                                 selected = as.character(unique(dataInput()[[2]])))
      }else{
        updateCheckboxGroupInput(session=session, 
                                 inputId="var2",
                                 choices = as.character(unique(dataInput()[[2]])),
                                 selected = c())
      }}
  })
  
  # var3
  observe({
    if(input$selectall3 > 0){
      if(input$selectall3 %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="var3",
                                 choices = as.character(unique(dataInput()[[3]])),
                                 selected = as.character(unique(dataInput()[[3]])))
      }else{
        updateCheckboxGroupInput(session=session, 
                                 inputId="var3",
                                 choices = as.character(unique(dataInput()[[3]])),
                                 selected = c())
      }}
  })
  
  # var4
  observe({
    if(input$selectall4 > 0){
      if(input$selectall4 %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="var4",
                                 choices = as.character(unique(dataInput()[[4]])),
                                 selected = as.character(unique(dataInput()[[4]])))
      }else{
        updateCheckboxGroupInput(session=session, 
                                 inputId="var4",
                                 choices = as.character(unique(dataInput()[[4]])),
                                 selected = c())
      }}
  })
  
  # Seleziona/deseleziona tutti (var5)
  observe({
    if(input$selectall5 > 0){
      if(input$selectall5 %% 2 == 1){
        updateCheckboxGroupInput(session=session, 
                                 inputId="var5",
                                 choices = as.character(unique(dataInput()[[5]])),
                                 selected = as.character(unique(dataInput()[[5]])))
      }else{
        updateCheckboxGroupInput(session=session, 
                                 inputId="var5",
                                 choices = as.character(unique(dataInput()[[5]])),
                                 selected = c())
      }}
  })
  
  
  output$dati_selezione <- renderTable({
    dataList()$dati %>%
      filter_(.dots = dataList()$condizioni)
  })
  
  
  # Tab "Ridisponi"
  output$reshape1 <- renderUI({
    selectInput(
      "var1_reshape",
      dataList()$vars[1],
      choices = c("in riga", "in colonna"),
      selected = "in riga"
    )
  })

  output$reshape2 <- renderUI({
    selectInput(
      "var2_reshape",
      dataList()$vars[2],
      choices = c("in riga", "in colonna"),
      selected = "in riga"
    )
  })
  
  output$reshape3 <- renderUI({
    selectInput(
      "var3_reshape",
      dataList()$vars[3],
      choices = c("in riga", "in colonna"),
      selected = "in riga"
    )
  })
  
  output$reshape4 <- renderUI({
    selectInput(
      "var4_reshape",
      dataList()$vars[4],
      choices = c("in riga", "in colonna"),
      selected = "in riga"
    )
  })

  output$reshape5 <- renderUI({
    if(dataList()$vars[5] != "valore"){
      selectInput(
        "var5_reshape",
        dataList()$vars[5],
        choices = c("in riga", "in colonna"),
        selected = "in riga"
      )
    }
  })
  
  
  string_formula <- reactive({
    reshape_list <- c(input$var1_reshape, input$var2_reshape, input$var3_reshape,
                      input$var4_reshape, input$var5_reshape)
    
    k <- length(dataList()$condizioni)

    nomi_vars <- dataList()$vars[1:k]
    reshape_list <- reshape_list[1:k]
    # nomi_vars <- colnames(dataInput())[1:length(reshape_list)]  # prova
    
    var_colonne <- nomi_vars[reshape_list == "in colonna"]
    if(length(var_colonne) == 0) var_colonne <- " . "
    var_righe <- nomi_vars[reshape_list == "in riga"]
    if(length(var_righe) == 0) var_righe <- " . "
    
    left_side <- paste(var_righe, collapse = " + ")
    
    right_side <- paste(var_colonne, collapse = " + ")
    string_formula <- paste(left_side, right_side, sep = " ~ ")
    
    string_formula <- as.formula(string_formula)
  })
  
  
  dati_reshape_ <- reactive({
    dataList()$dati_selezione %>%
      dcast(string_formula(), value.var = "valore")
  })
  
  output$dati_reshape <- renderTable({
    dati_reshape_()
  })
  
  
  # Tab "Esporta"
  output$downloadData <- downloadHandler(
    filename = function(){
      paste(input$nome_file, input$tipo_file, sep = ".")
    },
    
    content = function(file){
      sep <- switch(input$tipo_file, "csv" = ";", "tsv" = "\t")
      
      write.table(dati_reshape_(), file, sep = sep,
                  row.names = FALSE, na = "")
    }
  )
  
  
  
  
  
  
  #############################################################################  
  #############################################################################
  # output nel tab "Garbage_temp" (che si potranno cancellare)
  output$lista_dati_reactive <- renderPrint({
    str(dataList())
  })
  
  output$vars_condizioni <- renderPrint({
    vars <- colnames(dataInput())
    input_list <- list(input$var1, input$var2, input$var3, input$var4, input$var5)
    condizioni <- vector(mode = "list", length = length(vars) - 1)
    for(i in seq_along(condizioni)){
      condizioni[[i]] <- interp(~vars_ %in% input_list_, vars_ = as.name(vars[i]),
                                input_list_ = input_list[[i]])
    }
    print(condizioni)
  })
  
  output$var1_selezione <- renderText({
    vars <- colnames(dataInput())
    # input$var1
    # vars
    input_list <- list(input$var1, input$var2, input$var3, input$var4, input$var5)
    condizioni <- vector(mode = "list", length = length(vars) - 1)
    for(i in seq_along(condizioni)){
      condizioni[[i]] <- interp(~vars_ %in% input_list_, vars_ = as.name(vars[i]),
                                input_list_ = input_list[[i]])
    }
    as.character(condizioni[[1]])
  })
  
  output$formula_reshape <- reactive({
    "Stampa di prova"
    reshape_list <- c(input$var1_reshape, input$var2_reshape, input$var3_reshape,
                      input$var4_reshape, input$var5_reshape)
    
    # nomi_vars <- dataList()$vars[1:length(reshape_list)]
    k <- length(dataList()$condizioni)
    
    nomi_vars <- dataList()$vars[1:k]
    reshape_list <- reshape_list[1:k]
    
    var_colonne <- nomi_vars[reshape_list == "in colonna"]
    if(length(var_colonne) == 0) var_colonne <- " . "
    var_righe <- nomi_vars[reshape_list == "in riga"]
    if(length(var_righe) == 0) var_righe <- " . "
    
    left_side <- paste(var_righe, collapse = " + ")
    
    right_side <- paste(var_colonne, collapse = " + ")
    string_formula <- paste(left_side, right_side, sep = " ~ ")
    
    list(
      # nomi_vars,
      # reshape_list,
      string_formula
    )
  })
  
  output$sel_check1 <- renderUI({
    selectInput(
      "sel1",
      colnames(dataInput())[1],
      choices = as.character(unique(dataInput()[[1]])),
      selected = as.character(unique(dataInput()[[1]]))[1],
      multiple = TRUE
    )
  })
  
  output$lista_modalita <- renderPrint({
    vars_unique()
  })
  
  output$sel_check2 <- renderUI({
    selectizeInput(
      "sel2",
      label = colnames(dataInput())[1],
      choices = as.character(unique(dataInput()[[1]])),
      selected = as.character(unique(dataInput()[[1]]))[1],
      multiple = TRUE
    )
  })
  
  output$selection <- renderPrint({
    input$scelta_disposizione
  })
  
  output$scelta_disposizione2 <- renderUI({
    chooserInput(
      inputId = "scelta_disposizione3",
      leftLabel = "Variabili riga",
      rightLabel = "variabili colonna",
      leftChoices = dataList()$vars[-length(dataList()$vars)],
      rightChoices = c(),
      multiple = TRUE
    )
  })
  
})