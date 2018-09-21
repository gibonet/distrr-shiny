library(shiny)
source("chooser.R")

rds_files <- dir(path = "data/", pattern = "*.rds$")
nomi_files <- dir(path = "data", pattern = "*.txt$", full.names = TRUE)

txt_files <- unlist(lapply(nomi_files, readLines))
names(rds_files) <- txt_files


# ui.R
shinyUI(navbarPage(
  title = "Filtra e ridisponi un cubo di dati",
  tabPanel("Carica dati",
           sidebarLayout(
             sidebarPanel(
               selectInput("rds_file", "Scegli il file da caricare",
                           choices = rds_files),
               br(),
               h3("Panoramica delle variabili a disposizione:"),
               verbatimTextOutput("lista_modalita")
             ),
             
             
             mainPanel(
               # tableOutput("dati"),
               dataTableOutput("dati_DT")
             )
           )
  ),
  
  tabPanel("Filtra dati",
           fluidRow(
             column(2, actionButton("selectall1", "Seleziona/deseleziona tutti")),
             column(2, actionButton("selectall2", "Seleziona/deseleziona tutti")),
             column(2, actionButton("selectall3", "Seleziona/deseleziona tutti")),
             column(2, actionButton("selectall4", "Seleziona/deseleziona tutti")),
             column(2, actionButton("selectall5", "Seleziona/deseleziona tutti"))
           ),
           
           fluidRow(
             column(2, htmlOutput("check1")),
             column(2, htmlOutput("check2")),
             column(2, htmlOutput("check3")),
             column(2, htmlOutput("check4")),
             column(2, htmlOutput("check5"))
           ),
           
           fluidRow(
             tableOutput("dati_selezione")
           )
  
           ),
  
  
  tabPanel("Ridisponi",
           fluidRow(
             column(2, htmlOutput("reshape1")),
             column(2, htmlOutput("reshape2")),
             column(2, htmlOutput("reshape3")),
             column(2, htmlOutput("reshape4")),
             column(2, htmlOutput("reshape5"))
           ),
           
           fluidRow(
             tableOutput("dati_reshape")
           )
           ),
  
  tabPanel("Esporta",
           fluidRow(
             column(3,
                    textInput("nome_file", "Esporta nel file:", value = "temp"),
                    helpText("Nota: non è necessario mettere un'estensione al nome del file"),
                    radioButtons("tipo_file", "Tipo del file:",
                                 choices = c("csv", "tsv"),
                                 selected = "csv"),
                    downloadButton('downloadData', 'Esporta / scarica')
             )
           )
           ),
  
  
  
  tabPanel("Garbage_temp",
           fluidRow(
             verbatimTextOutput("lista_dati_reactive")
           ),
           
           fluidRow(
             textOutput("var1_selezione")
           ),

           fluidRow(
             verbatimTextOutput("vars_condizioni")
           ),
           
           fluidRow(
             verbatimTextOutput("formula_reshape")
           ),
           
           fluidRow(
             column(2, htmlOutput("sel_check1"))
           ),
           
           # fluidRow(
           #   verbatimTextOutput("lista_modalita")
           # ),
           
           fluidRow(
             column(2, htmlOutput("sel_check2"))
           )
           
           ),
  
  tabPanel("PROVE",
           chooserInput(
             inputId = "scelta_disposizione",
             leftLabel = "Variabili riga",
             rightLabel = "variabili colonna",
             leftChoices = c("scelta1", "scelta2", "scelta3"),
             # dataList()$vars[-length(dataList()$vars)]
             rightChoices = c(),
             multiple = TRUE
           ),
           verbatimTextOutput("selection"),
           htmlOutput("scelta_disposizione2")
           )
))