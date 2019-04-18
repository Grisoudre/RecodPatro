#=====================================================
# Packages
#=====================================================

library(shiny)
library(questionr)
library(tidyverse)
library(stringr)
library(DT)


 options(shiny.maxRequestSize=30*1024^2, encoding = "UTF-8")
 
#======================================================
# Importation de la table comprenant les diff. niveaux de PCS
#======================================================

 

# N5 <- read.csv2("N5.csv", fileEncoding="UTF-8", stringsAsFactor=F)

#=========================================================
# Fichier server
#=========================================================


  shinyServer(function(input, output, session) {


  # A/ Table de recodage vide
  #======================================================
  
  
  # C/ Fonctions supports au formulaire
  #========================================================
  
  trim_string <- function(string) gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", string))
  
  
  # C/ 1. Get table metadata. For now, just the fields
  # Further development: also define field types
  # and create inputs generically
  GetTableMetadata <- function() {
    fields <- c(  IdRec = "IdRec",
                  Nom= "Nom",
                  Aire ="Aire",
                  Arabe = "Arabe", 
                  Africain ="Africain",
                  Asiatique="Asiatique",
                  Anglophone = "Anglophone",
                  Latin= "Latin",
                  Francais="Francais",
                  Autre="Autre",
                  NSP="NSP")
    
    result <- list(fields = fields)
    return (result)
  }
  
  
  # C/ 2. Find the next ID of a new record (in mysql, this could be done by an incremental index)
  GetNextId <- function() {
    if (exists( "Recodage" ) && nrow(Recodage) > 0) {
      max(as.integer(rownames(Recodage))) + 1
    } else {
      return (1)
    }
  }
  
  # C/ 3. C = créer
  CreateData <- function(data) {
    data <- CastData(data)
    rownames(data) <- GetNextId()
    if (exists("Recodage")) {
      Recodage <<- rbind(Recodage, data)
    } else {
      Recodage <<- data
    }
  }
  
  
  # 4. R = lire
  ReadData <- function() {
    if (exists("Recodage")) {
      Recodage
    }
  }
  
  # 5. U = Mettre à jour
  UpdateData <- function(data) {
    data <- CastData(data)
    Recodage[row.names(Recodage) == row.names(data), ] <<- data
  }
  

  
  
  # 6. D = supprimer
  DeleteData <- function(data) {
    Recodage <<- Recodage[row.names(Recodage) != unname(data["IdRec"]), ]
  }
  
  
  
  # C/ Formulaire de recodage
  #========================================================
  
  # C/ 1. Cast from Inputs to a one-row data.frame
  CastData <- function(data) { 
    datar <- data.frame(Nom = data["Nom"],
                        Aire = data["Aire"],
                        Arabe = as.logical(data["Arabe"]),
                        Africain = as.logical(data["Africain"]),
                        Asiatique = as.logical(data["Asiatique"]),
                        Anglophone = as.logical(data["Anglophone"]),
                        Latin = as.logical(data["Latin"]),
                        Francais = as.logical(data["Francais"]),
                        Autre = as.logical(data["Autre"]),
                        NSP = as.logical(data["NSP"]),
                         stringsAsFactors = FALSE)
    
    rownames(datar) <- data["IdRec"]
    return (datar)
  }
  

  
  
  # C/ 2. Return an empty, new record
  CreateDefaultRecord <- function() {
    mydefault <- CastData(list(IdRec = "0",
                               Nom= "", Aire=" ",
                               Arabe=F, Africain=F,
                               Asiatique=F, Anglophone=F,
                               Latin=F, Francais =F,
                               Autre = F, NSP = F))
    
    return (mydefault)
  }
   
 
  # C/ 3. Mises à jour
  
  UpdateInputs <- function(data, session) {
    updateTextInput(session, "IdRec", value = unname(rownames(data)))
    updateTextInput(session, "Nom", value = unname(data["Nom"]))
    updateSelectInput(session, inputId= "Aire", selected=unname(data["Aire"]))
    updateCheckboxInput(session, inputId = "Arabe", value= as.logical(data["Arabe"]))
    updateCheckboxInput(session, inputId = "Africain", value= as.logical(data["Africain"]))
    updateCheckboxInput(session, inputId = "Asiatique", value= as.logical(data["Asiatique"]))
    updateCheckboxInput(session, inputId = "Anglophone", value= as.logical(data["Anglophone"]))
    updateCheckboxInput(session, inputId = "Latin", value= as.logical(data["Latin"]))
    updateCheckboxInput(session, inputId = "Francais", value= as.logical(data["Francais"]))
    updateCheckboxInput(session, inputId = "Autre", value= as.logical(data["Autre"]))
    updateCheckboxInput(session, inputId = "NSP", value= as.logical(data["NSP"]))
  }
  
  
  
  
  # A/ Importation du tableau de données
  #===========================================================
  
  # A/ 1. Données importées (adaptation d'explore-data, Paris Descartes) :
  output$donnees.fichier.ui <- renderUI({
    list(
      fileInput("donnees.fichier.input", "Choisir le fichier :"),
      radioButtons("donnees.fichier.header",
                   "Noms de variables en 1ère ligne :",
                   c("oui", "non")),
      radioButtons("donnees.fichier.sep",
                   "Séparateur de champs :",
                   c("point-virgule" = ";",
                     "virgule" = ",",
                     "espace" = " ",
                     "tabulation" = "\t")),
      radioButtons("donnees.fichier.dec",
                   "Séparateur de décimales :",
                   c("point" = ".", "virgule" = ",")),
      radioButtons("donnees.fichier.enc",
                   "Encodage des caractères :",
                   c("UTF-8 (par défaut sur Linux/Mac)" = "UTF-8",
                     "Windows-1252 (par défaut sur Windows)" = "WINDOWS-1252")),
      uiOutput("donnees.fichier.ok")
    )
    
  })
  # A/ 1. Données importées (adaptation d'explore-data, Paris Descartes) :
  output$donnees.fichier.ui2 <- renderUI({
    list(
     fileInput("donnees.fichier.input2", "Table de recodage sauvegardée :"),
      uiOutput("donnees.fichier.ok2")
     )
    
  })
  

  
  donnees_entree2 <-reactive({
    if (is.null(input$donnees.fichier.input2)) return (NULL)
    donnees_entree2 <- NULL
    try({
      donnees_entree2 <- read.table(
        input$donnees.fichier.input2$datapath,
        header = T,
        sep = ";",
        dec = ",",
        fileEncoding = "UTF-8",
        stringsAsFactors = FALSE)
    }, silent = TRUE)
    donnees_entree2 <- unique(donnees_entree2)
    donnees_entree2
  })
  
  
  donnees_entree <-reactive({
    if (is.null(input$donnees.fichier.input)) return (NULL)
    donnees_entree <- NULL
    try({
      donnees_entree <- read.table(
        input$donnees.fichier.input$datapath,
        header = input$donnees.fichier.header == "oui",
        sep = input$donnees.fichier.sep,
        dec = input$donnees.fichier.dec,
        fileEncoding = input$donnees.fichier.enc,
        stringsAsFactors = FALSE)
    }, silent = TRUE)
    donnees_entree <- unique(donnees_entree)
    donnees_entree
  })
  
  
  
  # A/ 2. Vérification de l'importation :
  # taille et str du tableau de départ :
  
  output$Dimensions <- renderText(
    if (is.null(input$donnees.fichier.input)) return ("")
    else {
      paste("Tableau constitué de", ncol(donnees_entree()),
            "colonnes et de", nrow(donnees_entree()),"lignes.
            Détail des variables :")
      
    })
  
  
  output$Resume <- renderTable({
    tmp <- donnees_entree()
    if (is.null(tmp)) {return (NULL)}else{
      
      Resume <- data.frame( Variable = names(tmp[1]),
                            Type = class(tmp[,1]),
                            NbreValeursDiff = nrow(unique(tmp[1])))
      for (i in (2:ncol(tmp))) {
        Resume <-rbind(Resume, data.frame( Variable = names(tmp[i]),
                                           Type = class(tmp[,i]),
                                           NbreValeursDiff = nrow(unique(tmp[i]))))
      }
      Resume
    }
  })
  
  # B/ Sélections des variables clés et table recodage intermédiaire (uniquement profession)
  #===============================================
  
  
  # B/ 1. Choix de l'identifiant :
  
  
  
  output$SelectID <- renderUI({
    selectInput("ID", "Choix de l'identifiant (doit être unique) :",
                choices=c(" ",names(donnees_entree())) , selected = " ")
  })
  
  
  # B/ 2. Sélection de la variable profession à recoder :
  
  
  
  output$SelectNameBrut <- renderUI({
    selectInput("Name", "Choix de la variable nom / prénom à recoder :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })
  
  
  observeEvent(input$OK,{
    
    Table <- donnees_entree()
    Ident <- input$ID
    Name <- input$Name
    
    validate(
      need((  Name != " ") , "")
    )
    RecodageNoms<- freq(Table[Name])
    RecodageNoms$IdRec<- seq(1:nrow(RecodageNoms))
    RecodageNoms$Nom <- row.names(RecodageNoms)
    RecodageNoms$Nom <- trim_string(RecodageNoms$Nom)
    RecodageNoms <- RecodageNoms[,c("IdRec","Nom")]
    RecodageNoms[,"Aire"] <- " "
    RecodageNoms[,"Arabe"] <- F
    RecodageNoms[,"Africain"] <- F
    RecodageNoms[,"Asiatique"] <- F
    RecodageNoms[,"Anglophone"] <- F
    RecodageNoms[,"Latin"] <- F
    RecodageNoms[,"Francais"] <- F
    RecodageNoms[,"Autre"] <- F
    
    RecodageNoms[,"NSP"] <- F
    row.names(RecodageNoms) <- RecodageNoms$IdRec
    RecodageNoms$IdRec <- NULL
    RecodageNoms
    Recodage <<- unique(rbind(Recodage, RecodageNoms))
    
#     write.csv2(Recodage,"temp.csv", fileEncoding = "UTF-8", na= "", row.names=F)
      UpdateData(formData())
       data <- ReadData()[1, ]
       UpdateInputs(data, session)


    updateTabsetPanel(session, "tabSession",
                      selected = "panel2")
  })

  
  observeEvent(input$OK2,{
    
    Table <- donnees_entree2()
    Recodage <<- Table
    
    #     write.csv2(Recodage,"temp.csv", fileEncoding = "UTF-8", na= "", row.names=F)
   UpdateData(formData())
   data <- ReadData()[1 , ]
   UpdateInputs(data, session)
 
    updateTabsetPanel(session, "tabSession",
                      selected = "panel2")

     })
  
  
  

# B/ 4. Table avec les recodages :
#=========================================================
  
  RecodReact <- reactive ({
    
    UpdateData(formData())
    data <- ReadData()[as.integer(input$IdRec)+1, ]
    UpdateInputs(data, session)
    r <- ReadData()
    r
  })
  
TableFinale <- reactive ({
  donnees_entree <- donnees_entree()
  Name <- input$Name
  Recodage <- RecodReact()
  TableFinale <- merge (donnees_entree, Recodage,
                        by.x = Name, by.y = "Nom", all.x = T)
 # TableFinale <- TableFinale [, c(2: (ncol(TableFinale)-9),1,(ncol(TableFinale)-8):ncol(TableFinale) )] 
  TableFinale
  
})



# input fields are treated as a group
formData <- reactive({
  sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
})



# Select row in table -> show details in inputs
observeEvent(input$RecodageNoms_rows_selected, {
  if (length(input$RecodageNoms_rows_selected) > 0) {
    data <- ReadData()[input$RecodageNoms_rows_selected, ]
    UpdateInputs(data, session)
    
  }
  
})



observeEvent(input$submit, {
  if (input$IdRec != "0") {
    UpdateData(formData())
    data <- ReadData()[as.integer(input$IdRec)+1, ]
    UpdateInputs(data, session)
  # write.csv2(Recodage, "data/temp.csv", fileEncoding = "UTF-8", na = "", row.names = FALSE )
    
  #  write.csv2(RecodageNoms, "RecodageNoms.csv", row.names = F, fileEncoding = "UTF-8", na="")
  } else {
    CreateData(formData())
    data <- ReadData()[as.integer(input$IdRec)+1, ]
    UpdateInputs(CreateDefaultRecord(), session)
  }
}, priority = 1)

observeEvent(input$delete, {
  DeleteData(formData())
  UpdateInputs(CreateDefaultRecord(), session)
}, priority = 1)



output$Finale <- DT::renderDataTable({
  
  TableFinale()
  
}, server = FALSE, selection = "single",
extensions = c('Scroller','FixedColumns', 'Buttons'), options = list(
  deferRender = TRUE ,
  buttons= c('copy','csv','excel','pdf'),
  scrollX = TRUE, scrollY= 300,
  fixedColumns = TRUE,  pageLength = 50, lengthMenu = c(5,10,20,50, 100), searching = TRUE)
)



# display table
output$RecodageNoms <- DT::renderDataTable({
  
  #update after submit is clicked
  input$submit
  input$delete
  #update after delete is clicked
  ReadData()

}, server = FALSE, selection = "single",
colnames = unname(GetTableMetadata()$fields),
extensions = c('Scroller','FixedColumns', "Buttons"), options = list(
  buttons= c('copy','csv','excel','pdf'),
  deferRender = TRUE ,
  scrollX = TRUE, scrollY= 300,
  fixedColumns = TRUE,  pageLength = 50, lengthMenu = c(5,10,20,50,100), searching = TRUE)
)



# Télécharger la table




output$DlTable <- downloadHandler(
  
  filename=function() {
    paste0("TableAvecAires_", Sys.Date() ,".csv")
  },
  content = function(file) {
    
    TableFinale <- TableFinale()
 
    write.csv2(TableFinale, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
  }
)


output$SauvegardeRecod <- downloadHandler(
  
  filename=function() {
    paste0("RecodageNoms_", Sys.Date() ,".csv")
  },
  content = function(file) {
    
    
    write.csv2(Recodage, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
  }
)



  })



