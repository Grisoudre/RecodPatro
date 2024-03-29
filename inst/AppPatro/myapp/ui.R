
library(shinythemes)
library(shinyjs)
library(DT)

# --------------------------- TITRE -------------------------------#

shinyUI(
  navbarPage("RecodPatro",id="tabSession",

             #--------------------------- ONGLET importation et filtres -------------------------#
             # 1. Import -------------
             tabPanel("1. Importation du tableau de départ", value="panel1",
                      fluidPage(
                        tags$head(tags$style(HTML(
                          "h6{ background-color: #FFF3BE ; font-size:16px;
                          font-family: calibri, Arial, sans-serif ; font-size:16px;}")
                          #,type="text/css",
                          #          ".shiny-output-error { visibility: hidden; }",
                          #           ".shiny-output-error:before { visibility: hidden; }"
                        )
                        )

                        ,theme=shinytheme("simplex"),useShinyjs(),
 
  # options(encoding = "UTF-8"),
                        (column(12,h6('ATTENTION : Avant toute action,
                                    cliquer sur "Open in browser", sans quoi les
                                    boutons et téléchargements ne fonctionneront pas.'))),

                        column(5,h4("1. Table contenant la variable à recoder (.txt ou .csv)"),
                               h6("La table brute est une base de données dont les lignes correspondent aux individus statistiques.
                                  Elle doit être en format texte (.txt ou .csv) ; Le délimitateur,
                                  l'extension du fichier et l'encodage des caractères sont précisés.
                                  Elle est importée en passant par le bouton \"browse\"."),
                               wellPanel(
                                 uiOutput("donnees.fichier.ui"))
                               ),
                        column(7,
                               fluidRow(column(12,
                                               h4("2. Variable à recoder"),
                                               h6('Attention : Même en cas de reprise d\'une table de recodage,
                                                   choisir la variable "Nom"'),
                                               uiOutput("SelectNameBrut"),
                                               h4("3. Choix des modalités à recoder"),
                                               h6("Soit utilisation des modalités de la variable sélectionnée,
                                                   soit reprise d'une table de recodage en cours"),
                                               wellPanel(
                                                 actionButton("OK", "A. Commencer avec la variable sélectionnée")),
                                                 h4("OU BIEN"),
                                               wellPanel(
                                               uiOutput("donnees.fichier.ui2"),
                                               actionButton("OK2","B. Utiliser une table de recodage sauvergardée")),
                                               
                                               
                                               h4("4. Détail du tableau importé"),
                                               h6("Vérifier que le tableau a correctement été
                                                  importé à l'aide du résumé suivant :"),
                                               textOutput("Dimensions"),
                                               tableOutput("Resume")
                                               )
                               )
                               
                        ))),


             #--------------------------- ONGLET tris à plat / croisés -------------------------#
# 2. Recodages -------------
             tabPanel("2. Recodage des noms ou prénoms", value= "panel2",
                      fluidPage(
                        h4("Aperçu de la table de recodage"),
                        
                        fluidRow(column(11,
                                        DT::dataTableOutput("RecodageNoms"))),
                        h4("Modalité à recoder"), 
                        fluidRow(
                          column(10,shinyjs::disabled(textInput("Nom", "Nom ou prénom à recoder", " ", width='100%')),offset = 0)
                          ,
                          column(1,shinyjs::disabled(textInput("IdRec", "Id.", " ")))
                        ),
                        
                        fluidRow(
                          column(11, selectInput("Aire", "Origine géographique",
                                                 choices=c(" ","Arabe","Africaine","Asiatique",
                                                           "Anglophone","Latine","Française",
                                                           "Autre","Ne sait pas"), width='100%',  selected=NULL),offset = 0)
                        ),
                        
                        h4("Ou bien en cases à cocher"),
                        fluidRow(
                          column(4,
                                 checkboxInput("Arabe","Moyen-Orient, Maghreb, musulman", value=F),
                                 checkboxInput("Africain","Afrique", value=F),
                                 checkboxInput("Asiatique","Asie", value=F)),
                          column(4,
                                 checkboxInput("Anglophone","Anglo-saxon", value=F),
                                 checkboxInput("Latin","Latin : hispanique, lusophone, italien", value=F),
                                 checkboxInput("Francais","Français", value=F)),
                          column(4,
                                 checkboxInput("Autre","Autre", value=F),
                                 checkboxInput("NSP","Indéterminé, ne sais pas", value=F) )
                          
                          ),
                        
                        hr(),
                        h4("Boutons"),
                        actionButton("submit", "Enregistrer + suivant"),
                        
                        h4("Sauvegarder la table de recodage - pour plus tard"),
                        downloadButton("SauvegardeRecod", "Télécharger la table de recodage"),
                        hr()
                        
                        
                        
                        
                        
                      )
             ),
             #--------------------------- Aperçu du tableau général avec recodages -------------------------#
             
             # 3. Aperçu ----------

             tabPanel("3. Aperçu du tableau général", value="panel3",
                      fluidPage(h4("Tableau"),
                                dataTableOutput("Finale"),
                                h4("Télécharger"),
                                downloadButton("DlTable", "Télécharger la table"),
                                hr()
                      )
             ),
             
             #--------------------------- SUPPRIMER messages d'erreur -------------------------#

             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }")



  )
)
