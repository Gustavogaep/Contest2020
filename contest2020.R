#Contest 2020

library(shinydashboard)

library(shiny)

library(leaflet)

library(dplyr)

library(tidyr)

library(rgdal)

library(DT)

library(ggplot2)

library(tm)

library(wordcloud)

library(memoise)


# for text stemming

library("SnowballC")

# color palettes

library("RColorBrewer")

#Para las estadisticas============================================

#global
#=======

#initialize

library(datasets)
library(ggplot2) 

#helper function (convert vector to named list)

namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}


expected<-read.csv("expected.csv")   
actual<-read.csv("actual.csv")  

#=================================================================


#Para la network=============================================

#install.packages("networkD3")
library(networkD3)

datos<- read.csv("la_net.csv")

attach(datos)
is.data.frame(datos)
#===========================================================



#para la nube

####################
#      Global
####################



# The list of valid books
books <<- list("Meetings for education (MFE)" = "huaican",
               "Dialogues for education (DFE)" = "huampani",
               "Citizen survey (CS)" = "ica")

# Using "memoise" to automatically cache the results

getTermMatrix <- memoise(function(book) {
  
  
  
  if (!(book %in% books))
    stop("Unknown book")
  
  
  
  text <- readLines(sprintf("./%s.txt", book),
                    encoding="UTF-8")  
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "los", "las", "les", "alli", "alla","ademas"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})




######################
#        ui
######################



ui<-dashboardPage(skin = "yellow",
                  
                  #==========================
                  #    dashboardHeader
                  #==========================
                  
                  dashboardHeader(
                    
                    
                    
                    title="NCE: Consultation process",titleWidth=500,
                    
                    
                    
                    #Tareas 1: Consulta Ciudadana
                    #===================================
                    
                    dropdownMenu(
                      
                      
                      type="tasks", badgeStatus = "success",
                      
                      taskItem(value=25,color="red",
                               "Citizen survey: National"
                      ),
                      
                      taskItem(value=17,color="red",
                               "Citizen survey: Amazonas"
                      ),
                      
                      taskItem(value=80,color="red",
                               "Citizen survey: Ancash"
                               
                      ),
                      
                      taskItem(value=10,color="red",
                               "Citizen survey: Apurimac"
                               
                      ),
                      
                      taskItem(value=13,color="red",
                               "Citizen survey: Arequipa"
                               
                      ),
                      
                      taskItem(value=15,color="red",
                               "Citizen survey: Ayacucho"
                               
                      ),
                      
                      taskItem(value=20,color="red",
                               "Citizen survey: Cajamarca"
                               
                      ),
                      
                      taskItem(value=50,color="red",
                               "Citizen survey: Callao"
                               
                      ),
                      
                      taskItem(value=60,color="red",
                               "Citizen survey: Cusco"
                               
                      ),
                      
                      taskItem(value=8,color="red",
                               "Citizen survey: Huancavelica"
                               
                      ),
                      
                      taskItem(value=6,color="red",
                               "Citizen survey: Huanuco"
                               
                      ),
                      
                      taskItem(value=4,color="red",
                               "Citizen survey: Ica"
                               
                      ),
                      
                      taskItem(value=12,color="red",
                               "Citizen survey: Junin"
                               
                      ),
                      
                      taskItem(value=15,color="red",
                               "Citizen survey: La Libertad"
                               
                      ),
                      
                      taskItem(value=20,color="red",
                               "Citizen survey: Lambayeque"
                               
                      ),
                      
                      taskItem(value=23,color="red",
                               "Citizen survey: Provincia de Lima"
                               
                      ),
                      
                      taskItem(value=30,color="red",
                               "Citizen survey: Region Lima"
                               
                      ),
                      
                      taskItem(value=3,color="red",
                               "Citizen survey: Loreto"
                               
                      ),
                      
                      taskItem(value=40,color="red",
                               "Citizen survey: Madre de Dios"
                               
                      ),
                      
                      taskItem(value=11,color="red",
                               "Citizen survey: Moquegua"
                               
                      ),
                      
                      taskItem(value=7,color="red",
                               "Citizen survey: Pasco"
                               
                      ),
                      
                      taskItem(value=5,color="red",
                               "Citizen survey: Piura"
                               
                      ),
                      
                      taskItem(value=12,color="red",
                               "Citizen survey: Puno"
                               
                      ),
                      
                      taskItem(value=3,color="red",
                               "Citizen survey: San Martin"
                               
                      ),
                      
                      taskItem(value=56,color="red",
                               "Citizen survey: Tacna"
                               
                      ),
                      
                      taskItem(value=60,color="red",
                               "Citizen survey: Tumbes"
                               
                      ),
                      
                      taskItem(value=40,color="red",
                               "Citizen survey: Ucayali"
                               
                      )
                      
                      
                    ),    
                    
                    
                    
                    
                    
                    #Tareas 2: Dialogos por la educacion
                    #===================================
                    
                    dropdownMenu(
                      
                      
                      type="tasks", badgeStatus = "success",
                      
                      taskItem(value=25,color="aqua",
                               "Dialogues for education: National"
                      ),
                      
                      taskItem(value=17,color="aqua",
                               "Dialogues for education: Amazonas"
                      ),
                      
                      taskItem(value=80,color="aqua",
                               "Dialogues for education: Ancash"
                               
                      ),
                      
                      taskItem(value=10,color="aqua",
                               "Dialogues for education: Apurimac"
                               
                      ),
                      
                      taskItem(value=13,color="aqua",
                               "Dialogues for education: Arequipa"
                               
                      ),
                      
                      taskItem(value=15,color="aqua",
                               "Dialogues for education: Ayacucho"
                               
                      ),
                      
                      taskItem(value=20,color="aqua",
                               "Dialogues for education: Cajamarca"
                               
                      ),
                      
                      taskItem(value=50,color="aqua",
                               "Dialogues for education: Callao"
                               
                      ),
                      
                      taskItem(value=60,color="aqua",
                               "Dialogues for education: Cusco"
                               
                      ),
                      
                      taskItem(value=8,color="aqua",
                               "Dialogues for education: Huancavelica"
                               
                      ),
                      
                      taskItem(value=6,color="aqua",
                               "Dialogues for education: Huanuco"
                               
                      ),
                      
                      taskItem(value=4,color="aqua",
                               "Dialogues for education: Ica"
                               
                      ),
                      
                      taskItem(value=12,color="aqua",
                               "Dialogues for education: Junin"
                               
                      ),
                      
                      taskItem(value=15,color="aqua",
                               "Dialogues for education: La Libertad"
                               
                      ),
                      
                      taskItem(value=20,color="aqua",
                               "Dialogues for education: Lambayeque"
                               
                      ),
                      
                      taskItem(value=23,color="aqua",
                               "Dialogues for education: Provincia de Lima"
                               
                      ),
                      
                      taskItem(value=30,color="aqua",
                               "Dialogues for education: Region Lima"
                               
                      ),
                      
                      taskItem(value=3,color="aqua",
                               "Dialogues for education: Loreto"
                               
                      ),
                      
                      taskItem(value=40,color="aqua",
                               "Dialogues for education: Madre de Dios"
                               
                      ),
                      
                      taskItem(value=11,color="aqua",
                               "Dialogues for education: Moquegua"
                               
                      ),
                      
                      taskItem(value=7,color="aqua",
                               "Dialogues for education: Pasco"
                               
                      ),
                      
                      taskItem(value=5,color="aqua",
                               "Dialogues for education: Piura"
                               
                      ),
                      
                      taskItem(value=12,color="aqua",
                               "Dialogues for education: Puno"
                               
                      ),
                      
                      taskItem(value=3,color="aqua",
                               "Dialogues for education: San Martin"
                               
                      ),
                      
                      taskItem(value=56,color="aqua",
                               "Dialogues for education: Tacna"
                               
                      ),
                      
                      taskItem(value=60,color="aqua",
                               "Dialogues for education: Tumbes"
                               
                      ),
                      
                      taskItem(value=40,color="aqua",
                               "Dialogues for education: Ucayali"
                               
                      )
                      
                      
                    ),
                    
                    
                    
                    #Tareas 3: Jornadas por la Educacion
                    #===================================
                    
                    dropdownMenu(
                      
                      
                      type="tasks", badgeStatus = "success",
                      
                      taskItem(value=25,color="green",
                               "Meetings for education: National"
                      ),
                      
                      taskItem(value=17,color="green",
                               "Meetings for education: Amazonas"
                      ),
                      
                      taskItem(value=80,color="green",
                               "Meetings for education: Ancash"
                               
                      ),
                      
                      taskItem(value=10,color="green",
                               "Meetings for education: Apurimac"
                               
                      ),
                      
                      taskItem(value=13,color="green",
                               "Meetings for education: Arequipa"
                               
                      ),
                      
                      taskItem(value=15,color="green",
                               "Meetings for education: Ayacucho"
                               
                      ),
                      
                      taskItem(value=20,color="green",
                               "Meetings for education: Cajamarca"
                               
                      ),
                      
                      taskItem(value=50,color="green",
                               "Meetings for education: Callao"
                               
                      ),
                      
                      taskItem(value=60,color="green",
                               "Meetings for education: Cusco"
                               
                      ),
                      
                      taskItem(value=8,color="green",
                               "Meetings for education: Huancavelica"
                               
                      ),
                      
                      taskItem(value=6,color="green",
                               "Meetings for education: Huanuco"
                               
                      ),
                      
                      taskItem(value=4,color="green",
                               "Meetings for education: Ica"
                               
                      ),
                      
                      taskItem(value=12,color="green",
                               "Meetings for education: Junin"
                               
                      ),
                      
                      taskItem(value=15,color="green",
                               "Meetings for education: La Libertad"
                               
                      ),
                      
                      taskItem(value=20,color="green",
                               "Meetings for education: Lambayeque"
                               
                      ),
                      
                      taskItem(value=23,color="green",
                               "Meetings for education: Provincia de Lima"
                               
                      ),
                      
                      taskItem(value=30,color="green",
                               "Meetings for education: Region Lima"
                               
                      ),
                      
                      taskItem(value=3,color="green",
                               "Meetings for education: Loreto"
                               
                      ),
                      
                      taskItem(value=40,color="green",
                               "Meetings for education: Madre de Dios"
                               
                      ),
                      
                      taskItem(value=11,color="green",
                               "Meetings for education: Moquegua"
                               
                      ),
                      
                      taskItem(value=7,color="green",
                               "Meetings for education: Pasco"
                               
                      ),
                      
                      taskItem(value=5,color="green",
                               "Meetings for education: Piura"
                               
                      ),
                      
                      taskItem(value=12,color="green",
                               "Meetings for education: Puno"
                               
                      ),
                      
                      taskItem(value=3,color="green",
                               "Meetings for education: San Martin"
                               
                      ),
                      
                      taskItem(value=56,color="green",
                               "Meetings for education: Tacna"
                               
                      ),
                      
                      taskItem(value=60,color="green",
                               "Meetings for education: Tumbes"
                               
                      ),
                      
                      taskItem(value=40,color="green",
                               "Meetings for education: Ucayali"
                               
                      )
                      
                      
                    )
                    
                  ),
                  
                  
                  
                  #===========================
                  #     dashboardSidebar
                  #===========================
                  
                  
                  
                  dashboardSidebar(
                    
                    
                    img(src="logo.png"),
                    
                    
                    sidebarMenu(
                      
                      #About us?
                      #================
                      
                      menuItem("About us?", tabName = "dia_edu", icon = icon("users")),      
                      
                      
                      #Seccion del mapa: National progress
                      #===================================
                      
                      
                      menuItem("National progress", tabName = "m_water", icon = icon("map")),      
                      
                      
                      #Opiniones recogidas
                      #===================
                      
                      menuItem("Opinions collected", tabName = "jor_edu", icon = icon("users")),
                      
                      
                      #Red de palabras principales
                      #===========================
                      menuItem("Main words network", tabName = "encuesta", icon = icon("users")),
                      
                      
                      #Estadisticas
                      #===========================
                      menuItem("Dynamic statistics", tabName = "stat", icon = icon("bar-chart-o"))
                      
                    )
                  ),
                  
                  
                  
                  
                  #============================
                  #      dashboardBody
                  #============================
                  
                  dashboardBody(
                    
                    
                    tabItems(
                      
                      
                      # Quienes somos
                      #############################
                      
                      tabItem(
                        
                        tabName="dia_edu",
                        h2("knowing the National Education Council (NEC) and the citizenship consultation process"),
                        
                        # Haciendo tabBoxes
                        #==================
                        
                        
                        fluidRow(
                          
                          #tabBox
                          #=============
                          
                          tabBox(
                            
                            
                            height="250px",
                            
                            tabPanel("NEC","The National Education Council (NEC) is a specialized, consultative and autonomous body of the Ministry of Education of Peru. Its purpose is to participate in the formulation, consultation, monitoring and evaluation of the National Educational Project, medium and long-term educational policies and plans, and intersectoral policies that contribute to the development of education."),
                            tabPanel("Consultation process","In order to prepare the book called National Educational Project, the Council has carried out three processes of consumption: The meetings for education, where the general public participated through workshops; the dialogues for education, where only education specialists participated; and finally the citizen survey where citizens participated through a web page."),
                            tabPanel("What is shown on this dashboard?","The items on the left side of this dashboard are the following: About us, explain what the NEC is and the consultation process; National progress, shows us how many events have been held in the different departments of Peru, differentiated by type of event and target audience; Opinions collected, through a word cloud we observe the most important terms when talking about education; Main words network, shows the words that are most related to the word education.Finally; Dynamic statistics, shows us the amount of events that are expected to be carried out and those already carried out (Expected amount, Actual amount) according to the criteria (Group) that the user can choose, taking into account that the qualitative variables are: NOMBDEP, Age, Event.")
                          ),
                          
                          
                          #Su dibujito del PEN
                          #===================
                          
                          tabBox(
                            
                            height="250px",
                            width = 4,
                            
                            tabPanel("National Educational Project",img(src="pen.png"))
                          )
                        ),
                        
                        
                        
                        #infoBox with fill=TRUE
                        #=======================
                        
                        fluidRow(
                          
                          infoBox("Early Childhood needs","Early childhood needs a lot of love and care",icon=icon("fab fa-angellist"),fill=TRUE),
                          infoBox("Childhood needs","They have to play with their parents",icon=icon("child"),color="purple",fill=TRUE),
                          infoBox("Teenagers needs","Understand them and don't leave them much homework",icon=icon("user-friends"),color="yellow",fill=TRUE),
                          infoBox("Youth needs","Young people need to be trained for work",icon=icon("users"),color="red",fill=TRUE),
                          infoBox("Adults needs","Adults need constant training",icon=icon("user-tie"),color="olive",fill=TRUE),
                          infoBox("Elderly needs","The elderly need a better health system",icon=icon("fas fa-blind"),color="fuchsia",fill=TRUE)
                        )
                        
                      ),
                      
                      
                      
                      #Para el mapita relacionado al dashboardSidebar
                      #################################################
                      
                      tabItem(
                        tabName = "m_water",
                        
                        fluidRow(
                          
                          #Una columna se divide en dos cajas (box), en la superior el grafico del mapa...
                          #..en la inferior la tabla de resultados
                          #===============================================================================  
                          
                          column(width = 9,
                                 
                                 #Cajon que corresponde al grafico del mapa
                                 #=========================================
                                 
                                 box(width = NULL, solidHeader = TRUE,
                                     leafletOutput("peruMap", height=400)),
                                 
                                 
                                 
                                 #Cajaon que corresponde a la tabla de resultados
                                 #===============================================
                                 
                                 box(width=NULL,
                                     dataTableOutput("departamentoTable"))
                                 
                          ),
                          
                          
                          #Cajaoncito pequenho donde selecciono primero el "Grupo Etario" y luego el "tipo de evento"
                          #===========================================================================================
                          
                          column(width=3,
                                 
                                 box(width=NULL, 
                                     
                                     #La lista desplegable de los grupos etarios
                                     #==========================================
                                     
                                     uiOutput("etarioSelect"),
                                     
                                     #Los botoncitos que indican el tipo de Evento, son tres tipos de eventos
                                     #=======================================================================
                                     
                                     radioButtons(
                                       "meas", 
                                       "Event",
                                       c("Meetings for education"="Meetings for education", 
                                         "Dialogues for education"="Dialogues for education",
                                         "Citizen survey"="Citizen survey")
                                     )
                                 )
                          ) 
                        )
                      ),
                      
                      
                      
                      #Nube de palabras y Tabla de Necesidades y Soluciones 
                      #####################################################
                      
                      
                      tabItem(
                        tabName="jor_edu",
                        h2("The most important words according to citizens"),
                        
                        # Haciendo tabBoxes
                        #==================
                        
                        
                        fluidRow(
                          
                          #tabBox
                          #=============
                          
                          tabBox(
                            
                            width=NULL,
                            height="2000px",
                            
                            
                            tabPanel("Opinions",
                                     sidebarPanel(
                                       
                                       h3("Word Cloud"),
                                       h4("Below we show the most frequently mentioned words, differentiated by the type of event used to collect the information."),
                                       
                                       
                                       #Insertando la Nube de Palabras:
                                       
                                       selectInput("selection", "Choose the information collection event:",
                                                   choices = books),
                                       actionButton("update", "Change"),
                                       hr(),
                                       sliderInput("freq",
                                                   "Minimum frequency for the word to be considered:",
                                                   min = 1,  max = 50, value = 15),
                                       sliderInput("max",
                                                   "Maximum number of words in the cloud:",
                                                   min = 1,  max = 300,  value = 100)
                                     ),
                                     
                                     # Show Word Cloud
                                     mainPanel(
                                       plotOutput("plotito")
                                       
                                     )
                                     
                                     
                                     
                                     #Cerrando la Nube de Palabras========================================================
                                     
                                     
                            )
                            
                          )
                          
                        )
                        
                        
                      ),
                      
                      
                      # Red de palabras
                      ##########################
                      
                      
                      tabItem(
                        tabName="encuesta",
                        h2("The words that are most associated with education are the following"),
                        
                        
                        fluidRow(
                          
                          #tabBox
                          #=============
                          
                          tabBox(
                            width=NULL,
                            height="2000px",
                            
                            tabPanel( "About education",
                                      
                                      sliderInput("opacity", "Color Graduality", 0.6, min = 0.1,
                                                  max = 1, step = .1),
                                      
                                      
                                      simpleNetworkOutput("simple")
                                      
                                      
                            )   
                            
                          )
                          
                        )
                        
                      ),
                      
                      # Estadisticas dinamicas
                      ##########################
                      
                      
                      tabItem(
                        tabName="stat",
                        h2("Quantities according to chosen criteria"),
                        
                        
                        fluidRow(
                          
                          #tabBox
                          #=============
                          
                          tabBox(
                            width=NULL,
                            height="2000px",
                            
                            tabPanel("Dynamic descriptive",sidebarPanel(
                              
                              selectInput("dataset","Data:", 
                                          list(expected = "expected", actual = "actual")
                              ),
                              
                              uiOutput("variable"), 	
                              uiOutput("group"),  		
                              selectInput("plot.type","Plot Type:", 
                                          list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
                              ),
                              checkboxInput("show.points", "show points", TRUE)
                            ),	
                            
                            # output	
                            
                            mainPanel(
                              h3(textOutput("caption")),
                              #h3(htmlOutput("caption")),
                              uiOutput("plot") # depends on input 
                            )
                            )   
                            
                          )
                          
                          
                        )
                        
                      )
                      
                    )
                    
                  )
                  
)



##########################################
#               server
###########################################


#==Primer segmento del app mapita=======================================================================


# El mapa de poligonos en blanco y negro
departamentos<-readOGR(dsn="BAS_LIM_DEPARTAMENTO.shp", layer="BAS_LIM_DEPARTAMENTO")

# Cut out unnecessary columns
departamentos@data<-departamentos@data[,c(1,3)]

# transform to WGS884 reference system 
departamentos<-spTransform(departamentos, CRS("+init=epsg:4326"))

# Find the edges of our map
bounds<-bbox(departamentos)

# Get the income data 
income_long2<-read.csv("income_long2.csv")  



#===Fin del primer segmento del app==============================================================================


server<-function(input,output,session){
  
  
  
  
  #Para el mapita
  #===============
  
  
  
  #====Segmento del app==========================================================================================================
  
  
  getDataSet<-reactive({
    
    # Get a subset of the income data which is contingent on the input variables..
    #...meas es simplemente el ID de las botoncitos de bolita
    
    dataSet<-income_long2[income_long2$Etario==input$dataEtario & income_long2$Measure==input$meas,]  
    
    # Copy our GIS data
    joinedDataset<-departamentos
    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by="NOMBDEP"))
    
    
    joinedDataset
    
  })
  
  
  # Due to use of leafletProxy below, this should only be called once...
  #Debido al uso de leafletProxy a continuación, esto solo debe llamarse una vez
  
  output$peruMap<-renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      
      # Centre the map in the middle of our co-ordinates
      setView(mean(bounds[1,]),
              mean(bounds[2,]),
              zoom=5 
      )       
    
  })
  
  
  observe({
    theData<-getDataSet() 
    
    # colour palette mapped to data
    pal <- colorQuantile("YlGn", theData$Porcentaje, n = 2) 
    
    
    
    # set text for the clickable popup labels
    
    departamento_popup <- paste0("<strong>Region: </strong>", 
                                 theData$NOMBDEP, 
                                 "<br><strong>",
                                 input$meas," 
                                 - Expected_amount: </strong>",
                                 formatC(theData$Cantidad_Esperada, format="d", big.mark=' '),
                                 "<br><strong>",
                                 input$meas," 
                                 - Actual_amount: </strong>",
                                 formatC(theData$Cantidad_Realizada, format="d", big.mark=' '),
                                 "<br><strong>",
                                 input$meas," 
                                 - Percentage: </strong>",
                                 formatC(theData$Porcentaje, format="d", big.mark=' ')
    )
    
    
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    
    leafletProxy("peruMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$Porcentaje), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 2,
                  popup = departamento_popup)  
    
  })
  
  
  # table of results, rendered using data table
  #La tablita que aparece abajito del mapa del Peru
  #================================================
  
  output$departamentoTable <- renderDataTable(
    
    datatable({
      
      dataSet<-getDataSet()
      
      # Just get name and value columns
      dataSet<-dataSet@data[,c(1,5,6,7)] 
      
      names(dataSet)<-c("Region","Expected amount","Actual amount",paste0(input$meas,"- Percentage"))
      
      dataSet
    },
    
    options = list(lengthMenu = c(5, 10, 25), pageLength = 10))
    
  )
  
  
  # year selecter; values based on those present in the dataset
  #Esta seccion es sobre la parte del cuadrito chiquito donde aparece el subtitulo
  # Target audience cuyo desplegable corresponde a "dataEtario"
  #=====================================================================
  
  output$etarioSelect<-renderUI({
    etarioRange<-sort(unique(as.factor(income_long2$Etario)), decreasing=TRUE)
    selectInput("dataEtario", "Target audience", choices=etarioRange, selected=etarioRange[1])
  })
  
  
  #==Fin del segmento mapita===================================================================================================
  
  
  
  
  #Para la nube de palabras
  #########################
  
  # Define a reactive expression for the document term matrix
  
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plotito <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),                  
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  
  #Para mi red de palabras==========================================
  
  output$simple <- renderSimpleNetwork({
    
    networkData <- data.frame(source, target)
    
    simpleNetwork(networkData, height = NULL, width = NULL,
                  linkDistance = 100, charge = -80, fontSize = 10, 
                  fontFamily = "serif",linkColour = "#999", 
                  nodeColour = "#3182bd", zoom = T, opacity = input$opacity)
  }) 
  
  
  #=================================================================
  
  
  
  
  #Para las estadisticas dinamicas==================================================
  
  #update variable and group based on dataset
  
  output$variable <- renderUI({ 
    
    obj<-switch(input$dataset,
                "expected" = expected,
                "actual" = actual)
    
    var.opts<-namel(colnames(obj))
    selectInput("variable","Variable:", var.opts) # actualiza UI 				 
  }) 
  
  
  output$group <- renderUI({ 
    
    obj<-switch(input$dataset,
                "expected" = expected,
                "actual" = actual)
    
    var.opts<-namel(colnames(obj))
    selectInput("group","Groups:", var.opts) # actualiza UI 				 
  }) 
  
  
  
  
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })
  
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  
  #plotting function using ggplot2
  #===============================
  
  output$p <- renderPlot({
    
    plot.obj<<-list() 
    plot.obj$data<<-get(input$dataset) 
    plot.obj$variable<<-with(plot.obj$data,get(input$variable)) 
    plot.obj$group<<-with(plot.obj$data,get(input$group)) 
    
    #dynamic plotting options
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= 	geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge")
    )
    
    require(ggplot2)
    #plotting theme
    .theme<- theme(
      axis.line = element_line(colour = 'gray', size = .75), 
      panel.background = element_blank(),  
      plot.background = element_blank()
    )	 
    if(input$plot.type=="boxplot")	{		
      p<-ggplot(plot.obj$data, 
                aes(
                  x 		= plot.obj$group, 
                  y 		= plot.obj$variable,
                  fill 	= as.factor(plot.obj$group)
                )
      ) + plot.type
      
      if(input$show.points==TRUE)
      { 
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      
    } else {
      
      p<-ggplot(plot.obj$data, 
                aes(
                  x 		= plot.obj$variable,
                  fill 	= as.factor(plot.obj$group),
                  group 	= as.factor(plot.obj$group),
                  
                )
      ) + plot.type
    }
    
    p<-p+labs(
      fill 	= input$group,
      x 		= "",
      y 		= input$variable
    )  +
      .theme
    print(p)
  })	
  
  
  #=================================================================================
  
}


shinyApp(ui,server)
