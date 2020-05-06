ll = installed.packages()
if(!("shiny" %in% ll)){install.packages("shiny")}
if(!("shinysky" %in% ll)){remotes::install_github("AnalytixWare/ShinySky")}
if(!("shinyjs" %in% ll)){install.packages("shinyjs")}
if(!("shinythemes" %in% ll)){install.packages("shinythemes")}
if(!("shinyalert" %in% ll)){install.packages("shinyalert")}
if(!("purrr" %in% ll)){install.packages("purrr")}
if(!("dplyr" %in% ll)){install.packages("dplyr")}
if(!("leaflet" %in% ll)){install.packages("leaflet")}
if(!("htmltools" %in% ll)){install.packages("htmltools")}
if(!("easycsv" %in% ll)){install.packages("easycsv")}
if(!("sf" %in% ll)){install.packages("sf")}
if(!("readr" %in% ll)){install.packages("readr")}
if(!("beepr" %in% ll)){install.packages("beepr")}
library(shiny)
library(shinysky)
library(shinyjs)
library(shinythemes)
library(shinyalert)
library(purrr)
library(dplyr)
#library(leaflet)
library(htmltools)
library(easycsv)
#library(sf)
library(readr)
library(SIRItoGTFS)
library(beepr)



# UI ----------------------------------------------------------------------
folder_ui <- function(string, id) {
  ns <- NS(id)
  fluidRow(
    column(9,
           span(HTML(string),
                shiny::actionButton(ns("folder_chooser"),
                               label = "Pick a folder",
                               icon = icon("folder-open")
                               )
                )
    ),
    column(3,
           uiOutput(ns("ui_placeholder"))
    )
  )
}

row_server <- function(input, output, session) {
  return_value <- reactive({input$inner_element})
  folder <- reactiveVal('')
  ns <- session$ns
  # output$ui_placeholder <- renderUI({
  #
  #   req(input$folder_chooser)
  #   folder = choose_dir()
  #   folder = reactive({folder})
  #   if(class(folder) == "character" &nchar(folder) > 1) {
  #     HTML(folder)
  #   } else {
  #     HTML('')
  #   }
  # })
  observeEvent(input$folder_chooser,{
    folder(choose_dir())
    output$ui_placeholder <- renderUI({
      if(class(folder()) == "character" & nchar(folder()) > 1) {
        HTML(folder())
      } else {
        HTML('')
      }
    })
  })

  ## if we later want to do some more sophisticated logic
  ## we can add reactives to this list
  #list(return_value = return_value)
  #list(folder)
  list(folder)
}

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "<br>", collapse = "")
    )
  }
  results
}

ui =  fluidPage(
  useShinyalert(),
  theme = shinytheme("paper"),
  tabsetPanel(id="tabs",
              tabPanel("Setup",
                       # shinythemes::themeSelector(),
                       # Head
                       tags$head(tags$style(".rightAlign{float:right;direction:rtl;text-align:right;}"),
                                 tags$title("SIRI to GTFS")),
                       busyIndicator(),

                       # Header
                       titlePanel(title=div(a(
                         img(src="logo_GAMES_lab.png", align = "right"),
                         href="http://raphael.geography.ad.bgu.ac.il/GAMESLab/"),
                         "SIRI to GTFS Analysis UI - Multiple Days")),

                       hr(),

                       # Main Area Layout
                       # First Row
                       sidebarLayout(
                         # sidebar Area
                         sidebarPanel(
                             shiny::actionButton("fileschoose",
                                                 icon=icon("file-upload"),
                                                 label = "Pick SIRI Files"
                             ),
                           width = 4
                         ),
                         # Main Area
                         mainPanel(
                           htmlOutput("fileschosen"),
                           width = 8
                         )
                       ),
                       # Second Row
                       sidebarLayout(
                         # sidebar Area
                         sidebarPanel(
                           shiny::actionButton("outputchoose",
                                               icon=icon("save"),
                                               label = "Pick output location"),
                           shiny::actionButton("run",label = "Run"),

                           width = 6
                         ),
                         # Main Area
                         mainPanel(
                           htmlOutput("progress"),
                           textOutput("text1"),
                           width = 6
                         )
                       ),
                       verbatimTextOutput("out")
              )
    )
)



# Server ------------------------------------------------------------------


server = function(input, output) {


  paths <- reactiveValues(
    siri=NULL,
    folders=c(),
    filelist=NULL,
    output=NULL
  )

  data <- reactiveValues(
    buses=NULL
  )

  values <- reactiveValues()

  folders <- reactiveVal(list())

  ################################
  #
  # Select SIRI Files
  #
  ################################

  observeEvent(input$outputchoose,{
    paths$output <- easycsv::choose_dir()
    output$fileschosen <- renderUI({
      if(is.null(paths$output)){
        HTML("No Output location selected")
      }else{
        HTML(paste("Output Path Will Be:", paths$output))
      }
    })
    })

  ################################
  #
  # Select SIRI Files
  #
  ################################

  observeEvent(input$fileschoose,{
    paths$siri <- choose.files(filters = Filters[c("txt","All"),])
    output$fileschosen <- renderUI({

      if(is.null(paths$siri)){
        HTML("Nothing selected")
      }else{
        datesList = list()
        for(i in 1:length(paths$siri)){
          daten = paste0("date",i)
          datesList[[i]] = htmlOutput(daten)

        }

        tagList(datesList)


      }
    })
    paths$folders = list()
    for(i in 1:length(paths$siri)){
      local({
        dateN = paste0("date",i)
        string = paste0("GTFS folder for: ",paths$siri[i])
        inputId = paste0("folderchoose",i)

        insertUI(
          selector = "#fileschosen",
          where = "beforeBegin",
          ui = folder_ui(string,inputId)
        )

        folders_list <- isolate(folders())
        new_folder <- callModule(row_server, inputId)
        #paths$folders = c(paths$folders,new_folder)
        folders_list <- c(folders_list, new_folder)
        names(folders_list)[length(folders_list)] <- inputId
        folders(folders_list)



        # output[[dateN]] <- renderUI({
        #   span(HTML(string)
        #        ,shiny::actionButton(inputId = inputId,
        #                             label = "Pick a folder", icon = icon("folder-open")))
        # })
        #
        # observeEvent(input[[paste0("folderchoose",i)]],{
        #   paths$folders[[i]] = choose_dir()
        #   print(paths$folders)
        # })

      })

    }


  })

  ################################
  #
  # Select GTFS Folder
  #
  ################################

  observeEvent(input$folderchoose,{
    path$folder = choose_dir()

    output$folderchosen <- renderUI({
      if(is.null(path$folder)){
        "Nothing selected"
      }else{

        tryCatch({
          n = length(list.files(path$folder,pattern = ".*.txt"))
          filelist = list.files(path$folder,pattern = ".*.txt")
          filelist = substr(filelist,1,nchar(filelist)-4)
          selectedFolder = paste(path$folder, "selected")
          str1 = paste(n, "files found")
          filelist = c(selectedFolder,str1,filelist)
          for(file in filelist){
            paste(file);
          }
          HTML(paste("",filelist, sep = '<br/>'))
        }, error = function(e) e)


      }
    })

  })

  ################################
  #
  # Run
  #
  ################################

  observeEvent(input$run, {
    paths$folders <-
      lapply(folders(), function(handle) {
        handle()
      })
    paths$folders = unlist(paths$folders)
    paths$folders = paths$folders[paths$folders != ""]
    if(length(paths$siri) != length(paths$folders)){

      shinyalert("Error!", "You need a GTFS folder for each SIRI file.", type = "error")
    }else{
      withProgress(message = 'Now On', style = "notification", detail = "part 0", value = 0, {

      for(j in 1:length(paths$siri)){

        incProgress(1/length(paths$siri), detail = paste(paths$siri[j]))
        # Load SIRI
        s = read_csv(paths$siri[j])
        original_n = nrow(s)
        original_nrows = format(original_n,big.mark=",",scientific=FALSE)

        s = s[s$Latitude != 'a',]
        s = s[complete.cases(s[ , c("Latitude","Longitude")]),]
        baseName = basename(paths$siri[j])
        baseName = substr(baseName,1,nchar(baseName)-4)
        assign(x = "SIRIdf", value = s, envir = as.environment(1))

        # Load GTFS
        paths$filelist = list.files(paths$folders[j],pattern = ".*.txt")
        gtfsNames = paste0("GTFS",substr(paths$filelist,1,nchar(paths$filelist)-4))
        withProgress(message = 'Loading GTFS', style = "notification", detail = "part 0", value = 0, {
          for (i in 1:length(paths$filelist)) {

            incProgress(1/length(paths$filelist), detail = paste("loading", paths$filelist[i]))
            dat <- readr::read_csv(paste0(paths$folders[j],"/",paths$filelist[i]))
            assign(x = gtfsNames[i], value = dat, envir = as.environment(1))

            }

          })


        data$buses<- STG(SIRIdf,
                        GTFSstops,
                        GTFSagency,
                        GTFScalendar,
                        GTFSroutes,
                        GTFSstop_times,
                        GTFStrips,
                        linerefs = unique(SIRIdf$LineRef),
                        epsg = 2039)
        if(!is.null(paths$output)){
          path = paste0(path.expand(paths$output),"/",baseName,"_output.csv")
          readr::write_csv(data$buses,path)
        }
        if(j == 1){
          path = paste0(path.expand(paths$output),"/output.csv")
          readr::write_csv(data$buses,path)
        }else{
          path = paste0(path.expand(paths$output),"/output.csv")
          readr::write_csv(data$buses,path,append=TRUE)
        }





      }
        beepr::beep(8)
        assign(x = "buses", value = data$buses, envir = as.environment(1))
        shinyalert("Done", paste("Finished Analyzing All Files,\n Saved to",paths$output), type = "success")
      })

    }

  })





}




shinyApp(ui = ui, server = server)
