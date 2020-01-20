library(shiny)
library(shinysky)
library(shinythemes)
library(easycsv)
library(readr)

lineNames = c()

ui <- fluidPage(
  # shinythemes::themeSelector(),
  theme = shinytheme("paper"),
  busyIndicator(),
  # Application title
  titlePanel("SIRI to GTFS Analysis UI"),


  sidebarLayout(
    sidebarPanel(
      actionButton("filechoose",label = "Pick a file"),
      actionButton("run",label = "Run")
    ),

    mainPanel(
      htmlOutput("filechosen")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      actionButton("folderchoose",label = "Pick a folder"),
      actionButton("run2",label = "Run")
    ),

    mainPanel(
      htmlOutput("folderchosen")
    )
  ),
  fluidRow(
    column(4,

           hr(),
           htmlOutput("selectOperator"),
           htmlOutput("selectUI"),
           # selectInput('inLinerefs', 'Options', lineNames, multiple=TRUE, selectize=FALSE),

    )
  ),
  fluidRow(
    column(4,

           hr(),
           htmlOutput("selectOperator"),
           htmlOutput("selectUI"),
           # selectInput('inLinerefs', 'Options', lineNames, multiple=TRUE, selectize=FALSE),

    )
  )
)


server <- function(input, output) {

  path <- reactiveValues(
    siri=NULL,
    folder=NULL
  )




  observeEvent(input$filechoose,{
    path$siri <- choose.files(filters = Filters[c("txt","All"),])
      # file.choose()

  })

  observeEvent(input$run,{
    tryCatch({
      s = read_csv(path$siri)
      original_n = nrow(s)
      original_nrows = format(original_n,big.mark=",",scientific=FALSE)

      s = s[s$Latitude != 'a',]
      s = s[complete.cases(s[ , c("Latitude","Longitude")]),]
      assign(x = "SIRIdf", value = s, envir = as.environment(1))
      now_n = nrow(s)
      nrows = format(now_n,big.mark=",",scientific=FALSE)
      str1 = paste0("SIRI table had ",original_nrows,",<br> with ",nrows, " valid rows (", round((now_n/original_n)*100,2),"%)")
      output$filechosen <- renderUI({
        HTML(str1)
      })

      output$selectUI <- renderUI({
        selectInput('inLinerefs', 'Options',
                    unique(s$LineRef),
                    size = 10,
                    multiple=TRUE, selectize=FALSE)
      })
    }, error = function(e) HTML(e))
  })

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

  observeEvent(input$run2,{
    style = "notification"
    dat <- data.frame(x = numeric(0), y = numeric(0))
    if(is.null(path$folder)){
      output$folderchosen <- renderUI({
        HTML("No Folder Selected")
      })
    }else{
      filelist = list.files(path$folder,pattern = ".*.txt")
      gtfsNames = paste0("GTFS",substr(filelist,1,nchar(filelist)-4))
      withProgress(message = 'Loading GTFS', style = style, detail = "part 0", value = 0, {
        for (i in 1:length(filelist)) {

          incProgress(0.1, detail = paste("loading", filelist[i]))
          dat <- readr::read_csv(paste0(path$folder,"/",filelist[i]))
          assign(x = gtfsNames[i], value = dat, envir = as.environment(1))

        }
        output$folderchosen <- renderUI({
          HTML("Loaded all GTFS tables")
        })
      })

    }

  })

  output$filechosen <- renderUI({

    if(is.null(path$siri)){
      HTML("Nothing selected")
    }else{
      HTML(path$siri)
    }
  })

  output$folderchosen <- renderUI({
    if(is.null(path$folder)){
      "Nothing selected"
    }else{
      tryCatch({

        filelist = list.files(path$folder,pattern = ".*.txt")
        n = length(filelist)
        filelist = substr(filelist,1,nchar(filelist)-4)
        str1 = paste(n, "files found")
        filelist = c(str1,filelist)
        for(file in filelist){
          paste(file);
        }
        HTML(paste("",filelist, sep = '<br/>'))
      }, error = function(e) e)


    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
