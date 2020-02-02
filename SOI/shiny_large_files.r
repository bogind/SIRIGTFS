library(shiny)
library(shinysky)
library(shinythemes)
library(purrr)
library(dplyr)
library(leaflet)
library(htmltools)
library(easycsv)
library(sf)
library(readr)

lineNames = c()

ui <- fluidPage(
  # shinythemes::themeSelector(),
  tags$head(tags$style(".rightAlign{float:right;}"),
            tags$title("SIRI to GTFS")),
  theme = shinytheme("paper"),
  busyIndicator(),
  # Application title
  titlePanel(title=div(img(src="logo_GAMES_lab.png", align = "right"),
                       "SIRI to GTFS Analysis UI")),


  hr(),
  sidebarLayout(
    sidebarPanel(
      shiny::actionButton("filechoose",
                          icon=icon("file-upload"),
                          label = "Pick a file"
                          ),
      # htmlOutput("load")
      shiny::actionButton("run",label = "Load")

    ),

    mainPanel(
      htmlOutput("filechosen")
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      # actionButton("folderchoose", icon = icon("fa-folder-open",lib="font-awesome"), label = "Pick a folder"),
      # submitButton("folderchoose", icon = icon("fa-folder-open")),
      shiny::actionButton(inputId = "folderchoose",
                   label = "Pick a folder", icon = icon("folder-open")),
      # htmlOutput("loadAll")
      shiny::actionButton("run2",label = "Load All"),

    ),

    mainPanel(
      htmlOutput("folderchosen")
    )
  ),
  hr(),
  fluidRow(
    column(4,

           hr(),
           # htmlOutput("selectOperator"),
           # htmlOutput("selectUI"),
           # htmlOutput("NselectedLines")
           # selectInput('inLinerefs', 'Options', lineNames, multiple=TRUE, selectize=FALSE),

    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      htmlOutput("selectOperator"),
      htmlOutput("selectUI"),
      htmlOutput("NselectedLines"),
      htmlOutput("startButton")


    ),

    mainPanel(
      htmlOutput("undecided"),
      leafletOutput("map1", width = 400, height = 400)
    )
  ),
  hr(),
  fluidRow(
    column(4,
           htmlOutput("attribution")
           )
    )

)


server <- function(input, output) {


  ################
  # Stored values for later use
  ################

  path <- reactiveValues(
    siri=NULL,
    folder=NULL
  )
  loaded <- reactiveValues(
    siri=NULL,
    GTFS=NULL
  )
  selection <- reactiveValues(
    linerefs = NULL
  )


  ################
  # Select SIRI File
  ################

  observeEvent(input$filechoose,{
    path$siri <- choose.files(filters = Filters[c("txt","All"),])
    output$filechosen <- renderUI({

      if(is.null(path$siri)){
        HTML("Nothing selected")
      }else{
        HTML(path$siri)
      }
    })

  })

  ################
  # Load SIRI File
  ################

  observeEvent(input$run,{
    if(is.null(path$siri)){
      output$filechosen <- renderUI({


          HTML("You need to select a file first")

      })
    }else{

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
        # loaded$siri = 1


      }, error = function(e) HTML(e))
    }

  })

  ################
  # Select GTFS Folder
  ################

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

  ################
  # Load GTFS Files
  ################

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
          # loaded$GTFS = 1
        })
      })

    }

  })

  ################
  # populate lines list
  ################


    loadedTables <- reactive({
      list(input$run,input$run2)
    })

    observeEvent(loadedTables(), {

      if(input$run==1 && input$run2==1 && exists("SIRIdf")){


        routes = GTFSroutes[GTFSroutes$route_id %in% unique(SIRIdf$LineRef),]
        routes2 = routes %>%
          left_join(GTFSagency)#%>%
          # group_by(agency_name, route_short_name) %>%
          # summarise(agency_id = min(agency_id),name = min(paste(agency_name, route_short_name)))
        routes2$name = paste(routes2$agency_name, routes2$route_short_name)
        routes2 = routes2[order(routes2$name),]

        assign(x = "routes2", value = routes2, envir = as.environment(1))

        selection$linerefs = unique(routes2$route_id)

        output$selectOperator <- renderUI({

          # selectInput('inOperators', 'סינון לפי מפעיל',
          #             unique(routes2$agency_name),
          #             size = 10,
          #             multiple=TRUE, selectize=FALSE)

          selectizeInput('inOperators', 'סינון לפי מפעיל',
                      unique(routes2$agency_name),
                      size = 10,
                      multiple=TRUE)
        })

        output$selectUI <- renderUI({

          # selectInput('inLinerefs', 'בחירת קווים',
          #             unique(routes2$name),
          #             size = 10,
          #             multiple=TRUE, selectize=FALSE)
          if(length(input$inOperators) > 0){
            ids = unique(GTFSagency$agency_id[GTFSagency$agency_name %in% input$inOperators])
            selectizeInput('inLinerefs', 'בחירת קווים',
                           unique(routes2$name[routes2$agency_id %in% ids]),
                           size = 10,options = list(
                             placeholder = 'Please select an option below'
                           ),
                           multiple=TRUE)

          }else{
            selectizeInput('inLinerefs', 'בחירת קווים',
                           unique(routes2$name),
                           size = 10,options = list(
                             placeholder = 'Please select an option below'
                           ),
                           multiple=TRUE)
          }


        })

        # Add the Start Analysis button

        output$startButton <- renderUI({

          shiny::actionButton("run3",label = "Run", icon = icon("play"))

        })
      }else{
        return()
      }


    })

    ################
    # filter lines list by operator
    ################

    observeEvent(input$inOperators,{
      ids = unique(GTFSagency$agency_id[GTFSagency$agency_name %in% input$inOperators])

      selection$linerefs = unique(routes2$route_id[routes2$agency_id %in% ids])
      print(length(selection$linerefs))

        output$selectUI <- renderUI({

          selectizeInput('inLinerefs', 'בחירת קווים',
                      unique(routes2$name[routes2$agency_id %in% ids]),
                      size = 10,options = list(
                        placeholder = 'Please select an option below'
                      ),
                      multiple=TRUE)#, selectize=FALSE)
        })

      # print(unique(GTFSagency$agency_id[GTFSagency$agency_name %in% input$inOperators]))
    })


    ################
    # select lines
    ################

    observeEvent(input$inLinerefs,{

      selection$linerefs = unique(SIRIdf$LineRef[SIRIdf$LineRef %in%
                                         routes2$route_id[routes2$name %in% input$inLinerefs]])



      print(length(selection$linerefs))

      # output$NselectedLines <- renderUI({
      #
      #   if(length(selection$linerefs) <= 0 ){
      #     HTML("Nothing selected")
      #   }else{
      #     str1 = paste(length(selection$linerefs), "routes have been selected")
      #     HTML(str1)
      #   }
      # })
    })



    ################
    # Start
    ################

    observeEvent(input$run3,{
      print("starting")

      assign(x = "linerefs", value = selection$linerefs, envir = as.environment(1))

      routes = GTFSroutes[GTFSroutes$route_id %in% linerefs,]
      print("routes")
      trips = GTFStrips[GTFStrips$route_id %in% routes$route_id,]
      print("trips")
      shapes = GTFSshapes[GTFSshapes$shape_id %in% trips$shape_id,]
      print("shapes")
      shapes =shapes[order(shapes$shape_pt_sequence),]
      print("shapes ordered")
      shapes_sf =   st_as_sf(shapes, coords = c("shape_pt_lon","shape_pt_lat"), crs = 4326)
      bbox <- st_bbox(shapes_sf) %>%
        as.vector()
      shapes_lines = shapes_sf %>%
        group_by(shape_id) %>%
        left_join(trips) %>%
        left_join(routes) %>%
        left_join(GTFSagency) %>%
        summarize(n = n(),
                  agency_name = min(agency_name),
                  route_name = min(route_short_name),
                  route_desc = min(route_long_name),
                  do_union=FALSE) %>%
        st_cast("LINESTRING")

      shapes_lines$popup_content = paste("<div dir='rtl' style='direction: rtl; text-align:right'><b>",
                                         shapes_lines$agency_name,"</b><br>",
                                         "קו",shapes_lines$route_name,"<br>",
                                         shapes_lines$route_desc,"</div>")

      map1 = leaflet(data = shapes_lines) %>%
        addTiles() %>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
        addPolylines(weight = 3, popup = ~popup_content)#~htmlEscape(popup))

      output$map1 <- renderLeaflet(map1)

    })

    ################
    # Placeholder for  SIRI File
    ################

    output$undecided <- renderUI({
      if(is.null(selection$linerefs)){
        HTML("")
      }else if(length(selection$linerefs) == length(unique(SIRIdf$LineRef)) ){
        str1 = paste0("Nothing selected, All routes will be used, ",length(selection$linerefs)," routes")
        HTML(str1)
      }else{
        str1 = paste(length(selection$linerefs), "routes have been selected")
        HTML(str1)
      }
    })



  ################
  # Placeholder for  SIRI File
  ################

  output$filechosen <- renderUI({

    if(is.null(path$siri)){
      HTML("Nothing selected")
    }else{
      HTML(path$siri)
    }
  })

  ################
  # Listen for folder load event
  ################

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

  output$attribution <- renderUI({
    HTML("<span style='font-size: xx-small;'>נבנה ע\"י <a href='mailto:dror@kaplanopensource.co.il'>דרור בוגין</a></span>")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
