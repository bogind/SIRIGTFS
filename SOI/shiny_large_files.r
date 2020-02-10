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
library(SIRItoGTFS)
library(ggplot2)
library(plotly)

lineNames = c()

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

ui <-
  #navbarPage("",

  fluidPage(
    theme = shinytheme("paper"),
    tabsetPanel(id="tabs",
                tabPanel("Setup",
  # shinythemes::themeSelector(),
  tags$head(tags$style(".rightAlign{float:right;}"),
            tags$title("SIRI to GTFS")),

  busyIndicator(),
  # Application title
  titlePanel(title=div(a(
                      img(src="logo_GAMES_lab.png", align = "right"),
                          href="http://raphael.geography.ad.bgu.ac.il/GAMESLab/"),
                       "SIRI to GTFS Analysis UI")),


  hr(),
  sidebarLayout(
    sidebarPanel(
      span(
        shiny::actionButton("filechoose",
                          icon=icon("file-upload"),
                          label = "Pick a file"
                          ),

      shiny::actionButton("run",label = "Load")
      ),
      width = 6
    ),

    mainPanel(
      htmlOutput("filechosen"),
      width = 6
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(

      span(shiny::actionButton(inputId = "folderchoose",
                   label = "Pick a folder", icon = icon("folder-open")),


      shiny::actionButton("run2",label = "Load All")),

      width = 6
    ),

    mainPanel(
      htmlOutput("folderchosen"),
      width = 6
    )
  ),

  hr(),
  sidebarLayout(
    sidebarPanel(
      htmlOutput("selectOperator"),
      htmlOutput("selectLines"),
      htmlOutput("NselectedLines"),
      htmlOutput("mapButton"),
      htmlOutput("dateSelect"),
      br(),
      htmlOutput("startButton"),
      width = 6


    ),

    mainPanel(
      htmlOutput("undecided"),
      leafletOutput("map1", width = 400, height = 400),
      width = 6
    )
  ),
  #hr(),
  fluidRow(
    column(4,
           htmlOutput("attribution")
           )
      )

      )
    )
  )
#)


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
    linerefs = NULL,
    plotLinerefs = NULL
  )
  firstRun <- reactiveValues(
    isIt = TRUE
  )
  data <- reactiveValues(
    buses = NULL
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

        output$selectLines <- renderUI({

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

          }
          operatorSel <- reactive({is.null(input$inOperators)})
          if(operatorSel()){
            selectizeInput('inLinerefs', 'בחירת קווים',
                           unique(routes2$name),
                           size = 10,options = list(
                             placeholder = 'Please select an option below'
                           ),
                           multiple=TRUE)
          }


        })

        # Add the Start Analysis button
        output$mapButton <- renderUI({

          shiny::actionButton("mapRoutes",label = "Show Routes On Map", icon = icon("map"))

        })

        output$startButton <- renderUI({


          shiny::actionButton("run3",label = "Run", icon = icon("play"))

        })

        # output$dateSelect <- renderUI({
        #   dateInput('date',
        #             label = 'Date input: yyyy-mm-dd',
        #             value = Sys.Date()
        #   )
        # })

      }else{
        return()
      }


    })

    ################
    # filter lines list by operator
    ################

    observeEvent(input$inOperators,{
      if(length(input$inOperators) > 0){
        ids = unique(GTFSagency$agency_id[GTFSagency$agency_name %in% input$inOperators])

        selection$linerefs = unique(SIRIdf$LineRef[SIRIdf$LineRef %in%
                                                     routes2$route_id[routes2$agency_id %in% ids]])

      }else if(is.null(input$inOperators) | length(input$inOperators) <= 0){
        selection$linerefs = unique(routes2$name)

      }


      choices = unique(routes2$name[routes2$route_id %in% selection$linerefs])
      output$selectLines <- renderUI({

          selectizeInput('inLinerefs', 'בחירת קווים',
                         choices,
                         size = 10,options = list(
                           placeholder = 'Please select an option below'
                         ),
                         multiple=TRUE)
      })

    })


    ################
    # select lines
    ################

    observeEvent(input$inLinerefs,{


      selection$linerefs = unique(SIRIdf$LineRef[SIRIdf$LineRef %in%
                                         routes2$route_id[routes2$name %in% input$inLinerefs]])

    })


    ################
    # Show Routes on Map
    ################

    observeEvent(input$mapRoutes,{


      assign(x = "linerefs", value = selection$linerefs, envir = as.environment(1))

      routes = GTFSroutes[GTFSroutes$route_id %in% linerefs,]

      trips = GTFStrips[GTFStrips$route_id %in% routes$route_id,]

      shapes = GTFSshapes[GTFSshapes$shape_id %in% trips$shape_id,]

      shapes =shapes[order(shapes$shape_pt_sequence),]

      shapes_sf = st_as_sf(shapes, coords = c("shape_pt_lon","shape_pt_lat"), crs = 4326)
      bbox <- st_bbox(shapes_sf) %>%
        as.vector()
      t = data.frame("shape_id" = c(),"route_id"=c())
      for(s in 1:length(unique(shapes_sf$shape_id))){
        shp = unique(shapes_sf$shape_id)[s]
        route = unique(trips$route_id[trips$shape_id == unique(shapes_sf$shape_id)[s]])[1]
        t[s,"shape_id"] = shp
        t[s,"route_id"] = route
      }
      shapes_lines = shapes_sf %>%
        group_by(shape_id) %>%
        left_join(t) %>%
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
    # Start
    ################

    observeEvent(input$run3,{
      assign(x = "linerefs", value = selection$linerefs, envir = as.environment(1))

      if(firstRun$isIt){
        firstRun$isIt = FALSE

        data$buses = STG(SIRIdf,
                    GTFSstops,
                    GTFSagenc.,
                    GTFScalendar,
                    GTFSroute.,
                    GTFSstop_times,
                    GTFStrips,
                    linerefs = linerefs,
                    epsg = 2039)

        data$buses$weekday <- as.factor(strftime(data$buses$RecordedAtTime, format = "%A"))
        data$buses$weekday <- factor(data$buses$weekday, levels = levels(data$buses$weekday)[c(4,2,6:7,5,1,3)])
        data$buses$lineref <-  as.numeric(data$buses$lineref)

        data$buses = data$buses %>%
                left_join(GTFSroutes, by =c("lineref"="route_id")) %>%
                left_join(GTFSagency, by = "agency_id")

        data$buses$name = paste(data$buses$agency_name, data$buses$route_short_name)

        appendTab(inputId = "tabs",
                  tabPanel("Summary",
                           textOutput("summary"),
                           sidebarLayout(
                             sidebarPanel(
                               htmlOutput("selectPlotAgency"),
                               htmlOutput("selectPlotLine"),
                               width = 6
                             ),
                             mainPanel(plotOutput("plot1", height = 300),
                                       plotOutput("plot2", height = 300),
                                       width = 6)
                           )
                       )
                  )

        appendTab(inputId = "tabs",
          tabPanel("Table", DT::dataTableOutput("table"))
        )


        output$table <- DT::renderDataTable({
          DT::datatable(data$buses)
        })

        output$selectPlotAgency <- renderUI({
          selectizeInput('inPlotAgency', 'בחירת מפעיל',
                         unique(data$buses$agency_name),
                         size = 10,
                         multiple=TRUE)
        })
        output$selectPlotLine <- renderUI({

          if(length(input$inPlotAgency) > 0){

            ids = unique(GTFSagency$agency_id[GTFSagency$agency_name %in% input$inPlotAgency])
            selectizeInput('inPlotLinerefs', 'בחירת קווים',
                           unique(routes2$name[data$buses$agency_id %in% ids]),
                           size = 10,options = list(
                             placeholder = 'Please select an option below'
                           ),
                           multiple=TRUE)

          }else{
            selectizeInput('inPlotLinerefs', 'בחירת קווים',
                           unique(data$buses$name),
                           size = 10,options = list(
                             placeholder = 'Please select an option below'
                           ),
                           multiple=TRUE)
          }


        })



      }



    })


    ################
    # Plots reactive functions
    ################

    observeEvent(input$inPlotAgency,{

      ids = unique(GTFSagency$agency_id[GTFSagency$agency_name %in% input$inPlotAgency])



      selection$plotLinerefs = unique(data$buses$route_id[data$buses$agency_id %in% ids])


      output$selectPlotLine <- renderUI({

        selectizeInput('inLinerefs', 'בחירת קווים',
                       unique(data$buses$name[buses$agency_id %in% ids]),
                       size = 10,options = list(
                         placeholder = 'Please select an option below'
                       ),
                       multiple=TRUE)#, selectize=FALSE)
      })

      output$selectPlotLine <- renderUI({

        if(length(input$inPlotAgency) > 0){

          selectizeInput('inPlotLinerefs', 'בחירת קווים',
                         unique(data$buses$name[data$buses$agency_id %in% ids]),
                         size = 10,options = list(
                           placeholder = 'Please select an option below'
                         ),
                         multiple=TRUE)

        }else{
          selectizeInput('inPlotLinerefs', 'בחירת קווים',
                         unique(data$buses$name),
                         size = 10,options = list(
                           placeholder = 'Please select an option below'
                         ),
                         multiple=TRUE)
        }


      })


      # selection$plotLinerefs = unique(buses$lineref[buses$agency_id %in% ids])

      t <- data$buses[data$buses$agency_id %in% ids,]
      t <- t[order(t$RecordedAtTime,t$OriginAimedDepartureTime),]
      t$hour <- as.numeric(strftime(t$arrival_time, format = "%H"))
      t1 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(mean(timediff, na.rm=TRUE)))
      t1 <- t1[t1$timediff <200,]

      t2 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(max(timediff, na.rm=TRUE)))

      t3 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(min(timediff, na.rm=TRUE)))

      t4 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(median(timediff, na.rm=TRUE)))

      cc <- colnames(t4)
      cc[4] <- "Median"
      colnames(t4) <- cc
      tmed <- t4$Median
      t1 <- cbind(t1,tmed)

      t5 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(IQR(timediff, na.rm=TRUE)))

      t6 <- t %>%
        group_by(lineref) %>%
        summarise_all(funs(mean(timediff, na.rm=TRUE)))

      st1 <- t %>%
        group_by(stop_code) %>%
        summarise_all(funs(median(timediff, na.rm=TRUE)))
      st2 <- t %>%
        group_by(stop_code) %>%
        summarise_all(funs(max(timediff, na.rm=TRUE)))



      st1 <- st1[,c(1,4)]
      st1 <- left_join(st1, GTFSstops, by = c("stop_code" = "stop_code"))
      st2 <- st2[,c(1,4)]
      st2 <- left_join(st2, GTFSstops, by = c("stop_code" = "stop_code"))
      stpos <- st1[st1$timediff > 0,]
      stneg <- st1[st1$timediff < 0,]

      matTA1 = st1[,c(1:5,8:10)]
      matTA1 = matTA1 %>%
        left_join(GTFSstops)

      matTA2 = st2[,c(1:5,8:10)]
      matTA2 = matTA2 %>%
        left_join(GTFSstops)

      p1 <- ggplot(data$buses[data$buses$timediff < 200,], aes(x = timediff, color = weekday, fill = weekday)) +
        geom_density(alpha = 0.2) +
        labs(title = paste("Density plot of",nrow(data$buses), "observations \n"),
             x = "Time Variation in minutes",
             y = "Density")+
        theme(plot.title = element_text(hjust = 0.5,size=14),
              panel.border = element_rect(linetype = "dashed", fill = NA),
              plot.background = element_rect(fill = "azure1"),
              legend.position="none"
        )


      p3 <- ggplot(data = t1, aes(x=hour)) +
        geom_ribbon(aes(ymin=timediff-2*sd(timediff), ymax=timediff+2*sd(timediff),fill = "orange"),alpha=0.15) +
        geom_ribbon(aes(ymin=timediff-1*sd(timediff), ymax=timediff+1*sd(timediff),fill = "cyan"),alpha=0.2) +
        geom_ribbon(aes(ymin=tmed-0.5*IQR(tmed), ymax=tmed+0.5*IQR(tmed),fill = "grey70"),alpha=0.5) +
        scale_x_continuous(breaks=seq(1,24,1)) +
        geom_line(aes(y=timediff,colour = "timediff")) +
        geom_line(aes(y = tmed,colour = "tmed"))+
        scale_colour_manual("",breaks = c("timediff", "tmed"),values = c("timediff"="Red", "tmed"="green"), labels = c("Mean", "Median"))+
        scale_fill_manual("",values = hcl(c(15,195,100),100,65, alpha=c(0.5,0.2,0.15)),
                          labels = c("SD","IQR","2SD"))+
        labs(title = paste("Time Variation \n", nrow(data$buses), "observations\n"),
             x = "Hour",
             y = "Time difference")+
        theme(plot.title = element_text(hjust = 0.5, size = 14),
              panel.border = element_rect(linetype = "dashed", fill = NA),
              plot.background = element_rect(fill = "azure1"),
              legend.box.background = element_rect(),
              legend.box.margin = margin(5, 5, 5, 5))

      output$plot1 <-  renderPlot({
        p1
      })
      output$plot2 <-  renderPlot({
        p3
      })



    })

    observeEvent(input$inPlotLinerefs,{

      selection$plotLinerefs = unique(data$buses$lineref[data$buses$name %in% input$inPlotLinerefs])

      t <- data$buses[data$buses$lineref %in% selection$plotLinerefs,]
      t <- t[order(t$RecordedAtTime,t$OriginAimedDepartureTime),]
      t$hour <- as.numeric(strftime(t$arrival_time, format = "%H"))
      t1 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(mean(timediff, na.rm=TRUE)))
      t1 <- t1[t1$timediff <200,]

      t2 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(max(timediff, na.rm=TRUE)))

      t3 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(min(timediff, na.rm=TRUE)))

      t4 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(median(timediff, na.rm=TRUE)))

      cc <- colnames(t4)
      cc[4] <- "Median"
      colnames(t4) <- cc
      tmed <- t4$Median
      t1 <- cbind(t1,tmed)

      t5 <- t %>%
        group_by(hour) %>%
        summarise_all(funs(IQR(timediff, na.rm=TRUE)))

      t6 <- t %>%
        group_by(lineref) %>%
        summarise_all(funs(mean(timediff, na.rm=TRUE)))

      st1 <- t %>%
        group_by(stop_code) %>%
        summarise_all(funs(median(timediff, na.rm=TRUE)))
      st2 <- t %>%
        group_by(stop_code) %>%
        summarise_all(funs(max(timediff, na.rm=TRUE)))



      st1 <- st1[,c(1,4)]
      st1 <- left_join(st1, GTFSstops, by = c("stop_code" = "stop_code"))
      st2 <- st2[,c(1,4)]
      st2 <- left_join(st2, GTFSstops, by = c("stop_code" = "stop_code"))
      stpos <- st1[st1$timediff > 0,]
      stneg <- st1[st1$timediff < 0,]

      matTA1 = st1[,c(1:5,8:10)]
      matTA1 = matTA1 %>%
        left_join(GTFSstops)

      matTA2 = st2[,c(1:5,8:10)]
      matTA2 = matTA2 %>%
        left_join(GTFSstops)

      p1 <- ggplot(t[t$timediff < 200,], aes(x = timediff, color = weekday, fill = weekday)) +
        geom_density(alpha = 0.2) +
        labs(title = paste("Density plot of",nrow(t), "observations \n"),
             x = "Time Variation in minutes",
             y = "Density")+
        theme(plot.title = element_text(hjust = 0.5,size=14),
              panel.border = element_rect(linetype = "dashed", fill = NA),
              plot.background = element_rect(fill = "azure1"),
              legend.position="none"
        )


      p3 <- ggplot(data = t1, aes(x=hour)) +
        geom_ribbon(aes(ymin=timediff-2*sd(timediff), ymax=timediff+2*sd(timediff),fill = "orange"),alpha=0.15) +
        geom_ribbon(aes(ymin=timediff-1*sd(timediff), ymax=timediff+1*sd(timediff),fill = "cyan"),alpha=0.2) +
        geom_ribbon(aes(ymin=tmed-0.5*IQR(tmed), ymax=tmed+0.5*IQR(tmed),fill = "grey70"),alpha=0.5) +
        scale_x_continuous(breaks=seq(1,24,1)) +
        geom_line(aes(y=timediff,colour = "timediff")) +
        geom_line(aes(y = tmed,colour = "tmed"))+
        scale_colour_manual("",breaks = c("timediff", "tmed"),values = c("timediff"="Red", "tmed"="green"), labels = c("Mean", "Median"))+
        scale_fill_manual("",values = hcl(c(15,195,100),100,65, alpha=c(0.5,0.2,0.15)),
                          labels = c("SD","IQR","2SD"))+
        labs(title = paste("Time Variation \n", nrow(t1), "observations\n"),
             x = "Hour",
             y = "Time difference")+
        theme(plot.title = element_text(hjust = 0.5, size = 14),
              panel.border = element_rect(linetype = "dashed", fill = NA),
              plot.background = element_rect(fill = "azure1"),
              legend.box.background = element_rect(),
              legend.box.margin = margin(5, 5, 5, 5))

      output$plot1 <-  renderPlot({
        p1
      })
      output$plot2 <-  renderPlot({
        p3
      })

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
