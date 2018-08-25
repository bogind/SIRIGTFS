#' @name makexmlsiri
#' @title Create the required XML files for SIRI downloads
#' @description
#' Creates XML files used for sending SIRI requests to Israel's MOT API.
#' @param stop_codes A \code{\link[base]{vector} of stop_code integers taken from the GTFS tables}. the XML will not fail to create if there are no integers.
#' @param route_codes A \code{\link[base]{vector} of route_id integers taken from the GTFS tables}. the XML will not fail to create if there are no integers.
#' @param wd the working directory to write the files in, if newfolder is TRUE a new folder called 'xml' will be created, if there allready is a folder called 'xml' in the working directory a prompt window will ask you to choose a new folder name.
#' @param xml_size Default is 200, the number of stops written to each XML file.
#' @param RequestorRef A unique Requestor key supplied by the Israeli MOT, it is needed since the API requires registration and a static IP address.
#' @param xml_name the prefix to be used for the new XML files. default is 'xml'.
#' @param newfolder Create a new folder for the files?
#' @details
#' Creates XML files used for sending SIRI requests to Israel's MOT API.
#' note that an API key "RequestorRef" is needed for the process.
#' It is recommended to use a single sorted table containing both stop_codes and route_ids.
#' @return
#' Returns a string message, and files written to working directory.
#' @seealso \code{\link{SIRItoGTFS}}
#' @keywords misc utilities
#' @import XML
#' @importFrom easycsv choose_dir
#' @export

makexmlsiri <- function(stop_codes,
                        route_codes,
                        wd = NULL,
                        xml_size = 200,
                        RequestorRef = NULL,
                        xml_name = "xml",
                        newfolder = TRUE){
  # it is reccomended that stop_codes and route_codes be columns in the same table.
  if(is.null(RequestorRef)){
    stop("no RequestorRef supplied")
  }
  if(is.null(wd)){
    wd = easycsv::choose_dir()
  }
  if(newfolder ==  TRUE){
    pathtonewfolder = paste0(wd,"\\xml")
    if(dir.exists(pathtonewfolder) == TRUE){
      pathtonewfolder = easycsv::choose_dir()
    }else{
      dir.create(pathtonewfolder)
    }
    wd = pathtonewfolder
  }
  if(length(stop_codes) != length(route_codes)){
    stop("stop_codes length is different than route_codes length!")
  }

  if(round(length(route_codes)/xml_size) < length(route_codes)/xml_size){
    Nxmls = round(length(route_codes)/xml_size) + 1
  }else{
    Nxmls = round(length(route_codes)/xml_size)
  }

  counter = 0
  frow = 1
  lrow = xml_size
  for(i in 1:Nxmls){
    counter = counter+1

    tstopList = data.frame(stop_code = stop_codes[frow:lrow], route_id = route_codes[frow:lrow])

    z = xmlTree("SOAP-ENV:Envelope", namespaces = list('SOAP-ENV'= "http://schemas.xmlsoap.org/soap/envelope/",
                                                       'SOAP-ENC' = "http://schemas.xmlsoap.org/soap/encoding/",
                                                       acsb="http://www.ifopt.org.uk/acsb",
                                                       datex2="http://datex2.eu/schema/1_0/1_0" ,
                                                       ifopt="http://www.ifopt.org.uk/ifopt" ,
                                                       siri="http://www.siri.org.uk/siri" ,
                                                       siriWS="http://new.webservice.namespace" ,
                                                       xsd="http://www.w3.org/2001/XMLSchema" ,
                                                       xsi="http://www.w3.org/2001/XMLSchema-instance"),
                attrs = c("xsi:schemaLocation"="./siri")
    )
    z$setNamespace("SOAP-ENV")

    z$addNode("SOAP-ENV:Header",close = TRUE)
    z$addNode("SOAP-ENV:Body",close = FALSE)
    z$setNamespace("siriWS")
    z$addNode("siriWS:GetStopMonitoringService",close = FALSE)
    z$setNamespace(ns = FALSE)
    z$addNode("Request",attrs = c("xsi:type"="siri:ServiceRequestStructure"),close = FALSE)
    z$setNamespace("siri")
    z$addNode("RequestTimestamp","2017-03-06T10:50:27+02:00",close = TRUE)
    z$addNode("RequestorRef",RequestorRef,
              attrs = c("xsi:type"="siri:ParticipantRefStructure"),close = TRUE) # Dror's Requestor ref.
    z$addNode("MessageIdentifier","0100700:1351669188:4684",
              attrs = c("xsi:type"="siri:MessageQualifierStructure"),close = TRUE)
    for(j in 1:nrow(tstopList)){
      z$addNode("StopMonitoringRequest",
                attrs = c(version="IL2.71","xsi:type"="siri:StopMonitoringRequestStructure"),close = FALSE)
      z$addNode("RequestTimestamp","2012-10-31T09:39:39.480+02:00")
      z$addNode("MessageIdentifier","0",attrs = c("xsi:type"="siri:MessageQualifierStructure"))
      z$addNode("PreviewInterval","PT3H")
      z$addNode("MonitoringRef",tstopList[j,]$stop_code, attrs = c("xsi:type"="siri:MonitoringRefStructure"))

      z$addNode("LineRef",tstopList[j,]$route_id, attrs = c("xsi:type"="siri:LineRefStructure"))

      z$addNode("MaximumStopVisits","100")
      z$closeNode()
    }
    file_name = paste0(wd,"\\",xml_name,counter,".xml")
    saveXML(z$value(),file=file_name)
    frow = frow+xml_size
    if(lrow+xml_size > length(route_codes)){
      lrow = length(route_codes)
    }else{
      lrow = lrow+xml_size
    }

  }
  print(paste(Nxmls,"files written to: ", wd))
}
