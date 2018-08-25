#' @name getlaststops
#' @title Create a stops table for makexmlsiri
#' @description
#' Creates a
#' @param GTFSroutes. A GTFS routes table, best load into environment with \code{\link{readGTFS}}
#' @param GTFStrips. A GTFS trips table, best load into environment with \code{\link{readGTFS}}
#' @param GTFSstop_times. A GTFS stop_times table, best load into environment with \code{\link{readGTFS}}
#' @param GTFSstops. A GTFS stops table, best load into environment with \code{\link{readGTFS}}
#' @details
#' Uses SQL within R to create a table containing each unique route_id in the GTFS provided,
#' for each route, it's shape_id and the stop_code and stop_id of the last stop in that shape.
#' usefull for the creation of XML files using \code{\link[SIRItoGTFS]{makexmlsiri}
#' @return
#' A \code{\link[base]{data.frame}} containing for each unique route_id in the GTFS tables it's
#' shape_id and the stop_id and stop_code of the last stop in that shape
#' @seealso \code{\link{SIRItoGTFS}}, \code{\link[SIRItoGTFS]{makexmlsiri}
#' @keywords misc utilities
#' @importFrom sqldf sqldf
#' @importFrom easycsv choose_dir
#' @export

getlaststops <- function(GTFSroutes.,
                         GTFStrips.,
                         GTFSstop_times.,
                         GTFSstops.  ){
  query1 = paste("select a.route_id, b.trip_id, b.shape_id from",
                 deparse(substitute(GTFSroutes.)),
                 "a left join",
                 deparse(substitute(GTFStrips.)),
                 "b on a.route_id=b.route_id group by a.route_id")
  route_trips = sqldf(query1)
  query2 = paste("select rt.route_id,rt.shape_id, f.stop_id last_stop_id, st.stop_code
                from route_trips rt left join
                      (SELECT a.trip_id, a.stop_sequence, a.stop_id
                        FROM",deparse(substitute(GTFSstop_times.)), "a
                 INNER JOIN (
                   SELECT trip_id, MAX(stop_sequence) stop_sequence
                   FROM ",deparse(substitute(GTFSstop_times.)), "
                   GROUP BY trip_id
                 ) b ON a.trip_id = b.trip_id AND
                 a.stop_sequence = b.stop_sequence) as f on rt.trip_id=f.trip_id left join
                 ",deparse(substitute(GTFSstops.)),"st on f.stop_id=st.stop_id
                group by rt.route_id, f.stop_id, st.stop_code"
  )
  route_last_stop = sqldf(query2)
  return(route_last_stop)
}
