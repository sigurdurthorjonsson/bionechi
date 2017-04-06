#' Read BI500 Report.CompactTable
#'
#' @name read_compact_table
#' @param report Report name, defaults to 'Report.CompactTables'.
#' @keywords manip
#' @examples
#' 
#'   
#' \dontrun{
#'   filename <- "/u2/reikn/Tac/2011/31-capelin/winterCapelin/gildi/2011/A01/Report.CompactTables"
#'   a01 <- read_compact_table(filename)
#' }

#' @rdname read_compact_table
#' @export read_compact_table
 
read_compact_table <- 
  function (report = "report.CompactTables", species = "CAPEL")
{
  require(geo)
  ow <- options("warn")
  ilat <- tempfile("repComp")
  on.exit(unlink(ilat))
  headerLine <- scan(report, what = character(), sep = "|", 
    strip.white = TRUE, skip = 14, nlines = 1, quiet = TRUE)
  colNumber <- grep(species, headerLine)
  system(paste("grep '^[019][0-9]\\.'", report, ">", ilat))
  comptab <- read.table(ilat, sep = "|", strip.white = TRUE, 
    na.strings = c("-", "---.--.--", "----.--.--"), as.is = TRUE)
  comptab <- comptab[, c(1:5, colNumber)]
  names(comptab) <- c("date", "time", "log.unit", "lat", "lon", 
    "sa")
  comptab$timeDate <- as.POSIXct(paste(comptab$date, comptab$time), 
    format = "%y.%m.%d %H:%M:%S")
  delim <- regexpr("/", comptab$log.unit)
  comptab$log <- as.numeric(substring(comptab$log.unit, 1, 
      delim - 1))
  comptab$unit <- as.numeric(substring(comptab$log.unit, delim + 
    1))
  latHemi <- substring(comptab$lat, 1, 1)
  latDeg <- substring(comptab$lat, 2, 3)
  latMin <- substring(comptab$lat, 5, 6)
  latDecMin <- substring(comptab$lat, 8, 9)
  options(warn = -1)
  comptab$lat <- as.numeric(paste(latDeg, latMin, latDecMin, 
    sep = ""))
  options(ow)
  comptab$lat <- ifelse(latHemi == "N", comptab$lat, (-1) * 
    comptab$lat)
  comptab$lat <- geoconvert(comptab$lat)
  lonHemi <- substring(comptab$lon, 1, 1)
  lonDeg <- substring(comptab$lon, 2, 4)
  lonMin <- substring(comptab$lon, 6, 7)
  lonDecMin <- substring(comptab$lon, 9, 10)
  options(warn = -1)
  comptab$lon <- as.numeric(paste(lonDeg, lonMin, lonDecMin, 
    sep = ""))
  options(ow)
  comptab$lon <- ifelse(lonHemi == "W", (-1) * comptab$lon, 
    comptab$lon)
  comptab$lon <- geoconvert(comptab$lon)
  comptab
}

