# a function to connect to a PostgreSQL database

library(RPostgreSQL)

# connect to a db
# e.g.,     phurvitz <- connectdb("phurvitz", "doyenne.csde.wahington.edu", 5432)
connectdb <- connectDB <- function(dbname, host="localhost", port=5432){
  if(!exists(dbname)){
    conn <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, host=host, port=port)
    message("connecting to ", dbname)
    return(conn)
  }
  if(exists(dbname)){
    cmd <- sprintf("isPostgresqlIdCurrent(%s)", dbname)
    connectionIsCurrent <- eval(parse(text=cmd))
    if(!connectionIsCurrent){
      message("establishing connection to ", dbname)
      conn <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, host=host, port=port)
    } else {
      cmd <- sprintf("conn <- %s", dbname)
      eval(parse(text=cmd))
    }
    return(conn)
  }
}

disconnectall <- function(verbose = FALSE){
  O <- lapply(dbListConnections(drv = PostgreSQL()), dbDisconnect)
  if(verbose){
    message(O)
  }
}

# for sqldf
sqldf.pg.options <- function(dbname="test", host="localhost", port=5432, user=Sys.getenv("USER") ){
  options(
    sqldf.RPostgreSQL.user = user,
    sqldf.RPostgreSQL.password = gsub(".*:", "", scan("~/.pgpass", what = "character", quiet = TRUE))[1],
    sqldf.RPostgreSQL.dbname = dbname,
    sqldf.RPostgreSQL.host = host,
    sqldf.RPostgreSQL.port = port)
}

# a function for checking if a db table exists
tExists <- dbTableExists <- function(conn, table_schema, table_name){
  sql <- sprintf("select count(*) = 1 from information_schema.tables where table_schema = '%s' and table_name = '%s';", 
                 table_schema, 
                 table_name)
  dbGetQuery(conn = conn, sql)[1,1]
}