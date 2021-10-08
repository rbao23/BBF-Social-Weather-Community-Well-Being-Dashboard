# script to connect
source("H:/code_base/dbconnect.R")
# password env var
source("H:code_base/pgpass.R")

# connect
ruihab_sb <- connectdb(host = "bffsw.csde.washington.edu", dbname = "ruihab_sb")

# test
dbListTables(conn = ruihab_sb)

