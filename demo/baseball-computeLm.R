# Demo compute linear regression model
#
# To install baseball demo dataset in Aster
# download baseball.zip from
# https://bitbucket.org/grigory/toaster/downloads/baseball.zip
# and run
# sh load_baseball_data.sh -d mydbname -U beehive 

library(toaster)

readlineDef <- function(prompt, default) {
  if (!is.null(prompt))
    prompt = paste0(prompt, "[", default, "]: ")
  else 
    prompt = paste0(prompt, ": ")
  
  result = readline(prompt)
  if (result == "") 
    return (default)
  else
    return (result)
}

connectToAster <- function(uid="beehive", pwd=NULL, server="localhost", port="2406", database="beehive") {
  uid = readlineDef("Enter user id: ", uid)
  pwd = readlineDef("Enter password: ", pwd)
  server = readlineDef("Enter server: ", server)
  port = readlineDef("Enter port: ", port)
  database = readlineDef("Enter database: ", database)
  
  tryCatch(close(conn), error=function(err) {NULL})
  
  connectStr = paste0("driver={Aster ODBC Driver};server=", server, ";port=", port, 
                      ";database=",database,";uid=",uid,";pwd=", pwd)
  conn = tryCatch({
    
    conn = odbcDriverConnect(connection=connectStr)
    odbcGetInfo(conn)
    return (conn)
  }, error=function(err) {
    stop("Can't connect to Aster - check ip/port/database/uid/pwd", err)
  })
}

pause <- function() {
  cat("Press ENTER/RETURN/NEWLINE to continue.")
  readLines(n=1)
  invisible()
}

### connect first
conn = connectToAster(pwd="beehive", server="141.206.75.100", database="partners2013")

pause()

### simple model with 3 numerical predictors
model1 = computeLm(channel=conn, tableName="batting_enh", formula= ba ~ rbi + bb + so)
summary(model1)

pause()

### added ER predictor to the model and defined subset with where
modelNL = computeLm(channel=conn, tableName="pitching_enh", formula= era ~ er + hr + bb + so, 
                    where = "yearid >= 2000 and lgid = 'NL'")
summary(modelNL)

pause()

### added categorical predictor
modelLg = computeLm(channel=conn, tableName="batting_enh", formula=ba ~ rbi + bb + so + lgid, 
                    where="lgid in ('AL','NL')")
summary(modelLg)

pause()

### category with more than 2 values, also lowered sample size to 500 rows 
### (coefficients are always computed on all data)
modelTeam10K = computeLm(channel=conn, tableName="batting_enh", formula=ba ~ rbi + bb + so + teamid,
                    sampleSize = 50, where="teamid in ('TEX','NYY','OAK','PIT','DET') and yearid >= 1990")
summary(modelLg10K)
