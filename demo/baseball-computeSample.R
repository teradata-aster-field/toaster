# Demo compute sample
#
# To install baseball demo dataset in Aster
# download baseball.zip from
# https://bitbucket.org/grigory/toaster/downloads/baseball.zip
# and run
# sh load_baseball_data.sh -d mydbname -U beehive 

library(toaster)

# update ODBC data source name
dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

# Sample 1% of batting rows
batters = computeSample(conn, "batting", sampleFraction=0.01)
dim(batters)

# Sample 1K of pitching rows
pitchers = computeSample(conn, "pitching", sampleSize=1000,
                  where="lgid = 'AL'")
dim(pitchers)