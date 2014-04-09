# Demo compute aggregates
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

# compute team aggregates - average rank and attendance - by decades
data = computeAggregates(channel = conn, tableName = "teams_enh",
               by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
               aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", 
                              "avg(attendance) attendance"))

# compute total strikouts per team and decade
data = computeAggregates(channel = conn, "pitching_enh", 
               aggregates = c("sum(so) so"),
               by=c("lgid", "teamid", "decadeid"), # percent=c("lgid", "decadeid") 
       )

# compute total strikouts and percent using window function
data = computeAggregates(channel = conn, "pitching_enh",
               by = c("teamid", "decadeid"), 
               aggregates = c("sum(so) so", 
                              "sum(so)/(sum(sum(so)) over (partition by decadeid)) percent"),
               where = "decadeid >= 1980")