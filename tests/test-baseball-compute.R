require(toaster)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

data = computeAggregates(channel = conn, tableName = "teams_enh",
               by = c("name || ', ' || park teamname", "lgid", "teamid", "decadeid"),
               aggregates = c("min(name) name", "min(park) park", "avg(rank) rank", 
                              "avg(attendance) attendance"))

data = computeAggregates(channel = conn, "pitching_enh", 
               aggregates = c("sum(so) so")
               by=c("lgid", "teamid", "decadeid"),
               percent=c("lgid", "decadeid"))

data = computeAggregates(channel = conn, "pitching_enh",
               by = c("teamid", "decadeid"), 
               aggregates = c("sum(so) so", 
                              "sum(so)/(sum(sum(so)) over (partition by decadeid)) percent"),
               where = "decadeid >= 1980")