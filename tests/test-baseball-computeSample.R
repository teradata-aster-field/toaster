require(toaster)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

batters = computeSample(conn, "batting", sampleFraction=0.01)
dim(batters)

pitchers = computeSample(conn, "pitching", sampleSize=1000,
                  where="lgid = 'AL'")
dim(ptichers)