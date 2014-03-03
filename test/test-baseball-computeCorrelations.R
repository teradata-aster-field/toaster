require(toaster)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

cormat = computeCorrelations(channel=conn, "pitching_enh", sqlColumns(conn, "pitching_enh"),
                             include = c('w','l','cg','sho','sv','ipouts','h','er','hr','bb','so','baopp',
                             'era','whip','ktobb','fip'),
                             where = "decadeid = 2000", test=FALSE)

cormat = cormat[cormat$metric1 < cormat$metric2, ]

corrs$value = round(corrs$value, 2)
createBubblechart(corrs, "metric1", "metric2", "value", fill="sign")
