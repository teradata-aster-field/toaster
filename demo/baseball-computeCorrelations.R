# Demo compute correlations and create correlation matrix
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

# Pitcher Metrics correlation Matrix
cormat = computeCorrelations(channel=conn, "pitching_enh", sqlColumns(conn, "pitching_enh"),
                             include = c('w','l','cg','sho','sv','ipouts','h','er','hr','bb','so','baopp',
                             'era','whip','ktobb','fip'),
                             where = "decadeid = 2000", test=FALSE)

cormat = cormat[cormat$metric1 < cormat$metric2, ]
cormat$value = round(cormat$value, 2)
createBubblechart(cormat, "metric1", "metric2", "value", fill="sign", legendPosition="none")
