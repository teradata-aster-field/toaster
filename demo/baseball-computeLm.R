# Demo compute linear regression model
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

model1 = computeLm(channel=conn, tableName="batting_enh", expr= ba ~ rbi + bb + so)

modelNL = computeLm(channel=conn, tableName="pitching_enh", expr= era ~ er + hr + bb + so, 
                    where = "yearid >= 2000 and lgid = 'NL'")

modelAL = computeLm(channel=conn, tableName="pitching_enh", expr= era ~ er + hr + bb + so, 
                    where = "yearid >= 2000 and lgid = 'AL'")
