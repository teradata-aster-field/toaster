require(toaster)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

model1 = computeLm(channel=conn, tableName="batting_enh", expr= ba ~ rbi + bb + so)
