require(toaster)

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