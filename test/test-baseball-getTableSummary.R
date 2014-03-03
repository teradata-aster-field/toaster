require(toaster)

dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(conn)
conn = odbcConnect(dsn, uid, pwd)

pitchingInfo = getTableSummary(channel=conn, 'pitching_enh')
# list all table columns
pitchingInfo$COLUMN_NAME

# compute statistics on subset of baseball data after 1999
battingInfo = getTableSummary(channel=conn, 'batting_enh', 
                              where='yearid between 2000 and 2013')
 
# compute statistics for certain columns including each percentile from 1 to 99
pitchingInfo = getTableSummary(channel=conn, 'pitching_enh',
                             include=c('h', 'er', 'hr', 'bb', 'so'),
                             percentiles=seq(1,99))
names(pitchingInfo)
                              
# compute statistics except certain columns
pitchingInfo = getTableSummary(channel=conn, 'pitching_enh',
                             except=c('lgid', 'teamid', 'playerid', 'yearid', 'decadeid'))                            

# compute statitics on all numeric columns except certain columns
teamInfo = getTableSummary(channel=conn, 'teams_enh', 
                           include=getNumericColumns(sqlColumns(conn, 'teams_enh')),
                           except=c('lgid', 'teamid', 'playerid', 'yearid', 'decadeid'))
