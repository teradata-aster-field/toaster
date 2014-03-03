dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(asterConn)
asterConn = odbcConnect(dsn, uid, pwd)


pitchingInfo = getTableSummary(asterConn, tableName='pitching', where='yearid between 2000 and 2013')
battingInfo = getTableSummary(asterConn, tableName='batting', where='yearid between 2000 and 2013')

salaryHistAll = computeHistogram(asterConn, tableName='public.salaries', columnName='salary', 
                                 binsize=200000, startvalue=0, 
                                 by='lgid', where='yearID between 2000 and 2013')
createPopPyramid(data=salaryHistAll, bin='bin_start', count='bin_count', divideBy='lgid', values=c('NL','AL'),
                 title="Salary Pyramid by MLB Leagues", xlab='Salary', ylab='Player Count')

salaryHist5Mil = computeHistogram(asterConn, tableName='salaries', columnName='salary', 
                                  binsize=100000, startvalue=0, endvalue=5000000,
                                  by='lgid', where='yearID between 2000 and 2013')
createPopPyramid(data=salaryHist5Mil, divideBy='lgid', values=c('NL','AL'),
                 title="Salary Pyramid by MLB Leagues (less 5M only)", xlab='Salary', ylab='Player Count')

eraHist = computeHistogram(asterConn, tableName='pitching', columnName='era', binsize=.1, startvalue=0, endvalue=10,
                           by='lgid', where='yearid between 2000 and 2013')
createPopPyramid(data=eraHist, divideBy='lgid', values=c('NL','AL'),
                 title="ERA Pyramid by MLB Leagues", xlab='ERA', ylab='Player Count')

# Log ERA
eraLogHist = computeHistogram(asterConn, tableName='pitching', columnName='era_log', binsize=.02, startvalue=-0.42021640338318984325, 
                              endvalue=2.2764618041732441,
                              by='lgid', where='yearid between 2000 and 2013 and era > 0')
createPopPyramid(data=eraLogHist, divideBy='lgid', values=c('NL','AL'),
                 title="log(ERA) Pyramid by MLB Leagues", xlab='log(ERA)', ylab='Player Count')



# Batting (BA)
battingHist = computeHistogram(asterConn, tableName='batting_enh', columnName='ba', binsize=.01, startvalue=0.01, endvalue=0.51,
                               by='lgid', where='yearid between 2000 and 2013')
createPopPyramid(data=battingHist, divideBy='lgid', values=c('NL','AL'),
                 title="Batting BA Pyramid by MLB Leages", xlab='BA', ylab='Player Count')