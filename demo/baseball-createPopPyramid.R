# Demo create population pyramid
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

# Compare salaries by league
salaryHistAll = computeHistogram(conn, tableName='public.salaries', columnName='salary', 
                                 binsize=200000, startvalue=0, 
                                 by='lgid', where='yearID between 2000 and 2013')
createPopPyramid(data=salaryHistAll, bin='bin_start', count='bin_count', divideBy='lgid', 
                 values=c('NL','AL'),
                 title="Salary Pyramid by MLB Leagues", xlab='Salary', ylab='Player Count')

# Same salary pyramid for up to 5 million 
salaryHist5Mil = computeHistogram(conn, tableName='salaries', columnName='salary', 
                                  binsize=100000, startvalue=0, endvalue=5000000,
                                  by='lgid', where='yearID between 2000 and 2013')
createPopPyramid(data=salaryHist5Mil, divideBy='lgid', values=c('NL','AL'),
                 title="Salary Pyramid by MLB Leagues (less 5M only)", xlab='Salary', ylab='Player Count')

# ERA Pyramid by Leagues
eraHist = computeHistogram(conn, tableName='pitching', columnName='era', 
                           binsize=.1, startvalue=0, endvalue=10,
                           by='lgid', where='yearid between 2000 and 2013')
createPopPyramid(data=eraHist, divideBy='lgid', values=c('NL','AL'),
                 title="ERA Pyramid by MLB Leagues", xlab='ERA', ylab='Player Count')

# Log ERA 
# you have to alter table in Aster to add column era_log = log(era)
eraLogHist = computeHistogram(conn, tableName='pitching', columnName='era_log', 
                              binsize=.02, startvalue=-0.42021640338318984325, 
                              endvalue=2.2764618041732441,
                              by='lgid', where='yearid between 2000 and 2013 and era > 0')
createPopPyramid(data=eraLogHist, divideBy='lgid', values=c('NL','AL'),
                 title="log(ERA) Pyramid by MLB Leagues", xlab='log(ERA)', ylab='Player Count')

# Batting (BA) Pyramid by Leagues
battingHist = computeHistogram(conn, tableName='batting_enh', columnName='ba', 
                               binsize=.01, startvalue=0.01, endvalue=0.51,
                               by='lgid', where='yearid between 2000 and 2013')
createPopPyramid(data=battingHist, divideBy='lgid', values=c('NL','AL'),
                 title="Batting BA Pyramid by MLB Leages", xlab='BA', ylab='Player Count')
