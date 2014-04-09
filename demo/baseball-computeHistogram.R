# Demo compute and create histogram
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

# Compare Yankees and Rangers ERA in 2000s
h2000s = computeHistogram(conn, 'pitching_enh', 'era',
                          binsize=0.2, startvalue=0, endvalue=10, 
                          where="yearID between 2000 and 2012 and teamid in ('NYA','TEX')", by='teamid')
createHistogram(h2000s, fill='teamid', facet='teamid', title='TEX vs. NYY 2000-2012', xlab='ERA', ylab='count',
                legendPosition='none')


# Compare League BA by decades
hBADecadeLg=computeHistogram(conn, 'batting_enh', 'ba', by=c('lgid','decadeid'),
                             startvalue=0.01, endvalue=0.5, binsize=0.01,
                             where="decadeid between 1960 and 2000")
createHistogram(hBADecadeLg, fill='lgid', facet=c('decadeid','lgid'), facetScales='fixed', 
                title='AL vs. NL Batting Avg 1960-2009', xlab='BA', ylab='count',
                breaks=seq(from=0.0, to=0.5, by=0.05), paletteValues = c("red","blue"),
                legendPosition='none')

# Compare League ERA by decades
hERADecadeLg=computeHistogram(conn, 'pitching_enh', 'era',
                              startvalue=0.01, endvalue=10., binsize=0.1,
                              where="decadeid between 1960 and 2000", by=c('lgid','decadeid'))
createHistogram(hERADecadeLg, fill='lgid', facet=c('lgid','decadeid'), facetScales='fixed', 
                title='AL vs. NL ERA 1960-2009', xlab='ERA', ylab='count',
                breaks=seq(from=0.0, to=10.0, by=0.5), paletteValues = c("red","blue"),
                legendPosition='none')
