detach("package:toaster", unload=T)
require(toaster)

dsn = "PartnersPresalesCluster"
uid = "beehive"
pwd = "beehive"
asterConn = odbcConnect(dsn, uid, pwd)
close(asterConn)

pitchingInfo = getTableSummary(asterConn, tableName='pitching', where='yearid between 2000 and 2013')
battingInfo = getTableSummary(asterConn, tableName='batting', where='yearid between 2000 and 2013')

salaryHistAll = computeHistogram(asterConn, tableName='salaries', columnName='salary', 
                                 binsize=100000, startvalue=0, 
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

# Histograms
h2000s = computeHistogram(asterConn, 'pitching_enh', 'era',
                 binsize=0.2, startvalue=0, endvalue=10, 
                 where="yearID between 2000 and 2012 and teamid in ('NYA','TEX')", by='teamid')
createHistogram(h2000s, fill='teamid', facet='teamid', title='TEX vs. NYY 2000-2012', xlab='ERA', ylab='count',
                legend.position='none')

h2009s = computeHistogram(asterConn, 'pitching_enh', 'era',
                          binsize=0.2, startvalue=0, endvalue=10, 
                          where="yearID between 2009 and 2012 and teamid in ('NYA','TEX')", by='teamid')
createHistogram(h2009s, fill='teamid', facet='teamid', title='TEX vs. NYY 2009-2012', xlab='ERA', ylab='count',
                legend.position='none')

# Analysis of BA by decade AL vs. NL
hBADecadeNL=computeHistogram(asterConn, 'batting_enh', 'ba',
                             startvalue=0.01, endvalue=0.5, binsize=0.01,
                             where="lgid='NL' and yearid between 1960 and 2009", by='decadeid')
hBADecadeAL=computeHistogram(asterConn, 'batting_enh', 'ba',
                             startvalue=0.01, endvalue=0.5, binsize=0.01,
                             where="lgid='AL' and yearid between 1960 and 2009", by='decadeid')
hBADecadeNL=cbind(hBADecadeNL, lgid='NL')
hBADecadeAL=cbind(hBADecadeAL, lgid='AL')
hBADecade = rbind(hBADecadeAL, hBADecadeNL)

createHistogram(hBADecade, fill='lgid', facet=c('decadeid','lgid'), facetScales='fixed', 
                title='AL vs. NL Batting Avg 1960-2009', xlab='BA', ylab='count',
                breaks=seq(from=0.0, to=0.5, by=0.05),
                legend.position='none')

# Analysis of ERA by decade AL vs. NL
hERADecadeNL=computeHistogram(asterConn, 'pitching_enh', 'era',
                            startvalue=0.01, endvalue=10., binsize=0.1,
                            where="lgid='NL' and yearid between 1960 and 2009", by='decadeid')
hERADecadeAL=computeHistogram(asterConn, 'pitching_enh', 'era',
                              startvalue=0.01, endvalue=10., binsize=0.1,
                             where="lgid='AL' and yearid between 1960 and 2009", by='decadeid')
hERADecadeNL=cbind(hERADecadeNL, lgid='NL')
hERADecadeAL=cbind(hERADecadeAL, lgid='AL')
hERADecade = rbind(hERADecadeAL, hERADecadeNL)

createHistogram(hERADecade, fill='lgid', facet=c('lgid','decadeid'), facetScales='fixed', 
                title='AL vs. NL ERA 1960-2009', xlab='ERA', ylab='count',
                breaks=seq(from=0.0, to=10.0, by=0.5),
                legend.position='none')

# Heatmap Analysis: BA by lgid
heatBA = computeHeatmap(asterConn, 'batting_enh', 'teamid', 'yearid',
                        aggregateFun=c("SUM(BA*AB)/SUM(AB)"),
                        aggregateAlias=c("ba"),
                        where="yearid between 1990 and 2012", by="lgid")

createHeatmap(heatBA, 'yearid', 'teamid', 'ba',
              title='BA Heatmap AL vs. NL 1990-2012', xlab='Year', ylab='Team',
              lowGradient=muted("grey", l=30, c=250), highGradient="red",
              text=TRUE, percent=FALSE, digits=3,
              facet='lgid', legend.position="none")

heatERA = computeHeatmap(asterConn, 'pitching_enh', 'teamid', 'yearid',
                         aggregateFun=c("SUM(IPOuts*ERA)/SUM(IPOuts)"),
                         aggregateAlias=c("era"),
                         where="yearid between 1990 and 2012", by="lgid")

createHeatmap(heatERA, 'yearid', 'teamid', 'era',
              title='ERA Heatmap AL vs. NL 1990-2012', xlab='Year', ylab='Team',
              lowGradient="red", highGradient=muted("grey", l=30, c=250),
              text=TRUE, percent=FALSE, digits=3,
              facet='lgid', legend.position="none")

# Bubble chart example

bubble = computeHeatmap(asterConn, 'teams_enh', 'teamid', 'decadeid',
                        aggregateFun=c("SUM(BA*AB)/SUM(AB)", 
                                       "SUM(IPOuts*ERA)/SUM(IPOuts)",
                                       "ROUND(AVG(rank))",
                                       "MIN(lgid)"),
                        aggregateAlias=c("ba", 
                                         "era",
                                         "rank",
                                         "lgid"),
                        where="yearid between 1970 and 2009")

bubble$size = 8 - bubble$rank
createBubblechart(bubble, "ba", "era", "size", label=NULL, fill="teamid",
                  facet="decadeid", ncol=1, shapeSizeRange=c(1,15),
                  title="Team Ranks by BA and ERA", 
                  textSize = 5, textColour = "black", textVjust = 1)

createBubblechart(bubble[bubble$decadeid >= 2000, ], "ba", "era", "size", label="teamid", fill="teamid",
                  facet=c("lgid", "decadeid"), ncol=1, shapeSizeRange=c(1,25),
                  title="Team Ranks by BA and ERA", 
                  textSize = 5, textColour = "black", textVjust = 1)