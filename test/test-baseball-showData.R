# Test showData with Lahman's baseball dataset in Aster

dsn = "LocalQueenVMWare"
dsn = "PartnersPresalesCluster"
uid = "beehive"
pwd = "beehive"
close(asterConn)
asterConn = odbcConnect(dsn, uid, pwd)

pitchingInfo = getTableSummary(asterConn, 'pitching_enh', where='yearid between 2000 and 2013')
battingInfo = getTableSummary(asterConn, 'batting_enh', where='yearid between 2000 and 2013')

# Overview
showData(asterConn, 'pitching_enh', pitchingInfo, type='numeric', format='overview',
         except=c('lgid', 'teamid', 'yearid','playerid', 'bfp', 'ipouts', 'h', 'so','r','g','bb','er'), 
         measures = c('average','deviation','IQR','0%','10%','25%','50%','75%','90%','100%'),
         where='yearid between 2000 and 2013')

showData(asterConn, 'pitching_enh', pitchingInfo, type='numeric', format='overview',
         include=c('h','so','r','g','bb','er','hr'), 
         measures = c('average','deviation','IQR','0%','10%','25%','50%','75%','90%','100%'),
         scales="free_y",
         where='yearid between 2000 and 2013')

showData(asterConn, 'pitching_enh', pitchingInfo, type='numeric', format='overview',
         include=c('era','fip','whip','ktobb'), 
         measures = c('average','deviation','IQR','0%','10%','25%','50%','75%'),
         scales="fixed",
         where='yearid between 2000 and 2013')

showData(asterConn, 'batting_enh', battingInfo, type='numeric', format='overview',
         except=c('legid', 'teamid', 'yearid','playerid'), where='yearid between 2000 and 2013')

# boxplot
showData(asterConn, 'pitching_enh', pitchingInfo, 
         except=c('yearid', 'teamid','bfp','ipouts','so','r','h','er','era','sv','g','bb'),
         format='boxplot', facet=TRUE, 
         defaultTheme=theme_grey(),
         where='yearid between 2000 and 2013')

showData(asterConn, 'batting_enf', battingInfo,
         # except=c('yearid','g_old','ab','tb','tob','so','h','bb','g_batting','g'),
         include = c('ab','tb','tob','so','h','bb','g_batting','g'),
         format='boxplot', facet=FALSE, coordFlip=TRUE,
         defaultTheme=theme_grey(),
         where='yearid between 2000 and 2013')

# histogram
showData(asterConn, 'pitching_enh', pitchingInfo,
         include=c('baopp', 'era', 'whip', 'ktobb', 'fip'), 
         # include=c('w', 'l', 'g', 'ipouts', 'h', 'er', 'hr', 'bb', 'so', 'bfp', 'r'),
         format='histogram', numBins=50, facet=TRUE,
         where='yearid between 2000 and 2013')

showData(asterConn, 'batting_enh', battingInfo,
         include=c('ba','ta','slg'),
         format='histogram', numBins=100, facet=TRUE,
         where='yearid between 2000 and 2013 
                and ab >= 30')

# correlation
showData(asterConn, tableName='pitching_enh', tableInfo=pitchingInfo, 
         include=c('w', 'l', 'g', 'ipouts', 'h', 'er', 'hr', 'bb', 'so', 'bfp', 'r'),
                #   'baopp', 'era', 'whip', 'ktobb', 'fip'),
         format='corr', corrLabel='value', digits=2, shapeSizeRange=c(5,25))

showData(asterConn, tableName='batting_enh', tableInfo=battingInfo,
         include=c('ab', 'r', 'h', 'x2b', 'x3b', 'hr', 'rbi', 'sb', 'cs', 'bb', 'so', 'ibb', 'hbp', 'sh', 'sf', 'gidp',
                   'tb', 'tob', 'xbh', 'ba', 'slg', 'ta'),
         format='corr', corrLabel='value', digits=2, shapeSizeRange=c(5,25))

# scatterplots

showData(asterConn, 'pitching_enh', format='scatterplot', include=c('so','bb', 'lgid', 'yearid'), 
         sampleSize=10000, facetName=c('lgid','yearid'), regressionLine=TRUE,
         where='yearid in (1980, 2010)')

showData(asterConn, 'pitching_enh', format='scatterplot', include=c('ktobb', 'fip', 'teamid','yearid'),
         sampleSize=10000, facetName=c('teamid','yearid'), regressionLine=TRUE,
         where="yearid in (2010,2011,2012) and teamid in ('TEX','NYA')")

