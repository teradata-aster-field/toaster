detach("package:toaster", unload=T)
require(toaster)

dsn = "LocalQueenVMWare"
dsn = "PresalesPartnersDB"
uid = "beehive"
pwd = "beehive"
close(asterConn)
asterConn = odbcConnect(dsn, uid, pwd)

# pre-process data to re-use
pitchingInfo = getTableSummary(asterConn, 'pitching_enh')
shrinkInfo = getTableSummary(asterConn, 'shrink')

# Boxplots

# all numerical attributes

# boxplot with facet (facet_wrap)


# Correlation matrix
# on all numerical attributes
showData(asterConn, tableName='pitching_enh', tableInfo=pitchingInfo, format='corr')

showData(asterConn, tableName='pitching_enh', tableInfo=pitchingInfo, format='scatterplot', 
         include=c('bb', 'so'), regressionLine=TRUE,
         sampleSize=10000, facetName='lgid',
         where='yearid >= 2000')

# correlation matrix on selected attributes
# with labeling by attribute pair name and
# controlling size of correlation bubbles
showData(asterConn, tableName='pitching', tableInfo=pitchingInfo, include=c('ERA','H','HR','GS','G','SV'), 
         format='corr', corrLabel='pair', shapeSizeRange=c(5,25))

# Histogram on all numeric attributes
showData(asterConn, tableName='pitching_enh', tableInfo=pitchingInfo, include=c('hr'), format='histogram')
showData(asterConn, tableName='pitching_enh', tableInfo=pitchingInfo, include=c('era', 'fip', 'so','bb','hr'), format='histogram')

# Overview is a histogram of statistical measures across attributes
showData(asterConn, tableName='pitching', tableInfo=pitchingInfo, format='overview', type='numeric')
showData(asterConn, tableName='shrink', tableInfo=shrinkInfo, format='overview', type='numeric',
         include=c('extended_cost', 'quantity'))


# Simple histogram on an attrbute
h = computeHistogram(asterConn, 'shrink', 'facility_id')
createHistogram(h)

# Scatterplot on pair of numerical attributes
# with facet_wrap (single-valued facetName)
showData(asterConn, tableName='shrink', tableInfo=shrinkInfo, format='scatterplot', include=c('extended_cost', 'quantity'),
         sampleSize=10000, facetName='activity_factor_id', 
         where="activity_factor_id in (601, 611)")

# Scatterplot on pair of numerical attributes
# with facet_grid (two-valued facetName)
showData(asterConn, tableName='shrink', tableInfo=shrinkInfo, format='scatterplot', include=c('extended_cost', 'quantity'),
         sampleSize=10000, facetName=c('activity_factor_id','facility_id'), 
         where="activity_factor_id in (601, 611) and facility_id in (1121,1219)")