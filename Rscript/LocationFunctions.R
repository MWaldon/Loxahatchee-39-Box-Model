# Functions related to Refuge location
# These functions were translated from Excel VBA function in spreadsheet 
#   "Refuge Site X Y values.XLS" written by M. Waldon. 
# X,Y values are in NAD83 UTM Zone 17 North. 
# Latitude and Longitude in decimal degrees use WGS 84. 
# In the calculations the boundary of the refuge is defined by line segments
#   with vertices beginning at the G-300 (vertex 1, line segment 1) and 
#   counting up clockwise through 19 vertices and line segments.
#

# XCalculate <- function (Lat, Lon) { # return x coordinate from lat/long
# YCalculate <- function (Lat, Lon) { # return y coordinate from lat/long
# LongCalculate <-  function (X, Y) { # return long from UTM xy
# LatCalculate <- function (X, Y)   { # return lat from UTM xy

# CanalMeters <- function (X, Y, SegTable) { # returns clockwise canal distance
# CanalDistance <-  function (X, Y, SegTable) { # returns distance to canal
# NearestSeg <-  function (X, Y, SegTable) { # returns closest segment to x,y
# PointLineCalc <- function(PtX, PtY, X1, Y1, X2, Y2) { # returns list(xmin, ymin, PointLineDist)
# PointSegDistance <-  function (PtX, PtY, X1, Y1, X2, Y2) { # returns distance from point to a line segment
# PointLineDistance <-  function (PtX, PtY, X1, Y1, X2, Y2) { # returns distance from point to line at any place in or out of segment
# PointLineX <-  function (PtX, PtY, X1, Y1, X2, Y2) { # returns X-value of point on a line perpendicular to point off line
# PointLineY <-  function (PtX, PtY, X1, Y1, X2, Y2) { # returns Y-value of point on a line perpendicular to point off line
# SegLength <-  function(X1, Y1, X2, Y2){  # returns distance from point 1 to point 2, ends of a line segment

# SegTable.create <- function() { # import the boundary segment table

# ---------------- Translated lat/long VBA Macros ----------------
# This module contains functions to approximately convert 
#   Latitude/Longitude to X, Y and X, Y to Latitude/Longitude
# Coefficients come from sheet XYCalc

XCalculate <- function (Lat, Lon) { # return x coordinate from lat/long
  # Lat and Lon in decimal degrees 
  Intercept = 8586878.104
  CLat = -566.5854118
  CLon = 99652.62469
  #
  # Intercept = Workbooks("Refuge Site X Y values.XLS").Worksheets("XYCalc").Range("b4").Value
  # CLat = Workbooks("Refuge Site X Y values.XLS").Worksheets("XYCalc").Range("b5").Value
  # CLon = Workbooks("Refuge Site X Y values.XLS").Worksheets("XYCalc").Range("b6").Value
  XCalculate = Intercept + (CLat * Lat) + (CLon * Lon)
  return(XCalculate) 
  } # End Function XCalculate

YCalculate <- function (Lat, Lon) { # return y coordinate from lat/long
  # Lat and Lon in decimal degrees
  Intercept = 37553.11646
  CLat = 110756.4352
  CLon = 514.9289668
  # Intercept = Workbooks("Refuge Site X Y values.XLS").Worksheets("XYCalc").Range("b9").Value
  # CLat = Workbooks("Refuge Site X Y values.XLS").Worksheets("XYCalc").Range("b10").Value
  # CLon = Workbooks("Refuge Site X Y values.XLS").Worksheets("XYCalc").Range("b11").Value
  YCalculate = Intercept + (CLat * Lat) + (CLon * Lon)
  return(YCalculate)
  } # End Function YCalculate

LongCalculate <-  function (X, Y) { # return long from UTM xy
  Intercept = -86.16775781
  CX = 0.0000100346
  CY = 0.0000000513329
  # Intercept = Worksheets("XYCalc").Range("b23").Value
  # CX = Worksheets("XYCalc").Range("b24").Value
  # CY = Worksheets("XYCalc").Range("b25").Value
  LongCalculate = Intercept + (CX * X) + (CY * Y)
  return(LongCalculate)
  } # End Function LongCalculate

LatCalculate <- function (X, Y){ # return lat from UTM xy
  Intercept = 0.061550898
  CX = -0.0000000466528
  CY = 0.00000902858
  # Intercept = Worksheets("XYCalc").Range("b28").Value
  # CX = Worksheets("XYCalc").Range("b29").Value
  # CY = Worksheets("XYCalc").Range("b30").Value
  LatCalculate = Intercept + (CX * X) + (CY * Y)
  return(LatCalculate)
  } # End Function LatCalculate



# ---------------- Translated xy VBA Macros ----------------

CanalMeters <- function (X, Y, SegTable) { # returns clockwise canal distance
  # returns canal total meters to closest canal point
  # canal meters start at G-300 and go clockwise around the refuge canals
  #
  # find nearest segment
  iseg = NearestSeg(X, Y, SegTable)
  # cumulative distance to vertex 1 & 2
  x1Dist = SegTable$clen_m[iseg]
  x2Dist <- x1Dist + SegTable$len_m[iseg]
  # xy for vertex 1 and 2
  X1 = SegTable$x1[iseg]
  Y1 = SegTable$y1[iseg]
  X2 <- SegTable$x2[iseg]
  Y2 <- SegTable$y2[iseg]
  # distance to segment from point
  L = PointLineDistance(X, Y, X1, Y1, X2, Y2)
  # if returned distance <0 then perpendicular intersects outside of segment 
  if (L < 0) { # return canal clockwise distance to nearest vertex
    d1 <- SegLength(X, Y, X1, Y1)
    d2 <- SegLength(X, Y, X2, Y2)
    if (d1<d2) {return(x1Dist)} else {return(x2Dist)}
  } else 
    { # perpendicular intersects inside the segment
    Xseg <- PointLineX(X, Y, X1, Y1, X2, Y2)
    Yseg <- PointLineY(X, Y, X1, Y1, X2, Y2)
    iSegLength = SegLength(Xseg, Yseg, X1, Y1)
    return(x1Dist + iSegLength)
    } # end else 
  } # End function

CanalDistance <-  function (X, Y, SegTable) { # returns distance to canal
  # returns distance from point to the closest segment
  seg = NearestSeg(X, Y, SegTable)
  X1 = SegTable$x1[seg]
  Y1 = SegTable$y1[seg]
  X2 = SegTable$x2[seg]
  Y2 = SegTable$y2[seg]
  L = PointSegDistance(X, Y, X1, Y1, X2, Y2)
  CanalDistance = abs(L)
  } # End function

NearestSeg <-  function (X, Y, SegTable) { # returns closest segment to x,y
  # initialize variables
  SegDist = 1E+99 # shortest distance
  iseg = -1 # segment number
  n <- length(SegTable$SegID) # total number of segments
  for (i in 1:n) { # 1 To SegTable.Rows.Count
    X1 = SegTable$x1[i]
    Y1 = SegTable$y1[i]
    X2 = SegTable$x2[i]
    Y2 = SegTable$y2[i]
    L = PointSegDistance(X, Y, X1, Y1, X2, Y2)
    if (L <= SegDist) { # if seg i is shorter then use it
      SegDist = L
      iseg = i
      } #End if
    } # Next seg
  return(iseg) # NearestSeg = iseg
  } # End function

PointLineCalc <- function(PtX, PtY, X1, Y1, X2, Y2) { # returns list(xmin, ymin, PointLineDist)
  #   xmin, ymin = x, y on perpendicular from PtX, PtY to line through x1, y1 to x2, y2
  #   PointLineDist= distance from point to line at any place in or out of segment
  #     PointLineDist is negative if it xmin ymin is not on segment
  #----------------------------------------------------------------------
  # from Ask A Scientist, http://www.newton.dep.anl.gov/askasci/math99/math99076.htm
  #
  # 1) A(x,y) lies on a line L perpendicular to y = mx + b.
  # 2) L has slope - 1/m.
  # 3) Use point-slope equation of straight line to obtain
  #    equation for L.
  # 4) Solve L and y = mx + b simultaneously to get point
  #   B(x,y) on y = mx + b that is nearest A(x,y).
  # 5) Now use distance formula on the points A and B.
  # Have fun!
  # Dr. Robinson
  
  # The distance between a given point A(x0,y0) and a line ax+by+c=0 is
  # d = absolute value of [(ax0+by0+c)/sqrt(a^2+b^2)]
  # Just change the y=mx+b equation into the ax+by+c=0 form.
  # Dr. C. Murphy
  #----------------------------------------------------------------------
  
  DeltaX = X2 - X1
  DeltaY = Y2 - Y1
  # test for special cases
  if ((DeltaX == 0) & (DeltaY == 0)) { #  return error
    # points 1 and 2 are same point, they do not define a line
    # xmin = "ERROR-points are equal"
    # ymin = "ERROR-points are equal"
    # PointLineDist = "ERROR-points are equal"
    # CHANGED to return distance between the points
    PointLineDist <- SegLength(ptx, pty, X1, Y1)
    return(list(xmin=X1, ymin=Y1, PointLineDist=PointLineDist))
    } #End if
  if (DeltaX == 0) { # vertical slope
        xmin = X1
        ymin = PtY
        PointLineDist = abs(PtX - X1)
        if (((PtY - Y1) * (PtY - Y2)) > 0) { # falls outside segment
          PointLineDist = -PointLineDist
          } #End if
        return(list(xmin=xmin, ymin=ymin, PointLineDist=PointLineDist))
    } # End if
  
  if (DeltaY == 0) { # horizontal slope
        xmin = PtX
        ymin = Y1
        PointLineDist = abs(PtY - Y1)
        if (((PtX - X1) * (PtX - X2)) > 0) { # falls outside segment
            PointLineDist = -PointLineDist 
            } #End if
        return(list(xmin=xmin, ymin=ymin, PointLineDist=PointLineDist))
        } # End if
  
  # m is slope
  m = DeltaY / DeltaX
  # intercept
  b = Y1 - (m * X1)
  
  # slope of perpendicular line is -1/m, find intercept bp
  bp = PtY + (PtX / m)
  
  # min is at intersection of lines, location of minimum distance point on line
  xmin = (bp - b) / (m + (1 / m))
  ymin = (m * xmin) + b
  
  PointLineDist = SegLength(xmin, ymin, PtX, PtY)
  
  # return a negative value if min point is not on the line segment
  
  if (((xmin - X1) * (xmin - X2)) > 0) { # falls outside segment
    PointLineDist = -PointLineDist
    } # End if
  return(list(xmin=xmin, ymin=ymin, PointLineDist=PointLineDist))
  } # End Sub PointLineCalc

PointSegDistance <-  function (PtX, PtY, X1, Y1, X2, Y2) { 
  # returns distance from point to a line segment
  L = PointLineDistance(PtX, PtY, X1, Y1, X2, Y2)
  # PointSegDistance = L
  if (L >= 0) {
    return(L) # Then Exit function if perpendicular is in segment
  } else { # perpendicular outside segment then distance to nearest vertex
    L1 = SegLength(PtX, PtY, X1, Y1)
    L2 = SegLength(PtX, PtY, X2, Y2)
    return(min(L1,L2)) # PointSegDistance = Application.Min(L1, L2)
  } # end if
  } # End function

PointLineDistance <-  function (PtX, PtY, X1, Y1, X2, Y2) {
  # returns distance from point to line at any place in or out of segment
  dlist <- PointLineCalc(PtX, PtY, X1, Y1, X2, Y2)
  return(dlist$PointLineDist) #$PointLineDist)
  } # End function # PointLineDistance

PointLineX <-  function (PtX, PtY, X1, Y1, X2, Y2) {
  # returns X-value of point on a line perpendicular to point off line
  PLCalc <- PointLineCalc(PtX, PtY, X1, Y1, X2, Y2)
  if(PLCalc$PointLineDist >=0) { # perpendicular falls within segment
    xmin <- PLCalc$xmin
  } else { # perpendicular falls outside of segment
    d1 <- SegLength(PtX, PtY, X1, Y1)
    d2 <- SegLength(PtX, PtY, X2, Y2)
    if (d1<d2) {xmin <- X1} else {xmin <- X2}
  } # end if else
  return(xmin)
  } # end PointLineX

PointLineY <-  function (PtX, PtY, X1, Y1, X2, Y2) {
  # returns Y-value of point on a line perpendicular to point off line
  # ymin <- PointLineCalc(PtX, PtY, X1, Y1, X2, Y2)$ymin
  PLCalc <- PointLineCalc(PtX, PtY, X1, Y1, X2, Y2)
  if(PLCalc$PointLineDist >=0) { # perpendicular falls within segment
    ymin <- PLCalc$ymin
  } else { # perpendicular falls outside of segment
    d1 <- SegLength(PtX, PtY, X1, Y1)
    d2 <- SegLength(PtX, PtY, X2, Y2)
    if (d1<d2) {ymin <- Y1} else {ymin <- Y2}
  } # end if else
  return(ymin) # PointLineY = ymin
  } # End PointLineY


SegLength <-  function(X1, Y1, X2, Y2){ 
  # returns distance from point 1 to point 2, ends of a line segment
  #
  SegLength = (((X2 - X1) ^ 2) + ((Y2 - Y1) ^ 2)) ^ 0.5
  
  } # End function

# -------------------- other related functions ---------------------
SegTable.create <- function() { # import the boundary segment table
  # read boundary segment table from spreadsheet "Refuge Site X Y values.XLS"
  
  library(readxl)
  SegTable <- read_excel(
    "../DataSets/Refuge Site X Y values.XLS", 
    sheet = "Boundary", range = "A28:M47")
  
  # set names of the canal or structure name for vertex 1
  #  (the canal that is named X here is the short canal segment G300-G301)
  V1Name <- c('L-40 G300',  rep('L-40',12), 'L-39', 'L-7', 
              'L-7', 'L-7 G301', 'X', 'X S-5A')
  SegTable <- cbind(V1Name, SegTable) # add a column of canal/site names
  
  # name the columns
  ColNam <- c('V1Name', 'SegID', 'V1ID', 'x1', 'y1', 'V2ID', 'x2', 'y2',
              'len_m', 'len_ft', 'len_mi', 
              'clen_m', 'clen_ft', 'clen_mi')
  names(SegTable) <- ColNam
  
  return(SegTable)
}

Refuge.xybound <- function(SegTable) { # return Refuge xy bounding box corners
  xmin <- min(SegTable$x1)
  xmax <- max(SegTable$x1)
  ymin <- min(SegTable$y1)
  ymax <- max(SegTable$y1)
  bbox <- c(xmin, xmax, ymin, ymax)
  names(bbox) <- c('xmin', 'xmax', 'ymin', 'ymax')
  return(bbox)
}

is.in_polygon <- function(x, y, pol_x, pol_y) {
  # Return logical TRUE if X,Y is inside polygon
  # x and y are coordinates of a single point
  # pol_x and pol_y are arrays defining polynomial vertices
  library(sp)
  result <- point.in.polygon(x, y, pol_x, pol_y)
  return(result != 0) # return TRUE if the point is in or on the polygon
}

SegTable <- SegTable.create() # create the table

# View(SegTable)
# plot(SegTable$x1, SegTable$y1)
# lines(c(SegTable$x1,SegTable$x2[19]),c(SegTable$y1, SegTable$y2[19]), col='red')

# -------------------------------------------
# script to test the functions
#
if(FALSE) {
  
# ----------------------- 
# print test results and plot a single point
bb <- Refuge.xybound(SegTable)
xmin <- bb['xmin']
xmax <- bb['xmax']
ymin <- bb['ymin']
ymax <- bb['ymax']
# plot the bounding box
plot(c(xmin,xmax,xmax,xmin,xmin),  
     c(ymin,ymin,ymax,ymax,ymin), type = 'l', col = 'gray', asp = 1)
# plot boundary vertices and border
points(SegTable$x1, SegTable$y1)
lines(c(SegTable$x1,SegTable$x2[19]),c(SegTable$y1, SegTable$y2[19]), col='red')
# choose a random point in the bounding box and plot it
x <- runif(n = 1, min = xmin, max = xmax)
y <- runif(n = 1, min = ymin, max = ymax)
print(paste('trial xy =', x, ', ',y))
points(x,y, col = 'blue')


# CanalMeters <- function (X, Y, SegTable) { # returns clockwise canal distance
print(paste(CanalMeters(x,y, SegTable), '= clockwise canal distance from G-300'))
# CanalDistance <-  function (X, Y, SegTable) { # returns distance to canal
print(paste(CanalDistance(x,y, SegTable), '= distance to canal'))
# NearestSeg <-  function (X, Y, SegTable) { # returns closest segment to x,y
i <- NearestSeg(x,y, SegTable)
x1 <- SegTable$x1[i]
y1 <- SegTable$y1[i]
x2 <- SegTable$x2[i]
y2 <- SegTable$y2[i]
print(paste(i, '= closest segment to x,y'))
print(paste('x1,y1 x2,y2 =', x1, x2, '  ', x2, y2))
# PointLineCalc <- function(PtX, PtY, X1, Y1, X2, Y2) { # returns list(xmin, ymin, PointLineDist)
dlist <- PointLineCalc(x, y, x1, y1, x2, y2)
print('PointLineCalc return list')
print(dlist)
# PointSegDistance <-  function (PtX, PtY, X1, Y1, X2, Y2) { # returns distance from point to a line segment
print(paste(PointSegDistance(x, y, x1, y1, x2, y2), '= distance from point to line segment'))
# PointLineDistance <-  function (PtX, PtY, X1, Y1, X2, Y2) { # returns distance from point to line at any place in or out of segment
print(paste(PointLineDistance(x, y, x1, y1, x2, y2), '= distance from point to line segment'))
# PointLineX <-  function (PtX, PtY, X1, Y1, X2, Y2) { # returns X-value of point on a line perpendicular to point off line
x3 <- PointLineX(x, y, x1, y1, x2, y2)
print(paste(x3, '= x-value of point on a line perpendicular to point off line'))
# PointLineY <-  function (PtX, PtY, X1, Y1, X2, Y2) { # returns Y-value of point on a line perpendicular to point off line
y3 <- PointLineY(x, y, x1, y1, x2, y2)
print(paste(y3, '= y-value of point on a line perpendicular to point off line'))
# SegLength <-  function(X1, Y1, X2, Y2){  # returns distance from point 1 to point 2, ends of a line segment
print(paste(SegLength(x1, y1, x2, y2), '= length of segment'))

# plot closest segment
lines(c(x1,x2), c(y1,y2), col = 'blue')
# plot lie from point to segment
points(x3, y3, col = 'green')
lines(c(x,x3), c(y,y3), col = 'green')

# -----------------------
# plot for many points
# plot the bounding box (new plot)
plot(c(xmin,xmax,xmax,xmin,xmin),  
     c(ymin,ymin,ymax,ymax,ymin), type = 'l', col = 'gray', asp = 1)
# plot boundary vertices and border
points(SegTable$x1, SegTable$y1)
lines(c(SegTable$x1,SegTable$x2[19]),c(SegTable$y1, SegTable$y2[19]), col='red')

# generate a grid of xy points inside the bounding box
xystep <- 500
for (x in seq(from = xmin, to = xmax, by = xystep)) {
  for (y in seq(from = ymin, to = ymax, by = xystep)) {
    if (is.in_polygon(x, y, SegTable$x1, SegTable$y1)){ # if xy is inside refuge
      i <- NearestSeg(x,y, SegTable)
      x1 <- SegTable$x1[i]
      y1 <- SegTable$y1[i]
      x2 <- SegTable$x2[i]
      y2 <- SegTable$y2[i]
      x3 <- PointLineX(x, y, x1, y1, x2, y2)
      y3 <- PointLineY(x, y, x1, y1, x2, y2)
      lines(c(x,x3), c(y,y3), col = 'green')
      points(x3, y3, col = 'green')
      points(x,y, col = 'blue', cex = 0.5)
      # test the canal distance and x,y functions
      d1 <- SegLength(x3, y3, x, y)
      d2 <- CanalDistance(x,y, SegTable)
      if (abs(d1-d2)>1) {print(paste('distance error at xy=', x, y))}
    } # end if
  } # end for y
} # end for x

} # end if FALSE

