# <function>
# <name>
# xydist
# </name>
# <description>
# Distance between two pairs of x-y coordinates. Input can be atomic or vector. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
xydist=function(x1,y1,x2,y2) 
  return( sqrt( (x1-x2)^2 + (y1-y2)^2 ) )
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# xydistvect
# </name>
# <description>
# Distance between two x-y coordinates, but accepts each set of coordinates as a vector of length 2,
# with the first element the x coordinates, the second y. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
xydistvect=function(pt1,pt2)
  return( xydist(pt1[,1],pt1[,2],pt2[,1],pt2[,2]) )
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# xydistmat
# </name>
# <description>
# Distance between two x-y coordinates, but accepts two sets of coordinates in a single matrix (4 columns ordered x1, y1, x2, y2). 
# For use with apply.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
xydistmat=function(pts)
  return( xydist(pts[,1],pts[,2],pts[,3],pts[,4]) )
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# perpendicular.distance
# </name>
# <description>
# Distance from a point to a line (so it's the perpendicular distance);
# m and b are slope and intercept; x and y are (vectors) of points. Note check for infinite slope, meaning that
# the intercept b is the x-intercept.

# </description>
# <display>true</display>
# <update>true</update>
# <arguments>
# b: y-intercept
# m: line slope
# </arguments>
# <sample>
# 
# </sample>
# <source>
perpendicular.distance=function(b,m,x,y) 
 {
  if(!is.infinite(m)) return(sqrt((y-m*x-b)^2/(1+m^2)))
  else return(abs(x-b))
 }
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# parallel.line
# </name>
# <description>
# Finds the slope and intercept of the line parallel to a line whose slope and intercept are given, 
# through the point x,y. Note that the intercept is not needed. The is.infinite check is necessary for vertical lines.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
parallel.line=function(b,m,x,y)
{
 if(is.infinite(m)) return(data.frame(b=x,m=Inf))
 
 intercept=y-m*x
 result=data.frame(b=intercept,m=rep(m,length(intercept)))
 
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# intersection.of.lines
# </name>
# <description>
# Finds the point where 2 lines intersect, given lines as 2 parameters each (intercept b then slope m). 
# If the two lines are identical, it returns NAs. Note the check for both slopes being infinite (vertical lines).
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
intersection.of.lines=function(b1,m1,b2,m2)
{
 if(is.infinite(m1) & is.infinite(m2)) return(c(NA,NA))
 if(m1==m2 & b1==b2) return(c(x=NA,y=NA))

 if(is.infinite(m1) & !is.infinite(m2)) return(c(b1,m2*b1+b2))
 else if(!is.infinite(m1) & is.infinite(m2)) return(c(b2,m1*b2+b1))
 
 x=(b2-b1)/(m1-m2)
 y=m1*x+b1

 return(c(x,y))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# line.intersection
# </name>
# <description>
# Old version of function to return point where lines intercept, accepting slope then intercept, 
# rather than the more standard intercept then slope. Kept in case old functions call this version. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
line.intersection=function(m1,b1,m2,b2)
  return(intersection.of.lines(b1,m1,b2,m2))
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# line.intersection.pts
# </name>
# <description>
# Finds the point where 2 lines intersect, given each line as 2 pairs of points on the line
# </description>
# <arguments>
# Both arguments must have columns x, y, with two rows, one row per point. 
# </arguments>
# <sample>
# 
# </sample>
# <source>
line.intersection.pts=function(pts1,pts2)
{
 line1=slope.intercept.frompts(pts1[1,],pts1[2,])
 line2=slope.intercept.frompts(pts2[1,],pts2[2,])

 return(line.intersection(line1[1],line1[2],line2[1],line2[2]))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# pts.to.interceptslope
# </name>
# <description>
# Gets slope and intercept of a line given two pairs of coordinates on the line. If the x's are exactly
# equal, so slope is infinite, it returns the x as the first argument. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
pts.to.interceptslope=function(pt1,pt2)
{
 if(pt1$x==pt2$x) return(c(pt1$x,Inf))

 m=(pt2$y-pt1$y)/(pt2$x-pt1$x)
 b=pt1$y-m*pt1$x

 return(c(b,m))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# slope.intercept.frompts
# </name>
# <description>
# Old version of pts.to.interceptslope, returns slope than intercept. Opposite is more standard. Kept for
# compatibility with old functions. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
slope.intercept.frompts=function(pt1,pt2)
{
 line=pts.to.interceptslope(pt1,pt2)
 
 return(c(line[2:1]))
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# segmentPt
# </name>
# <description>
# Draw a line segment between two points, where each point is a vector of x then y coordinates
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
segmentPt=function(pt1,pt2) 
  segments(pt1[1],pt2[1],pt1[2],pt2[2])
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# drawrectangle
# </name>
# <description>
# Draws a rectangle given a matrix or dataframe of 4 x-y coordinates. The column names must be x and y.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
drawrectangle=function(corners,add=TRUE)
{
 if(!add)
  {
   xrange=range(corners$x)
   yrange=range(corners$y)
   plot(xrange[1],yrange[1],xlim=xrange,ylim=yrange,xlab="",ylab="",col="white",axes=FALSE)
  }

 segmentPt(corners[1,],corners[2,])
 segmentPt(corners[2,],corners[3,])
 segmentPt(corners[3,],corners[4,])
 segmentPt(corners[1,],corners[4,])
}
# </source>
# </function>

# <function>
# <name>
# insideRectangle
# </name>
# <description>
# Checks a vector of coordinates x, y to return which are inside a rectangle. For a much more general function for checking whether
# points are inside polygons, use the function inout() in the package splancs.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
insideRectangle=function(x,y,xrange,yrange)
{
 inX=x>=xrange[1] & x<xrange[2] & !is.na(x)
 inY=y>=yrange[1] & y<yrange[2] & !is.na(y)
 return(inX&inY)
}
# </source>
# </function>
# 


# <function>
# <name>
## are.ptsinside
# </name>
# <description>
# Checks many points (dataframe pt with x and y) against a single quadrat whose corners are given by as xlo, ylo, xhi, yhi.
# It returns a logical vector, TRUE for the points inside. This is same as insideRectange, but accepting input as a matrix pts
# and a single vector of the four corners of the rectange. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
are.ptsinside=function(pts,coord)
 return(insideRectange(x=pts[,1],y=pts[,2],xrange=coord[,c(1,3)],yrange=coord[,c(2,4)]))

# lowerx=coord[1]<pts[,1]
# upperx=coord[3]>=pts[,1]
# lowery=coord[2]<pts[,2]
# uppery=coord[4]>=pts[,2]

# return(lowerx & upperx & lowery & uppery)
# }

# </source>
# </function>

# <function>
# <name>
# ispt.inside
# </name>
# <description>
# Check a single pt (x and y) against a large number of quadrats whose corners are given by the rows of coord, xlo, ylo, xhi, yhi.
# It returns the fraction of quadrats which the point falls inside. This is exactly like are.ptsinside() but allows there to be many
# rectangles, defined by a dataframe coord.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
ispt.inside=function(pt,coord)
{
 norect=dim(coord)[1]
 inside=logical()
 
 for(i in 1:norect) logical[i]=are.ptsinside(pt,drp(coord[i,]))
 
 return(inside)
# lowerx=coord[,1]<pt[1]
# upperx=coord[,3]>=pt[1]
# lowery=coord[,2]<pt[2]
# uppery=coord[,4]>=pt[2]

# covered=length(which(lowerx & upperx & lowery & uppery))

# return(covered/dim(coord)[1])
}
# </source>
# </function>


# 
# <function>
# <name>
# inside.rect
# </name>
# <description>
# Determines whether any of the 4 corners of one rectangle are within a second rectangle. Both rectangles are submitted as c(x0,x1,y0,y1). If just one
# of the corners is inside, it returns true. See insideRectange(), which has a similar name but does something different.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
inside.rect=function(rect1,rect2)
{
 if(rect1[2]<rect2[1]) return(FALSE)
 if(rect1[1]>rect2[2]) return(FALSE)
 if(rect1[4]<rect2[3]) return(FALSE)
 if(rect1[3]>rect2[4]) return(FALSE)

 return(TRUE)
}
# </source>
# </function>
# 

# 
# <function>
# <name>
# circle
# </name>
# <description>
# Calculates points on a 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
circle=function(x,center=c(0,0),radius=5,half="top")
 {
  xprime=x-center[1]
  
  yprimesq=radius^2-xprime^2
  yprime=numeric()
  yprime[yprimesq>=0]=sqrt(yprimesq[yprimesq>=0])
  
  if(half=="bottom") yprime=(-1)*yprime
  
  y=yprime+center[2]
  return(y)
 }
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# fullcircle
# </name>
# <description>
# Create a dataframe for a full circle, with x values repeated to get top then bottom. NA is inserted
# so this can be passed directly to graphing functions.

# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
fullcircle=function(x,center=c(0,0),radius=5)
{
 tophalf=circle(x,center,radius,half='top')
 bottomhalf=circle(x,center,radius,half='bottom')
 len=length(x)
 result=data.frame(x=c(x,NA,x[len:1]),y=c(tophalf,NA,bottomhalf[len:1]))
 return(result)
}
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# ellipse
# </name>
# <description>
# Equation for (half) a canonical 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
ellipse=function(x,center=c(0,0),radius=c(7,5),half='top')
{
  xprime=x-center[1]
  inc=abs(xprime)<=radius[1]
  yprimesq=yprime=t=numeric()

  t[inc]=acos(xprime[inc]/radius[1])
  yprime[inc]=radius[2]*sin(t[inc])
  
  if(half=="bottom") yprime=(-1)*yprime
  
  y=yprime+center[2]
  return(y)
 }
# </source>
# </function>
# 
# 
# 
# <function>
# <name>
# fullellipse
# </name>
# <description>
# Creates a dataframe for a full ellipse. 
# </description>
# <arguments>
# <ul>
# <li> x = input x coordinates
# <li> center = x-y coordinates of ellipse center (vector of 2)
# </ul>
# </arguments>
# <sample>
# 
# </sample>
# <source>
fullellipse=function(x,center=c(0,0),radius=c(7,5))
{
 tophalf=ellipse(x,center,radius,half='top')
 bottomhalf=ellipse(x,center,radius,half='bottom')
 
 result=data.frame(x=c(x,NA,x),y=c(tophalf,NA,bottomhalf))
 return(result)
}
   

# </source>
# </function>
# 
# 

# <function>
# <name>
# cartesian.to.polar
# </name>
# <description>
# Convert Cartesian coordinates to polar. Returns a dataframe of two columns named r and theta. This always
# returns a theta between -pi/2 and pi/2. Note that polar.to.cartesian may not return the starting x, y submitted
# to cartesian.to.polar due to problems with signs. It will work if theta is kept positive (ie, quadrant 1).
# </description>
# <arguments>
# <ul>
# <li> x = input vector of x coordinates
# <li> y = input vector of y coordinates (same length as x)
# </ul>
# </arguments>
# <sample>
# cartesian.to.polar(2,4)
# </sample>
# <source>
cartesian.to.polar=function(x,y)
{
 r=sqrt(x^2+y^2)
 theta=atan(y/x)
 zeroes=(x==0)
 
 theta[zeroes&y>0]=pi/2
 theta[zeroes&y<0]=(-1)*pi/2
 
 return(data.frame(r,theta))
}
# </source>
# </function>
# 


# <function>
# <name>
# polar.to.cartesian
# </name>
# <description>
# Convert polar coordinates to Cartesian. Returns a dataframe of two columns named x and y.
# </description>
# <arguments>
# <ul>
# <li> r = input vector of radii (distance from origin)
# <li> theta = input vector of angle from horizontal (radians), same length as r
# </ul>
# </arguments>
# <sample>
# polar.to.cartesian(2,pi/3)
# </sample>
# <source>
polar.to.cartesian=function(r,theta)
{
 x=r*cos(theta)
 y=r*sin(theta)
 return(data.frame(x,y))
}
# </source>
# </function>

