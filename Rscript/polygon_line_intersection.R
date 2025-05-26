#' Find the intersection point where a line segment crosses a polygon boundary
#'
#' @param polygon A matrix with n rows and 2 columns, where each row represents 
#'                the x,y coordinates of a polygon vertex
#' @param line_start A numeric vector of length 2 with x,y coordinates of the line 
#'                   segment start point (must be inside the polygon)
#' @param line_end A numeric vector of length 2 with x,y coordinates of the line 
#'                 segment end point (must be outside the polygon)
#' @return A numeric vector of length 2 with x,y coordinates of the intersection point
#' @export
find_polygon_line_intersection <- function(polygon, line_start, line_end) {
  # Input validation
  if (!is.matrix(polygon) || ncol(polygon) != 2) {
    stop("polygon must be a matrix with 2 columns representing x,y coordinates")
  }
  
  if (nrow(polygon) < 3) {
    stop("polygon must have at least 3 vertices")
  }
  
  if (length(line_start) != 2 || length(line_end) != 2 || 
      !is.numeric(line_start) || !is.numeric(line_end)) {
    stop("line_start and line_end must be numeric vectors of length 2")
  }
  
  # Ensure polygon is closed (last point equals first point)
  if (!identical(polygon[1,], polygon[nrow(polygon),])) {
    polygon <- rbind(polygon, polygon[1,])
  }
  
  # Function to check if a point is inside a polygon using ray casting algorithm
  # A point is inside if a ray from the point to infinity crosses an odd number of polygon edges
  point_in_polygon <- function(point, poly) {
    x <- point[1]
    y <- point[2]
    n <- nrow(poly)
    inside <- FALSE
    
    for (i in 1:(n-1)) {
      # Get vertex coordinates
      x1 <- poly[i, 1]
      y1 <- poly[i, 2]
      x2 <- poly[i+1, 1]
      y2 <- poly[i+1, 2]
      
      # Check if point is on an edge
      if (((y1 <= y && y < y2) || (y2 <= y && y < y1)) && 
          (x < (x2 - x1) * (y - y1) / (y2 - y1) + x1)) {
        inside <- !inside
      }
    }
    
    return(inside)
  }
  
  # Function to find intersection between two line segments
  # Uses parametric equations of lines to find the intersection point
  line_segments_intersection <- function(p1, p2, p3, p4) {
    # Line segment 1: p1 to p2 (the input line)
    # Line segment 2: p3 to p4 (a polygon edge)
    
    # Extract coordinates
    x1 <- p1[1]; y1 <- p1[2]
    x2 <- p2[1]; y2 <- p2[2]
    x3 <- p3[1]; y3 <- p3[2]
    x4 <- p4[1]; y4 <- p4[2]
    
    # Calculate denominator for intersection equations
    # If zero, lines are parallel or coincident
    denominator <- (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
    
    if (abs(denominator) < 1e-10) {
      return(NULL)
    }
    
    # Calculate parametric values for intersection
    # ua represents position along first line segment (0-1)
    # ub represents position along second line segment (0-1)
    ua <- ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denominator
    ub <- ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) / denominator
    
    # Check if intersection is within both line segments
    if (0 <= ua && ua <= 1 && 0 <= ub && ub <= 1) {
      # Calculate intersection point coordinates using parametric form
      x <- x1 + ua * (x2 - x1)
      y <- y1 + ua * (y2 - y1)
      return(c(x, y))
    } else {
      return(NULL)  # No intersection within the segments
    }
  }
  
  # Verify that start point is inside and end point is outside
  start_inside <- point_in_polygon(line_start, polygon)
  end_inside <- point_in_polygon(line_end, polygon)
  
  if (!start_inside) {
    stop("The line segment must start inside the polygon")
  }
  
  if (end_inside) {
    stop("The line segment must end outside the polygon")
  }
  
  # Find intersections with each edge of the polygon
  n <- nrow(polygon)
  intersections <- list()
  
  for (i in 1:(n-1)) {
    # Get polygon edge vertices
    edge_start <- polygon[i, ]
    edge_end <- polygon[i+1, ]
    
    # Find intersection between line segment and polygon edge
    intersection <- line_segments_intersection(line_start, line_end, edge_start, edge_end)
    
    if (!is.null(intersection)) {
      intersections[[length(intersections) + 1]] <- intersection
    }
  }
  
  # Process results based on number of intersections found
  if (length(intersections) == 0) {
    stop("No intersection found. Check for numerical precision issues or invalid input.")
  } else if (length(intersections) == 1) {
    # Single intersection case
    intersection_point <- intersections[[1]]
  } else {
    # Multiple intersections - find closest to start point
    distances <- sapply(intersections, function(p) {
      sqrt(sum((p - line_start)^2))
    })
    intersection_point <- intersections[[which.min(distances)]]
  }
  
  return(intersection_point)
}
# This set of functions and example was initially written by Perplexity.ai 
#   (https://www.perplexity.ai/search/you-are-a-programmer-writing-a-xfAs665eQ06eGZp6Ta7ieA)
#   The following prompt was used:
# You are a programmer writing a function in R. The input to the function is a two-dimensional polygon and a line segment that begins inside the polygon and ends outside of the polygon. The function returns the coordinates of the point where the line segment intersects the polygon. Write this R function. Include comments in the function code explaining the methodology

# _________________________________________________________________
if (FALSE) { #    Example usage, set to TRUE if testing

# Create a square polygon
polygon <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)

# Define start (inside) and end (outside) points
start <- c(5, 5)
end <- c(15, 5)

# Calculate intersection
intersection <- find_polygon_line_intersection(polygon, start, end)
print(intersection)  # Should return c(10, 5)
}
