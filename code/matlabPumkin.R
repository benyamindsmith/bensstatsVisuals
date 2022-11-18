library(pracma)
library(rgl)

# 3D sphere function
sphere <- function(n) {
  dd <- expand.grid(theta = seq(0, 2*pi, length.out = n+1),
                    phi = seq(-pi, pi, length.out = n+1))
  with(dd, 
       list(x = matrix(cos(phi) * cos(theta), n+1),
            y = matrix(cos(phi) * sin(theta), n+1),
            z = matrix(sin(phi), n+1))
  )
}

# Pumpkin Code
sph <- sphere(200)
X <- sph[[1]]
Y <- sph[[2]]
Z <- sph[[3]]

# scaling
R <- 1 - (1 - seq(from=0, to=20, by=0.1) %% 2) ^ 2 / 12 # function
R2 <- 0.8 - Z[1,]^4 * 0.2

x <- R * X # scale rows for wavy side
y <- R * Y # scale rows for wavy side
z <- t(R2 * t(Z)) # scale columns by transpose for flat oval shape

# color according to distance to [0,0,0]
hypot_3d <- function(x, y, z) {
  return(sqrt(x^2 + y^2 + z^2))
}
c_ <- hypot_3d(x,y,z) + rnorm(201) * 0.03
color_palette <- terrain.colors(20) # color look-up table
col <- color_palette[ as.numeric(cut(c_, breaks = 20)) ] # assign color to 20 levels of c_


# Code for Stem

s <- c(1.5, 1, rep(0.7,6)) %*% t(c(rep(c(0.1,0.06),10),0.1))
mesh <- meshgrid(x=seq(from=0,to=pi/2,by=pi/15),y=seq(from=0,to=pi,by=pi/20))
tt<- mesh[[1]]
p <- mesh[[2]]
Xs<- -(0.4-cos(p)*t(s))*cos(tt)+0.4
Zs <- (0.5-cos(p)*t(s))*sin(tt) + 0.55
Ys <-  -sin(p)*t(s)
color_palette_2 <- hcl.colors(20,palette = "Purple-Brown")
col2 <- color_palette_2[ as.numeric(cut(c_, breaks = 20)) ] # assign color to 20 levels of c_

# Pumpkin
persp3d(x, y, z, color = col, aspect=FALSE,xlab="", ylab="", zlab="")

# Stem (Both sides)
surface3d(Xs,Ys,Zs,color=col2)
surface3d(Xs,-Ys,Zs,color=col2)
