##Input data file
library(readxl)
data_emiten <- read_excel("G:/data emiten.xlsx")
data <- as.data.frame(data_emiten)

###Multicollinearity check###
library(PerformanceAnalytics)
chart.Correlation(data[,2:12])
##function of multicollinearity check
#in this case, the result >0.5 so it has multicollinearity

#bartlett test
uji_bart <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) 
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "Khi-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}
uji_bart(data[,2:12])
#Bartlett test or also known as the homogeneity test. Homogeneous assumptions must be fulfilled so that the model does not change for each observation or is not affected by time.

##KMO Test
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # remove blank data (NA)
  r <- cor(x)                             # Create a correlation matrix
  r2 <- r^2                               # coefficient value for r squared
  i <- solve(r)                           # Inverse matrix of the correlation matrix
  d <- diag(i)                            # diagonal elements of the inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # Partial quadratic correlation coefficient
  diag(r2) <- diag(p2) <- 0               # remove diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
kmo(data[,2:12]) #sample enough to use if kmo>0.5
#The Kaiser–Meyer–Olkin (KMO) test is a statistical measure to determine how suited data is for factor analysis.

###Analisis PCA###
library(factoextra)
pca_data <- prcomp(data[,2:12], scale. = TRUE)
pca_data
round(pca_data$rotation,2)
#the large number of factors seen from the eigen value > 1 or variance value > 80% 
round(pca_data$sdev^2,2) 
fviz_eig(pca_data, addlabels = TRUE, ylim = c(0, 80))
round(pca_data$rotation[,1:4],2)

##PCA result data
pca_fix=pca_data$x[,1:4]
pca_fix
new_Data=as.data.frame(pca_fix)
new_Data
uji_bart(new_Data)
