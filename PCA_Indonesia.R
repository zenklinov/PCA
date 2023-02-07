##Input file data
library(readxl)
data_emiten <- read_excel("G:/data emiten.xlsx")
data <- as.data.frame(data_emiten)

###cek multikolinearitas###
library(PerformanceAnalytics)
chart.Correlation(data[,2:12])
##fungsi cek multiko 
#saat dicek ada multiko karna >0.5

#uji bartlett
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
#uji Bartlett atau dapat disebut juga sebagai uji homogenitas. Asumsi homogen harus memenuhi agar model tidak berubah untuk setiap pengamatan atau tidak dipengaruhi oleh waktu.

##Uji KMO
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # menghilangkan data kosong (NA)
  r <- cor(x)                             # Membuat matrix korelasi
  r2 <- r^2                               # nilai koefisien untuk r squared
  i <- solve(r)                           # Inverse matrix dari matrix korelasi
  d <- diag(i)                            # element diagonal dari inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # koefisien korelasi Parsial kuadrat
  diag(r2) <- diag(p2) <- 0               # menghapus element diagonal 
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
kmo(data[,2:12]) #sampel cukup untuk digunakan jika kmo>0.5
#The Kaiser–Meyer–Olkin (KMO) test is a statistical measure to determine how appropriate data is for factor analysis.

###Analisis PCA###
library(factoextra)
pca_data <- prcomp(data[,2:12], scale. = TRUE)
pca_data
round(pca_data$rotation,2)
#bayaknya jumlah faktor dilihat dari nilai eigen value > 1 atau nilai varians > 80% 
round(pca_data$sdev^2,2) 
fviz_eig(pca_data, addlabels = TRUE, ylim = c(0, 80))
round(pca_data$rotation[,1:4],2)

##data hasil PCA
pca_fix=pca_data$x[,1:4]
pca_fix
new_Data=as.data.frame(pca_fix)
new_Data
uji_bart(new_Data)
