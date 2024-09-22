# Analysing blue light filter for prescription glasses
# www.overfitting.net
# https://www.overfitting.net/2024/09/cuantificando-el-efecto-del-filtro-de.html

library(tiff)


# READ IMAGES
# RAW linear development: dcraw -v -r 1 1 1 1 -S 16383 -o 0 -4 -T *.ARW
img1=readTIFF("foto1_singafas.tiff", native=FALSE, convert=FALSE)
img2=readTIFF("foto2_congafas.tiff", native=FALSE, convert=FALSE)

DIMY=nrow(img1)
DIMX=ncol(img1)


# Crop centre
DELTA=50
img1=img1[(DIMY/2-DELTA):(DIMY/2+DELTA-1), (DIMX/2-DELTA):(DIMX/2+DELTA-1), ]
img2=img2[(DIMY/2-DELTA):(DIMY/2+DELTA-1), (DIMX/2-DELTA):(DIMX/2+DELTA-1), ]

writeTIFF(img1, "foto1_singafas_crop.tiff", bits.per.sample=16, compression="LZW")
writeTIFF(img2, "foto2_congafas_crop.tiff", bits.per.sample=16, compression="LZW")


# Estimate Luminance
img1L=array(0, c(2*DELTA, 2*DELTA, 4))
img2L=img1L
img1L[,,1:3]=img1
img2L[,,1:3]=img2

img1L[,,4]=0.299*img1[,,1] + 0.587*img1[,,2] + 0.114*img1[,,3]
img2L[,,4]=0.299*img2[,,1] + 0.587*img2[,,2] + 0.114*img2[,,3]
rm(img1, img2)


# Calculate loss
colores=c("R", "G", "B", "L")
for (i in 1:4) {
    f=median(img2L[,,i]) / median(img1L[,,i])
    print(paste0(colores[i], " loss: ", round(f*100,1), "% / ",
                 round(log(f,2),3), "EV"))
}
