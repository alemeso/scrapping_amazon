#scraping amazon
rm( list= ls() )

library(rvest)
library(dplyr)
library(tidyverse)
library(readxl)
library(googledrive)

#url<- "https://www.amazon.com.mx/s?k=hausklein&__mk_es_MX=%C3%85M%C3%85%C5%BD%C3%95%C3%91&crid=8EJJUTAGK156&sprefix=hausklein%2Caps%2C235&ref=nb_sb_noss_1"
setwd("C:/Users/ameso/OneDrive/Escritorio/Scraping/csvs/propios amz mx")
asins <- read_excel("Asins.xlsx")

#asin<- c("B08Y941X1X","B08Y943W82", "B09FB16FMP")#formato pagina

# "B09F9WDS82", "B09F9Y86KT", "B09F9ZB1FV","B09F9ZN4XJ", "B09FB16FMP", "B09FB1S6NB","B09FB22N4Q","B09FB41BQZ","B09FBS4YHZ", "B09FBSXWYX","B09FBW3D2C","B09FBW5S4L","B09FBWJQNK","B09FBWP9SY","B09FC2FML4","B09FC38WHW","B09G7CRJ9V","B09G8GWR1L","B09G8H2N1X","B09G8JKDPL","B09G8R88NW","B09MKVYBN9","B09SM841WG","B09VTNFJ46","B09VTNK8WD","B09VTP8RP3","B09VV5TM59","B09VV9QWDK","B09VVM1MNW")

#pagina <- read_html(url)

#nombre <- pagina %>% html_nodes(".a-size-base-plus") %>% html_text()

#link=paste0("https://www.amazon.com.mx/dp/",asin)  #2 sin error, 1 con error
link=paste0("https://www.amazon.com.mx/dp/",asins$Idpubli)

#link=paste0("https://www.amazon.com.mx/dp/","B08Y941X1X") #sin error

#link=paste0("https://www.amazon.com.mx/dp/","B09FB16FMP")# con error

obtenerdatos <- function(link){
  #as = "B08Y95WNF5" #con desc
  #as = "B08F9JMYGM"  #sin desc
  #as = "B097JW67YK"  #con promo
  
  #link=paste0("https://www.amazon.com.mx/dp/",as)
  pagina_producto = read_html("https://www.google.com/")
  try(pagina_producto <- read_html(link))
  
 
  if(!exists("pagina_producto")){
    precio=1
    tachado=1
    promo=1
    reviews=1
    estrellas=1
    seller=1
  }
  else{

  Sys.sleep(4)
  
  ###precio lista   .a-price-whole
  precio<- pagina_producto %>% html_nodes(".a-price-whole") %>% html_text()
  precio <- gsub('[,]','',precio)
  precio <- gsub('[.]','',precio)
  precio=as.numeric(precio[1])
  precio
  
  Sys.sleep(1)
  
  #tachado
  tachado <- pagina_producto %>% html_nodes(".a-color-secondary .a-size-base span") %>% html_text()
  tachado=gsub('[$]','',tachado)
  tachado=as.numeric(tachado[1])
  tachado
  
  Sys.sleep(2)
  
  ###tipo promo
  #promo <- pagina_producto %>% html_nodes(".a-text-ellipsis") %>% html_text()
  #promo=promo[2]
  
  Sys.sleep(2)

  
  ##reviews
  reviews <- pagina_producto %>% html_nodes("#acrCustomerReviewText") %>% html_text()
  reviews=reviews[1]
  reviews=gsub(' calificaciones','',reviews)
  reviews=as.numeric(reviews)
  
  ####estrellas
  estrellas <- pagina_producto %>% html_nodes("#acrPopover .a-star-4-5") %>% html_text()
  estrellas = estrellas[1]
  estrellas = gsub(" de 5 estrellas",'', estrellas)
  estrellas = as.double(estrellas)
  
  Sys.sleep(3)
  
  #### vendedor   
  
  seller <- pagina_producto %>% html_nodes("#sellerProfileTriggerId") %>% html_text()
  seller = seller[1]
  Sys.sleep(2)

  
  ### se une
  }
  data=rbind(link, precio, tachado, reviews, estrellas, seller)
  return(data)
  

}

datos = sapply(link, FUN = obtenerdatos, USE.NAMES = FALSE)

datos2=datos
## data-asin  https://www.amazon.com.mx/dp/ asin
datos2=t(datos)
datos2=cbind(asins,datos2)

col=c("marketplace","asinID","sku", "link", "precio_venta", "precio_lista", "reviews", "estrellas", "seller", "fecha")
datos2 <- as.data.frame(datos2)
datos2 <- mutate(datos2, Fecha = Sys.Date())
datos2 <- mutate(datos2, amazon)
colnames(datos2) = col


write.table(datos2, file= "tabla_prod_amazon.csv", sep = ";",  append= TRUE, row.names = FALSE, col.names = FALSE)


#drive_upload(datos2, path = https://drive.google.com/drive/u/1/folders/1THl3FsVICCTH137UL0xXyhVYlAVy7Dld)
