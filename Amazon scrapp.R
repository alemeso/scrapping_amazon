#scraping amazon
rm( list= ls() )

library(rvest)
library(dplyr)
library(tidyverse)

url<- "https://www.amazon.com.mx/s?k=hausklein&__mk_es_MX=%C3%85M%C3%85%C5%BD%C3%95%C3%91&crid=8EJJUTAGK156&sprefix=hausklein%2Caps%2C235&ref=nb_sb_noss_1"
setwd("C:/Users/ameso/OneDrive/Escritorio/Scraping/csvs/amz mx")

#formato pagina

pagina <- read_html(url)

nombre <- pagina %>% html_nodes(".a-size-base-plus") %>% html_text()

asin <- pagina %>% html_nodes("div") %>% html_attr("data-asin")
asin = asin[!is.na(asin)]
asin = asin[asin!=""]
asin = unique(asin)

link=paste0("https://www.amazon.com.mx/dp/",asin)
class(link)
obtenerdatos <- function(link){
  #as = "B08Y95WNF5" #con desc
  #as = "B08F9JMYGM"  #sin desc
  #as = "B097JW67YK"  #con promo
  
  #link=paste0("https://www.amazon.com.mx/dp/",as)
  pagina_producto <- read_html(link)
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
  
  #Sys.sleep(2)

  
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
  data=rbind(link, precio, tachado, promo, reviews, estrellas, seller)
  return(data)
  
  
}

start_time <- Sys.time()   
datos = sapply(link, FUN = obtenerdatos, USE.NAMES = FALSE)

datos2=datos
## data-asin  https://www.amazon.com.mx/dp/ asin
datos2=t(datos)
datos2=cbind(asin,datos2)

col=c("asin", "link", "precio_venta", "precio_lista", "tag_promo", "reviews", "estrellas", "seller", "fecha")
datos2 <- as.data.frame(datos2)
datos2 <- mutate(datos2, Fecha = Sys.Date())
datos2 <- mutate(datos2, amazon)
colnames(datos2) = col

write.csv(datos2,  paste0("amz mx 2 ",Sys.Date(), ".csv"), row.names = FALSE)
end_time <- Sys.time()

end_time-start_time
