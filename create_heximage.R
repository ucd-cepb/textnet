
library(hexSticker)
library(showtext)
library(ggplot2)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Roboto mono")
## Automatically use showtext to render text for future devices
showtext_auto()

# used this https://demos.explosion.ai/displacy
# used this sentence: "The Olcese GSA has undertaken significant technical work, in coordination with the other Basin GSAs, to implement the corrective actions recommended in the Incomplete Determination, as highlighted below and summarized in Section 1 of the Olcese GSP."
img <- "img/heximage.png"

sticker(img, 
        package="textNet", 
        h_color="#ece7f2", h_fill="#2b8cbe",
        p_size=8, p_color = "white",
        s_x=1, s_y=.82 ,
        s_width=.65,  
        p_family = "Roboto mono", filename="img/hexsticker.png")
