#' Visualize texture triangles according to de Bakker & Schelling
#'
#' Create texture ternary plot in the ggplot environment
#' @param textuurdata data.frame of texture data. Must contain columns 'clay', 'silt', 'sand'
#' @param type type of texture ternary triangle. Currently supported: 'eolisch'
#'
#' @import ggplot2
#' @import ggtern
#' @import data.table
#' @importFrom plyr ddply
#'
#' @examples
#' plot_textuurdriehoek(textuurdata = textuur, type = "eolisch")
#type
#type <- "eolisch"
#type <- "niet_eolisch"

#copy bodemdata sbb zuid-limburg
#textuur <- data.table::fread("C:/Users/Maarten van Doorn/Desktop/Projecten/1984.N.23 - natuurontwikkeling Zuid-Limburg/data/analyses/ronde1_voorlopig.csv")

#tidy
#textuur <- textuur[, c("location", "clay_liab (%)", "silt_liab (%)", "sand_liab (%)")]
#colnames(textuur) <- gsub("_liab|%|\\)|\\(| ", "", colnames(textuur))
#setnames(textuur, old = c("clay", "silt", "sand"),
#         new = c("Clay", "Silt", "Sand"))

#adjust so its 100
#textuur[, Sand := 100 - (Silt + Clay)]

#function
plot_textuurdriehoek <- function(textuurdata, type){
  #copy
  textuur <- copy(textuurdata)

  #set theme
  ggplot2::theme_set(new = theme_bw())

  #if type = eolisch
  if(type == "eolisch"){
    #boundaries
    boundaries <- rbind(
      data.frame(
        `Sand` = c(0, 82.5, 27, 15, 0),
        `Clay` = c(100, 17.5, 17.5, 22, 30),
        `Silt` = c(0,0, 54.5, 63, 70),
        Label = c("Kleiige leem")
      ),
      data.frame(
        `Sand` = c(0, 15, 15, 0),
        `Clay` = c(0, 0, 22, 30),
        `Silt` = c(100, 85, 63, 70),
        `Label` = "Siltige leem"
      ),
      data.frame(
        `Sand` = c(82.5, 92, 50, 50, 15, 15, 27),
        `Clay` = c(17.5, 8, 8, 0, 0, 22, 17.5),
        `Silt` = c(0, 0, 42, 50, 85, 63, 54.5),
        `Label` = "Zandige leem"
      ),
      data.frame(
        `Sand` = c(100, 92, 90, 90),
        `Clay` = c(0, 8, 8, 0),
        `Silt` = c(0, 0, 2, 10),
        `Label` = c("L.\narm")
      ),
      data.frame(
        `Sand` = c(90, 90, 82.5, 82.5),
        `Clay` = c(8, 0, 0, 8),
        `Silt`= c(2, 10, 17.5, 9.5),
        `Label` = "Zwak\nlemig"
      ),
      data.frame(
        `Sand` = c(82.5, 82.5, 67.5, 67.5),
        `Clay` = c(8, 0, 0, 8),
        `Silt` = c(9.5, 17.5, 32.5, 24.5),
        `Label` = "Sterk\nlemig"
      ),
      data.frame(
        `Sand` = c(67.5, 67.5, 50, 50),
        `Clay` = c(0, 8, 8, 0),
        `Silt` = c(32.5, 24.5, 42, 50),
        `Label` = "Zeer sterk\nlemig"
      )
    )

    #set labels
    dfLabel <- plyr::ddply(boundaries, 'Label', function(df) {
      label <- as.character(df$Label[1])
      df$Angle <- switch(label, "Kleiige leem" = 0, 0)
      colMeans(df[setdiff(colnames(df), "Label")])
    })

    #cols
    cols <- alpha(RColorBrewer::brewer.pal(7, 'Set2'), 0.2)

  } else if(type == "niet_eolisch"){
    #boundaries
    boundaries <- rbind(
      data.frame(
        `Sand` = c(0, 50, 0),
        `Clay` = c(100, 50, 50),
        `Silt` = c(0, 0, 50),
        Label = c("Zeer zware klei")
      ),
      data.frame(
        `Sand` = c(65, 50, 0, 0),
        `Clay` = c(35, 50, 50, 35),
        `Silt` = c(0, 0, 50, 65),
        `Label` = "Matig zware klei"
      ),
      data.frame(
        `Sand` = c(65, 0, 0, 75),
        `Clay` = c(35, 35, 25, 25),
        `Silt` = c(0, 65, 75, 0),
        `Label` = "Lichte klei"
      ),
      data.frame(
        `Sand` = c(75, 0, 0, 82.5),
        `Clay` = c(25, 25, 17.5, 17.5),
        `Silt` = c(0, 75, 82.5, 0),
        `Label` = "Zware zavel"
      ),
      data.frame(
        `Sand` = c(0, 82.5, 88, 0),
        `Clay` = c(17.7, 17.5, 12, 12),
        `Silt` = c(82.2, 0, 0, 88),
        Label = "Matig lichte zavel"
      ),
      data.frame(
        `Sand` = c(0, 88, 92, 0),
        `Clay` = c(12, 12, 8 ,8),
        `Silt` = c(88, 0, 0, 92),
        Label = "Zeer lichte zavel"
      ),
      data.frame(
        Sand = c(92, 95, 50, 50),
        Clay = c(8, 5, 5, 8),
        Silt = c(0, 0, 45, 42),
        Label = "Kleiig zand"
      ),
      data.frame(
        Sand = c(100, 95, 50, 50),
        Clay = c(0, 5, 5, 0),
        Silt = c(0, 0, 45, 50),
        Label = "Kleiarm zand"
      ),
      data.frame(
        Sand = c(50, 50, 0, 0),
        Clay = c(8, 5, 5, 8),
        Silt = c(42, 45, 95, 92),
        Label = "Kleiig silt"
      ),
      data.frame(
        Sand = c(50, 50, 0, 0),
        Clay = c(0, 5, 5, 0),
        Silt = c(50, 45, 95, 100),
        Label = "Kleiarm silt"
      )
    )

    #set labels
    dfLabel <- plyr::ddply(boundaries, 'Label', function(df) {
      label <- as.character(df$Label[1])
      df$Angle <- switch(label, "Zeer zware klei" = 0, 0)
      colMeans(df[setdiff(colnames(df), "Label")])
    })

    #cols
    cols <- alpha(RColorBrewer::brewer.pal(8, 'Set2'), 0.2)
    cols <- sample(cols, 11, replace = TRUE)
  }

  #plot
  ggtern(data = textuur,
         mapping = aes(
    x = Sand,
    y = Clay,
    z = Silt)) +
    geom_polygon(data = boundaries,
                 mapping = aes(fill = Label),
                 col = 'black',
                 show.legend = FALSE) +
    theme_clockwise() +
    theme_showarrows() +
    theme_hidetitles() +
    labs(x = "Zand (%)", y = "Klei (%)", z = "Silt (%)", fill = "Classificatie") +
    scale_L_continuous(breaks = seq(0,1,0.1)) +
    scale_R_continuous(breaks = seq(0,1,0.1)) +
    scale_T_continuous(breaks = seq(0,1,0.1)) +
    scale_fill_manual(values = cols) +
    geom_text(data = dfLabel, mapping = aes(label = Label, angle = Angle), size = 2.5) +
    geom_point()
}
#plot_textuurdriehoek(textuurdata = textuur, type = 'niet-eolisch')
