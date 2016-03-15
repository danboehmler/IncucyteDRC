#' importPlatemapXML
#'
#' Imports .Platemap XML files from Incucyte Zoom software
#'
#' @param filepath
#'
#' @return data fame
#' @export
#'
#' @examples
#' 1+1
importPlatemapXML <- function(filepath) {
    message(sprintf("Importing platemap xml from %s",filepath))
    platemap.xml <- xmlTreeParse(filepath)

    #import the XML file
    xml.top <- xmlRoot(platemap.xml[[1]])
    #reference the wellstore tree
    wellstore <- xml.top[[2]][[1]]
    #set up the output dataframe
    platemap.df <- data.frame()
    #iterate through wells
    for (i in 1:length(wellstore)) {
        #print(i)
        w <- wellstore[i]
        this.row <- xmlGetAttr(w[[1]],'row') #use xmlGetAttr to pull out relevant attributes for each well
        this.col <- xmlGetAttr(w[[1]],'col')
        #default values
        this.samptype <- 'B'
        this.sampleid <- '-'
        this.sampleconc <- NA
        this.sampleconcunits <- NA
        this.growthcondition <- NA
        this.celltype <- NA
        this.celltype_passage <- NA
        this.celltype_seedingdensity <- NA

        if (xmlSize(w[[1]]) > 0) { #blanks don't have the rest of the xml tree, so detect this using xmlSize function

            this.samptype <- 'C'
            items <- w[[1]][[1]]
            for (k in 1:length(items)) {
                j<- items[[k]]
                if (xmlGetAttr(j, 'type') == 'Compound') {
                    this.samptype <- 'S'
                    this.sampleid <- xmlGetAttr(j[[1]], 'description')
                    this.sampleconc <- xmlGetAttr(j, 'concentration')
                    this.sampleconcunits <- xmlGetAttr(j, 'concentrationUnits')
                } else if (xmlGetAttr(j, 'type') == 'GrowthCondition') {
                    this.growthcondition <- xmlGetAttr(j[[1]], 'description')
                }  else if (xmlGetAttr(j, 'type') == 'CellType') {
                    this.celltype <- xmlGetAttr(j[[1]], 'description')
                    this.celltype_passage <- xmlGetAttr(j, 'passage')
                    this.celltype_seedingdensity <- xmlGetAttr(j, 'seedingDensity')
                }
            }
        }

        #check for DMSO
        if (grepl ( 'DMSO' , this.sampleid, ignore.case=T  ) ) { this.samptype <- 'C'  }

        #create a dataframe to rbind to the container dataframe
        this.well <- data.frame(row=as.numeric(this.row),
                                col=as.numeric(this.col),
                                sampleid=as.character(this.sampleid),
                                conc=as.numeric(this.sampleconc),
                                samptype=as.character(this.samptype),
                                concunits=as.character(this.sampleconcunits),
                                growthcondition=as.character(this.growthcondition),
                                celltype=as.character(this.celltype),
                                passage=as.character(this.celltype_passage),
                                seedingdesnsity=as.character(this.celltype_seedingdensity),
                                stringsAsFactors=F)

        platemap.df <- rbind(platemap.df,this.well)


    }

    #minor mods to data frame
    platemap.df$row <- platemap.df$row + 1
    platemap.df$col <- platemap.df$col + 1
    platemap.df$wellid <- paste(LETTERS[platemap.df$row], platemap.df$col, sep='')

    print('Plate map import successful!')
    return (platemap.df)

}
