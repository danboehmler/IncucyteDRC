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
#' test_df <- importPlatemapXML(system.file(file='extdata/example.PlateMap', package='IncucyteDRC'))
#' head(test_df)
importPlatemapXML <- function(filepath) {
    message(sprintf("Importing platemap xml from %s",filepath))
    platemap.xml <- XML::xmlTreeParse(filepath)

    #import the XML file
    xml.top <- XML::xmlRoot(platemap.xml[[1]])
    #reference the wellstore tree
    wellstore <- xml.top[[2]][[1]]
    #set up the output dataframe
    platemap.df <- data.frame()
    #iterate through wells
    for (i in 1:length(wellstore)) {
        #print(i)
        w <- wellstore[i]
        this.row <- XML::xmlGetAttr(w[[1]],'row') #use xmlGetAttr to pull out relevant attributes for each well
        this.col <- XML::xmlGetAttr(w[[1]],'col')
        #default values
        this.samptype <- 'B'
        this.sampleid <- '-'
        this.sampleconc <- NA
        this.sampleconcunits <- NA
        this.growthcondition <- NA
        this.celltype <- NA
        this.celltype_passage <- NA
        this.celltype_seedingdensity <- NA

        if (XML::xmlSize(w[[1]]) > 0) { #blanks don't have the rest of the xml tree, so detect this using xmlSize function

            this.samptype <- 'C'
            items <- w[[1]][[1]]
            for (k in 1:length(items)) {
                j<- items[[k]]
                if (XML::xmlGetAttr(j, 'type') == 'Compound') {
                    this.samptype <- 'S'
                    this.sampleid <- XML::xmlGetAttr(j[[1]], 'description')
                    this.sampleconc <- XML::xmlGetAttr(j, 'concentration')
                    this.sampleconcunits <- XML::xmlGetAttr(j, 'concentrationUnits')
                } else if (XML::xmlGetAttr(j, 'type') == 'GrowthCondition') {
                    this.growthcondition <- XML::xmlGetAttr(j[[1]], 'description')
                }  else if (XML::xmlGetAttr(j, 'type') == 'CellType') {
                    this.celltype <- XML::xmlGetAttr(j[[1]], 'description')
                    this.celltype_passage <- XML::xmlGetAttr(j, 'passage')
                    this.celltype_seedingdensity <- XML::xmlGetAttr(j, 'seedingDensity')
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

    message('Plate map import successful!')
    return (platemap.df)

}
