
require(XML)

# Parses a dbGaP variable report in XML format, and writes a tab-delimited
# format suitable for viewing in Excel or re-import into R for analysis
# input: 1) inFile: path to file (character)
# output: returns no value. Side effect of writing tab-delimited format to 
# file in the same directory, with filename <inputFile>.txt
#
# Example view of dbGaP xml file 
# ftp://ftp.ncbi.nlm.nih.gov/dbgap/studies/phs000775/phs000775.v1.p1/pheno_variable_summaries/phs000775.v1.pht003945.v1.p1.Molecular_Genetics_Schizophrenia_Subject_Phenotypes.var_report.xml
# see also the dbgap_xml2table_example folder in the Rutils/ github repo.
#
# Example use of script:
# xml2table("myFile.xml") 
# results in writing of results to "myFile.xml.txt"
# 
# Output columns:
# Variable ID ; 	Name ; 	Variable Description; 	Type; 	
# Valid Values Cases;  	Valid Values Controls; Valid Values Others ; 
# Invalid Values	Nulls	Count
# Most Frequently Occurring Valid Values (Counts)	Variable Comment
#
# Author Shraddha Pai
dbgap_xml2table <- function(inFile) {   
	outFile	<- sprintf("%s.txt",inFile)

	cat("* Read XML into list ...\n")
    print(system.time(dat     <- xmlToList(inFile)))
	cat("\n")
    m       <- sum(names(dat)=="variable") # numvariables
	cat(sprintf("   %i variables found\n", m))

    # variable ID, name, description, type, 
    # valid-value-case valid-value-control valid-other 
	# invalid-values **** This one is computed as (n-sum(valid))
	# nulls count
    # most-frequent-counts
    # variable-comment
    n       <- 12
    
    # does all the work of parsing variable-level data
    .parseRec <- function(x) {
        id		<- x$.attrs["id"]
        name	<- x$.attrs["var_name"]
		desc	<- x$description
		type	<- x$.attrs["calculated_type"]
		# valid values
		vcase	<- "0"; vctrl <- "0"; vother <- "0";
		tmp		<- x$total$subject_profile$case_control
		if ("case" %in% names(tmp)){
			vcase	<-tmp$case
		}
		if ("control" %in% names(tmp)){
			vctrl	<-tmp$control
		}
		if ("other" %in% names(tmp)){
			vother	<-tmp$other
		}
		# tallies
		nulls	<- x$total$stats$stat["nulls"]
		count	<- x$total$stats$stat["n"]
		invalid	<- as.integer(count)-(as.integer(vcase)+as.integer(vctrl)
							 +as.integer(vother))
		# commonly occurring values
		tmp		<- x$total$stats
		idx		<- which(names(tmp)=="example")
		if (length(idx)<1) str <- ""
		else {
		str		<- lapply(tmp[idx], function(y) {
					z <- sprintf("%s (%s)", y$text, y$.attrs["count"])
		})
			str <- paste(unlist(str),sep=";",collapse=";")
		}
		if ("comment" %in% names(x)) {
			comm	<- gsub("\n",";",x$comment)
			comm	<- gsub("\t"," ",comm)
		} else {
			comm	<- ""
		}

		return(unlist(c(id,name,desc,type,vcase,vctrl,vother,invalid,nulls,
					  count,str,comm)))
    }
    	
		non    <- which(names(dat) == ".attrs")
        print(system.time(out     <- lapply(dat[-non], .parseRec)))
		outm	<- matrix(unlist(out),byrow=TRUE, nrow=length(out))
		colnames(outm)	<- c("Variable ID","Name","Variable Description",
			"Type","Valid Values Cases", "Valid Values Controls",
			"Valid Values Others", "Invalid Values","Nulls","Count",
			"Most Frequently Occurring Valid Values (Counts)",
			"Variable Comment")
		cat("\n")
		
		cat("* Writing to file \n")
		tmp	<- dat$.attrs
		cat(sprintf("# %s: %s: Variable report for %s\n", tmp["study_id"],
			tmp["name"], tmp["dataset_id"]), file=outFile)
		suppressWarnings(write.table(outm,file=outFile,sep="\t",col.names=TRUE,
					row.names=FALSE, quote=FALSE,append=TRUE))
}
