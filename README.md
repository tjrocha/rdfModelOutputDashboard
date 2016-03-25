# RDF Model Output Explorer
This generates a dashboard to view, analyze, and query data from RiverWare *.rdf model 
outputs. This is primarily meant to support U.S. Bureau of Reclamation modeling and 
analysis efforts with the 24-Month Study, Mid-Term Operations Model and Colorado River 
Simulation System models. Although the stated purpose is to support USBR, the tool will 
be developed to be as generic as possible so as to enable other users to use is so long 
as a RiverWare *.rdf file is provided.

The dashboard uses the following R libraries below and is being developed in [RStudio](https://www.rstudio.com/).
<BR>[shiny](http://shiny.rstudio.com/)
<BR>[shinydashboard](https://rstudio.github.io/shinydashboard/index.html)
<BR>[DT](https://rstudio.github.io/DT/)
<BR>[xts](https://cran.r-project.org/web/packages/xts/index.html)
<BR>[zoo](https://cran.r-project.org/web/packages/zoo/index.html)
<BR>[RWDataPlot](https://github.com/rabutler/RWDataPlot)

An updated test version may be found at the link below; whenever I remember to publish my latest version of the code that is...
<BR>[shinyapps.io](https://jrocha.shinyapps.io/rdfModelOutputDashboard/)

# Instructions
<BR>1. Select a model from the top-most drop down box or click on 'Choose File' to select and upload an RDF file. The current file size limit is 30MB while in the beta testing phase.  
<BR>2. Once a model has been selected another drop down box will be populated with the slot names present in the selected model RDF file. You may click on the drop-down box to specify a slot to select or you may type in partial names to filter the available slots in the list. The crop-down box may take a few seconds to generate. 
<BR>3. Once a model and a slot has been selected, you may now view charts and data in their respective sections via the sidebar. You may change your selections at any time.
