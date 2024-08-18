# mvh (my virtual herbarium)

This is an R package for assembling and organizing virtual herbaria.

The example below is of a pipeline to search and download up to eight specimens (“limit=8”) of the blueberry genus *Vaccinium* (Ericaceae) from the Ann Arbor (MI, USA) area (“coordinates = c(42.28, -83.74)”). 
```r
metadata <- search_specimen_metadata(taxon_name = "Vaccinium", coordinates= c(42.28, -83.74), limit=8)
download_specimen_images(metadata,
                         dir_name="Vaccinium_in_AnnArbor_example/specimens",
                         result_file_name="Vaccinium_in_AnnArbor_example/result_download")
```

The example below shows how to search up to 100 specimens (limit=”100”) of the widespread species *Myrcia splendens* (Myrtaceae) and plot the number of specimens per institution and country.
```r
metadata <- search_specimen_metadata(taxon_name = "Myrcia splendens", limit=100)
pdf("plots_for_mvh_ms.pdf", height=5, width=10)
par(mfrow=c(1,2))
plot_specimens_by_institution(metadata)
plot_specimens_by_country(metadata)
dev.off()
```
