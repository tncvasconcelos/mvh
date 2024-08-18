# mvh
R package for assembling and organizing virtual herbaria



metadata <- search_specimen_metadata(taxon_name = "Vaccinium", coordinates= c(42.28, -83.74), limit=8)
download_specimen_images(metadata,
                         dir_name="Vaccinium_in_AnnArbor_example/specimens",
                         result_file_name="Vaccinium_in_AnnArbor_example/result_download")



metadata <- search_specimen_metadata(taxon_name = "Myrcia splendens", limit=100)
pdf("plots_for_mvh_ms.pdf", height=5, width=10)
par(mfrow=c(1,2))
plot_specimens_by_institution(metadata)
plot_specimens_by_country(metadata)
dev.off()
