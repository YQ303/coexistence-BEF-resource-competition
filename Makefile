.PHONY: prerequisites setup run report graph status clean

prerequisites:
	./install-prerequisites-macos.sh

setup:
	Rscript setup.R

run:
	Rscript run.R

graph:
	Rscript -e 'targets::tar_visnetwork()'

report:
	Rscript -e 'targets::tar_make(report)'

status:
	Rscript -e 'targets::tar_meta(fields = c(name, progress, seconds))'

clean:
	Rscript -e 'targets::tar_destroy(destroy = "all")'
