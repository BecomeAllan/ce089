render:
	Rscript -e 'rmarkdown::render_site()'

clean:
	rm -rf cache css docs figures js

all: clean render
