all: slides.html

slides.html: template.html slides.md mkSlides.hs
	./mkSlides.hs
