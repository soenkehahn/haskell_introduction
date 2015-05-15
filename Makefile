all: slides.html
	ack-grep todo slides.md || true

slides.html: template.html slides.md mkSlides.hs
	# doctoc slides.md
	./mkSlides.hs
