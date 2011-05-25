
# Makefile for Lumen documentation
#
# Chip Richards, NiEstu, Phoenix AZ, Spring 2010

# List of doc files
PAGES = \
	index \
	demo-manual \
	install \
	old-intro \
	rationale

NARRS = \
	narrative-colors \
	narrative-demos \
	narrative-lumen \
	narrative-lumen-events \
	narrative-lumen-image \
	narrative-lumen-misc \
	narrative-sgi_simple \
	narrative-spinner \
	narrative-texture \
	narrative-text1 \
	narrative-text2 \
	narrative-multi \
	narrative-nehe

FILES = ${PAGES} ${NARRS}

MDS   = $(addsuffix .markdown, ${FILES})
HTMLS = $(addsuffix .html,     ${FILES})

# Implicit rules
%.html : %.markdown
	./make-html $<

# The top-level target
all: ${HTMLS}

# Cleanup targets
clean:
	rm *.html

realclean: clean
	rm *~
