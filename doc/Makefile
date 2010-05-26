
# Makefile for Lumen documentation
#
# Chip Richards, NiEstu, Phoenix AZ, Spring 2010

# List of doc files
PAGES = \
	index \
	old-intro

NARRS = \
	narrative-lumen \
	narrative-colors

RATS = \
	rationale

FILES = ${PAGES} ${NARRS} ${RATS}

MDS   = $(addsuffix .markdown, ${FILES})
HTMLS = $(addsuffix .html,     ${FILES})

# Implicit rules
%.html : %.markdown
	./make-html $<

# The top-level target
all: narratives

# Build targets
narratives: ${HTMLS}

# Cleanup targets
clean:
	rm *.html

realclean: clean
	rm *~
