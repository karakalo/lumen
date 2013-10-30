objects     = freetype_wrap.o 
lib         = libfreetype_c.a
library_dir = ../../library

lib : $(library_dir)/$(lib)

freetype_wrap.o : freetype_wrap.cxx
	${CC} -c -g -Wall -pedantic -W `freetype-config --cflags` -lpthread -o freetype_wrap.o freetype_wrap.cxx

$(lib) : $(objects)
	ar rcs $(lib) $(objects)

$(library_dir)/$(lib) : $(lib)
	cp -p  $(lib) $(library_dir)

clean :
	rm -f $(library_dir)/$(lib) $(lib) $(objects)

