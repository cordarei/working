INCLUDES    = `fltk-config --use-images --cxxflags`\
              `pkg-config --cflags cairo`
LIBRARY     = `fltk-config --use-images --ldstaticflags`\
              `pkg-config --libs cairo`
TARGET      =  driver
.PHONY   : clean install

VPATH = src

${TARGET} : driver.o CairoBox.o 
	g++ -g -Wall $^ ${LIBRARY}  -o $@

CairoBox.o : CairoBox.cpp CairoBox.h
	g++ -g -Wall ${INCLUDES} -c $< -o $@

driver.o : driver.cpp CairoBox.h
	g++ -g -Wall ${INCLUDES} -c $< -o $@

clean:
	rm *.o ${TARGET}

