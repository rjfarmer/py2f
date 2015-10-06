

PYTHON3=python3.4
PYTHON2=python2.7

PYTHON=$(PYTHON2)
CC=gcc
FCC=gfortran

FLAGS=$(shell $(PYTHON)-config --cflags)
LIBS=$(shell $(PYTHON)-config --ldflags)

BIN=py2f

SRC_C=py2f.c
SRC_H=py2f.h
SRC_F=py2f.f90

build_c : $(SRC_C) $(SRC_H)
	$(CC) $(FLAGS) $(LIBS) -c $(SRC_C)

build_f : $(SRC_F)
	$(FCC) $(FLAGS) $(LIBS) -c $(SRC_F)

all :  $(SRC_C) $(SRC_H) $(SRC_F)
	$(FCC) $(FLAGS) $(LIBS) -o $(BIN) $(SRC_C) $(SRC_H) $(SRC_F)
	

.PHONY : clean

clean:
	-@rm -f *.o *.mod *.gch *~
