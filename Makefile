

PYTHON3=python3.4
PYTHON2=python2.7

PYTHON=$(PYTHON2)

CC=gcc
FCC=gfortran

EXTRA=-g -Wall

FLAGS=$(shell $(PYTHON)-config --cflags)
LIBS=$(shell $(PYTHON)-config --ldflags) 

BIN=py2f

SRC_C=py2f.c
SRC_H=py2f.h
SRC_F=py2f.f90

TEST=py2f_test.f90
TEST_BIN=py2f_test

build_c : $(SRC_C) $(SRC_H)
	$(CC) $(FLAGS) $(EXTRA) -c $(SRC_C)

build_f : $(SRC_F)
	$(FCC) $(FLAGS) $(EXTRA) -c $(SRC_F)

all :  build_c build_f
	$(FCC) $(FLAGS) $(LIBS) $(EXTRA) -o $(BIN) $(SRC_C) $(SRC_F)

test : build_c build_f $(TEST)
	$(FCC) $(FLAGS) $(LIBS) $(EXTRA) -o $(TEST_BIN) $(TEST) $(SRC_C) $(SRC_F)

.PHONY : all

clean:
	-@rm -f *.o *.mod *.gch *~ $(TEST_BIN) $(BIN)
