

PYTHON3=python3.4
PYTHON2=python2.7

PYTHON=$(PYTHON2)

FLAGS=$(shell $(PYTHON)-config --cflags)
LIBS=$(shell $(PYTHON)-config --ldflags)

all:
	gcc $(FLAGS) -c py2f.h
	gcc $(FLAGS) -c py2f.c
	gfortran $(FLAGS) -c py2f.f90
	gfortran $(FLAGS) $(LIBS) -o py2f py2f.f90 py2f.c
