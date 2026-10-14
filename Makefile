FC = gfortran
FFLAGS ?= -O2 -std=f2008 -Wall -Wextra

.PHONY: all test clean

all: life

life: life.f90
	$(FC) $(FFLAGS) -o $@ $<

test: life
	./life --self-test

clean:
	$(RM) life *.mod
