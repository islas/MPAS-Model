.SUFFIXES: .f90 .cpp .o
PY_INC     := $(shell python3-config --includes)
PY_CFLAGS  := $(shell python3-config --cflags)
PY_LDFLAGS := $(shell python3-config --ldflags)

OBJS = EmbeddedModules.cpp.o
      #  EmbeddedInterpreter.cpp.o \
      #  EmbeddedInterpreter.f90.o \
      #  EmbeddedModules.cpp.o \
      #  f_c_helpers.f90.o

all: mpaspyio

mpaspyio: $(OBJS)
	ar -ru libmpaspyio.a $(OBJS)

# EmbeddedInterpreter.f90.o: f_c_helpers.f90.o

# EmbeddedInterpreter.cpp.o:

clean:
	$(RM) *.o *.mod libmpaspyio.a
	
%.f90.o: %.f90
	$(RM) $@ $*.mod
	$(FC) $(CPPFLAGS) $(FFLAGS) -c $< -o $*.f90.o

%.cpp.o: %.cpp
	$(RM) $@
	$(CXX) $(CFLAGS) -I$(PYBIND11) $(PY_INC) $(PY_CFLAGS) -c $< -o $@