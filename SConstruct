# Copyright (c) 2016 Alex Kramer <kramer.alex.kramer@gmail.com>
# See the LICENSE.txt file at the top-level directory of this distribution.

import os

source_dir =  os.path.abspath("./src") + "/"
build_dir = os.path.abspath("./build") + "/"

env = DefaultEnvironment(ENV = os.environ, TOOLS = ['default', "gfortran"])

IEEE_flags = "-fno-unsafe-math-optimizations -frounding-math -fsignaling-nans "
debug_flags = "-Og -g3 -Wall -Wextra -Wconversion -Wunused-parameter " + \
    "-pedantic -std=f2008 -fcheck=all -fbacktrace "
general_flags = "-frecursive "
openmp_flags = "-fopenmp "
prod_flags = "-O3 -march=native "

flags = general_flags + IEEE_flags + openmp_flags + prod_flags

env.Replace(F90FLAGS = flags)
env.Replace(LINKFLAGS = flags)
env.Replace(FORTRANMODDIRPREFIX = "-J")
env.Replace(FORTRANMODDIR = build_dir)

Export("env")

SConscript(source_dir+"SConscript", variant_dir=build_dir, duplicate=1)

# For whatever reason, we can't use duplicate=0 and have *.mod files in the
# build directory. But, if we duplicate the source tree into the build
# directory SCons doesn't automatically clean the source files, so we have to
# manually define the entire build directory as a cleaning target.
Clean(".", build_dir)
