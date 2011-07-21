#! /bin/bash
swig -cffi -swig-lisp -module ogre interface.i
cp ogre.lisp ../../test/dist/bin/
cp ogre.asd ../../test/dist/bin/

