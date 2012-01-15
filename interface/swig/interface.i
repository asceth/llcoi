%module llcoi
%feature("export");
%feature("intern_function","1");
%insert("lisphead") %{
(in-package :ogre)

(define-foreign-library libllcoi
    (:unix (:or "libllcoi.so.3" "libllcoi.so"))
    (t (:default "libllcoi")))

(defun load-foreign-libraries ()
  (use-foreign-library libllcoi)
  (format t "~&[ogre] foreign library libllcoi loaded~%"))


(load-foreign-libraries)

(in-package :ogre)

%}
%insert("swiglisp") %{
#+sbcl(sb-ext::set-floating-point-modes :traps nil)
%}

%include "../h/ogre_enums.h"
%include "../h/ogre_interface.h"
%include "../h/ogre_root.h"
%include "../h/ois_interface.h"




