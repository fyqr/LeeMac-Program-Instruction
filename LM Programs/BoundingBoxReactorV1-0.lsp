;;--------------=={ BoundingBox Reactor NEW }==---------------;;
;;                                                            ;;
;;  Allows the user to create a new reactor owner group for   ;;
;;  which a bounding polyline will be updated following       ;;
;;  modification of an owner within the group                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:bbrn ( / *error* owners lst )
  (vl-load-com)
  ;; © Lee Mac 2010

  (defun *error* ( msg )
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (setq owners (_GetAllOwners :vlr-object-reactor))
  
  (if (setq lst
        (vl-remove-if
          (function
            (lambda ( object )
              (vl-position object owners)
            )
          )
          (LM:SS->VLA (ssget))
        )
      )
    (vlr-object-reactor lst
      (cdr
        (assoc 5
          (entget
            (LWPoly
              (LM:BBox->List
                (LM:ListBoundingBox lst)
              )
              1
            )
          )
        )
      )
      (list (cons :vlr-modified 'BB:WasModified))
    )
  )

  (princ)
)

;;------------=={ BoundingBox Reactor REMOVE }==--------------;;
;;                                                            ;;
;;  Allows the user to remove objects from an existing        ;;
;;  reactor owner group, or remove all groups                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:bbrr ( / *error* owners gr code data hLst msg e o nm )
  (vl-load-com)
  ;; © Lee Mac 2010

  (defun *error* ( msg )
    (and nm (setvar 'NOMUTT nm))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (setq owners (_GetAllOwners :vlr-object-reactor))

  (mapcar 'vlr-remove (mapcar 'car owners))

  (princ (setq msg "\nSelect Group to Remove Objects from <All> : "))

  (while
    (progn
      (setq gr (grread 't 15 2) code (car gr) data (cadr gr))

      (cond
        (
          (= 5 code)

          (if (and
                (setq e (car (nentselp data)))
                (setq o (vlax-ename->vla-object e))
                (setq group
                  (cdar
                    (vl-member-if
                      (function
                        (lambda ( x )
                          (vl-position o (cdr x))
                        )
                      )
                      owners
                    )
                  )
                )
              )

            (HighlightObjects (setq hLst group) T)

            (if hLst (progn (HighlightObjects hLst nil) (setq hLst nil)))
          )
          t
        )
        (
          (= 3 code)

          (if (and
                (setq e (car (nentselp data)))
                (setq o (vlax-ename->vla-object e))
                (setq group
                  (car
                    (vl-member-if
                      (function
                        (lambda ( x )
                          (vl-position o (cdr x))
                        )
                      )
                      owners
                    )
                  )
                )
              )
            (if (progn
                  (if hLst (progn (HighlightObjects hLst nil) (setq hLst nil)))
                  (setq nm (getvar 'NOMUTT))
                  (setvar 'NOMUTT 1)
                  (princ "\nSelect Objects to Remove <All> : ")
                  (setq lst (LM:SS->VLA (ssget)))
                  (setvar 'NOMUTT nm)
                  lst
                )
              (progn
                (mapcar
                  (function
                    (lambda ( owner )
                      (if (vl-position owner (cdr group))
                        (vlr-owner-remove (car group) owner)
                      )
                    )
                  )
                  lst
                )

                (BB:WasModified (car lst) (car group) nil)
                
                (princ "\n<< Objects Removed from Group >>")
                nil
              )
              (progn
                (setq owners (vl-remove group owners))
                (vlr-remove (car group))
                
                (if hLst (progn (HighlightObjects hLst nil) (setq hLst nil)))
                (princ "\n<< Group BoundingBox Associativity Removed >>")
                nil
              )
            )
            (princ (strcat "\n** No Group Found **" msg))
          )
        )
        (
          (or (and (= 2 code) (member data '(13 32))) (= 25 code))

          (setq owners nil)
          (princ "\n<< BoundingBox Associativity Removed from All Objects >>")
         
          (if hLst (progn (HighlightObjects hLst nil) (setq hLst nil)))
        )
        ( (princ (strcat "\n** Invalid KeyPress **" msg)) )
      )
    )
  )

  (mapcar 'vlr-add (mapcar 'car owners))
  (princ)
)

;;-------------=={ BoundingBox Reactor ADD }==----------------;;
;;                                                            ;;
;;  Allows the user to add objects to an existing reactor     ;;
;;  owner group                                               ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:bbra ( / *error* owners msg gr code data e o group hLst lst nm )
  (vl-load-com)
  ;; © Lee Mac 2010

  (defun *error* ( msg )
    (and nm (setvar 'NOMUTT nm))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (setq owners (_GetAllOwners :vlr-object-reactor))

  (mapcar 'vlr-remove (mapcar 'car owners))

  (princ (setq msg "\nSelect Group to Add Objects to: "))

  (while
    (progn
      (setq gr (grread 't 15 2) code (car gr) data (cadr gr))

      (cond
        (
          (= 5 code)

          (if (and
                (setq e (car (nentselp data)))
                (setq o (vlax-ename->vla-object e))
                (setq group
                  (cdar
                    (vl-member-if
                      (function
                        (lambda ( x )
                          (vl-position o (cdr x))
                        )
                      )
                      owners
                    )
                  )
                )
              )

            (HighlightObjects (setq hLst group) T)

            (if hLst (progn (HighlightObjects hLst nil) (setq hLst nil)))
          )
          t
        )
        (
          (= 3 code)

          (if (and
                (setq e (car (nentselp data)))
                (setq o (vlax-ename->vla-object e))
                (setq group
                  (caar
                    (vl-member-if
                      (function
                        (lambda ( x )
                          (vl-position o (cdr x))
                        )
                      )
                      owners
                    )
                  )
                )
              )

            (if (progn
                  (setq nm (getvar 'NOMUTT))
                  (setvar 'NOMUTT 1)
                  (princ "\nSelect Objects to Add: ")
                  (setq lst (LM:SS->VLA (ssget)))
                  (setvar 'NOMUTT nm)
                  lst
                )
              (progn
                (mapcar
                  (function
                    (lambda ( owner ) (vlr-owner-add group owner))
                  )
                  lst
                )

                (BB:WasModified (car lst) group nil)
                
                (if hLst (progn (HighlightObjects hLst nil) (setq hLst nil)))
                (princ "\n<< Objects added to group >>")
                nil
              )
            )
            (princ (strcat "\n** No Group Found **" msg))
          )
        )
      )
    )
  )

  (mapcar 'vlr-add (mapcar 'car owners))
  (princ)
)

;;--------------------=={ Was Modified }==--------------------;;
;;                                                            ;;
;;  Callback function for the BoundingBox Reactor to update   ;;
;;  the bounding polyline of the calling owner group          ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  owner   - object to which the modification event applies  ;;
;;  reactor - reactor object to which the owner belongs       ;;
;;  args    - reactor data associated with the event          ;;
;;------------------------------------------------------------;;

(defun BB:WasModified ( owner reactor args / h )
  (vl-load-com)

  (if (and (setq h (vlr-data reactor))
           (setq h (handent h))
           (entget h)
      )
    (entdel h)
  )

  (
    (lambda ( owners )
      (if owners
        (vlr-data-set reactor
          (cdr
            (assoc 5
              (entget
                (LWPoly
                  (LM:BBox->List
                    (LM:ListBoundingBox owners)
                  )
                  1
                )
              )
            )
          )
        )
        (vlr-remove reactor)
      )
    )         
    (vl-remove-if
      (function
        (lambda ( owner )
          (if (vlax-erased-p owner)
            (vlr-owner-remove reactor owner)
          )
        )
      )
      (vlr-owners reactor)
    )
  )

  (princ)
)

;;-----------------=={ Highlight Objects }==------------------;;
;;                                                            ;;
;;  Highlights or Unhighlights a list of supplied VLA Objects ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  lst - list of VLA Objects                                 ;;
;;  h   - Boolean Flag: (T=Highlight, nil=Unhighlight)        ;;
;;------------------------------------------------------------;;

(defun HighLightObjects ( lst h )
  (
    (lambda ( x )
      (mapcar
        (function
          (lambda ( object ) (vla-highlight object x))
        )
        lst
      )
    )
    (if h :vlax-true :vlax-false)
  )
)

;;----------------=={ Get Reactor Object }==------------------;;
;;                                                            ;;
;;  Returns the reactor object of the specified type          ;;
;;  associated with the application data supplied             ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  data - reactor application data                           ;;
;;  typ  - Type of reactor to query, eg :vlr-object-reactor   ;;
;;------------------------------------------------------------;;
;;  Returns: VLA Reactor Object, else nil                     ;;
;;------------------------------------------------------------;;

(defun _GetReactorObject ( data typ )
  (vl-some
    (function
      (lambda ( reactor )
        (if (eq data (vlr-data reactor)) reactor)
      )
    )
    (cdar (vlr-reactors typ))
  )
)

;;------------------=={ Get All Owners }==--------------------;;
;;                                                            ;;
;;  Returns a list of reactors of the specified type and      ;;
;;  owners for each reactor                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  typ - Type of reactor to query, eg :vlr-object-reactor    ;;
;;------------------------------------------------------------;;
;;  Returns: ((<reactor> <owner> ... <owner>) (<reactor> ..)) ;;
;;------------------------------------------------------------;;

(defun _GetAllOwners ( typ )
  (mapcar
    (function
      (lambda ( reactor )
        (cons reactor (vlr-owners reactor))
      )
    )
    (cdar (vlr-reactors typ))
  )
)

;;---------------------=={ LWPolyline }==---------------------;;
;;                                                            ;;
;;  Creates an LWPolyline with vertices at each point in the  ;;
;;  supplied list                                             ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  lst - list of vertices for polyline                       ;;
;;  cls - Closed Bit Flag: 1=Closed, 0=Open                   ;;
;;------------------------------------------------------------;;
;;  Returns:  entity name of created LWPolyline, else nil     ;;
;;------------------------------------------------------------;;

(defun LWPoly ( lst cls )
  (entmakex
    (append
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 90 (length lst))
        (cons 70 cls)
      )
      (mapcar '(lambda ( p ) (cons 10 p)) lst)
    )
  )
)

;;-----------------=={ SelectionSet -> VLA }==----------------;;
;;                                                            ;;
;;  Converts a SelectionSet to a list of VLA Objects          ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ss - Valid SelectionSet (Pickset)                         ;;
;;------------------------------------------------------------;;
;;  Returns:  List of VLA Objects                             ;;
;;------------------------------------------------------------;;

(defun LM:ss->vla ( ss )
  ;; © Lee Mac 2010
  (if ss
    (
      (lambda ( i / e l )
        (while (setq e (ssname ss (setq i (1+ i))))
          (setq l (cons (vlax-ename->vla-object e) l))
        )
        l
      )
      -1
    )
  )
)

;;------------------=={ List BoundingBox }==------------------;;
;;                                                            ;;
;;  Returns the coordinates of a rectangle framing all        ;;
;;  objects in a supplied list                                ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  lst - list of VLA-Objects to process                      ;;
;;------------------------------------------------------------;;
;;  Returns:  coordinates of rectangle framing objects        ;;
;;------------------------------------------------------------;;

(defun LM:ListBoundingBox ( lst / ll ur bb )
  ;; © Lee Mac 2010

  (foreach obj lst (vla-getBoundingBox obj 'll 'ur)
    (setq bb (cons (vlax-safearray->list ur)
                   (cons (vlax-safearray->list ll) bb))
    )
  )

  (mapcar
    (function
      (lambda ( operation )
        (apply (function mapcar) (cons operation bb))
      )
    )
   '(min max)
  )
)

;;----------------=={ BoundingBox -> List }==-----------------;;
;;                                                            ;;
;;  Returns the coordinates of a rectangle from the           ;;
;;  coordinates of the lower-left and upper-right corners     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  bbox - list of lower-left and upper-right coordinates     ;;
;;------------------------------------------------------------;;
;;  Returns:  coordinates of rectangle                        ;;
;;------------------------------------------------------------;;

(defun LM:BBox->List ( bbox )
  (mapcar
    (function
      (lambda ( funcs )
        (mapcar
          (function
            (lambda ( func ) ((eval func) bbox))
          )
          funcs
        )
      )
    )
    '((caar  cadar)  (caadr cadar) (caadr cadadr) (caar  cadadr))
  )
)