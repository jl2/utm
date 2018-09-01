;;;; package.lisp
;;
;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-user)
(defpackage :utm.test
  (:use :cl
        :fiveam
        :utm))

(in-package :utm.test)

(def-suite :utm)
(in-suite :utm)

(test lat-lon-to-utm
  (let ((tolerance 0.002)
        (coords (list #(39.9835310406 -105.2536113560 "NAD83" 13 478346.59856 4425960.12177)
                      #(46.8392935284 -115.3348159790 "NAD83" 11 626974.542 5188651.724)
                      #(48.2338208531 19.3716430664 "GRS80" 34 379083.606 5343570.714)
                      #(59.8723979923 105.1062011719 "GRS80" 48 505946.450 6637205.350)
                      #(-29.3438753994 130.8032226563 "GRS80" 52 675064.430 6752564.671)

                      )))
    (dolist (coord coords)
      (let* ((lat (aref coord 0))
             (lon (aref coord 1))
             (ellipsoid (aref coord 2))
             (zone (aref coord 3))
             (easting (aref coord 4))
             (northing (aref coord 5))
             (translated (lat-lon-to-utm lat lon :ellipsoid ellipsoid))
             (teasting (car translated))
             (tnorthing (cadr translated))
             (tzone (caddr translated)))
        (format t "~%(- ~a ~a) ~a~%" teasting easting (- teasting easting))
        (format t "~%(- ~a ~a) ~a~%" tnorthing northing (- tnorthing northing))
        (is-true (< (abs (- teasting easting)) tolerance))
        (is-true (< (abs (- tnorthing northing)) tolerance))
        (is-true (= zone tzone))))))
