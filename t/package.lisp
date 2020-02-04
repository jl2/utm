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

(defparameter *utm-tolerance* 0.002)
(defparameter *test-coords* '(
                              #(39.9835310406d0 -105.2536113560d0 "NAD83" 13 478346.59856d0 4425960.12177d0)
                              #(46.8392935284d0 -115.3348159790d0 "NAD83" 11 626974.542d0 5188651.724d0)
                              #(48.2338208531d0 19.3716430664d0 "GRS80" 34 379083.606d0 5343570.714d0)
                              #(59.8723979923d0 105.1062011719d0 "GRS80" 48 505946.450d0 6637205.350d0)
                              #(-29.3438753994d0 130.8032226563d0 "GRS80" 52 675064.430d0 6752564.671d0)))
(test lat-lon-to-utm
  (dolist (coord *test-coords*)
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
      (is-true (< (abs (- teasting easting)) *utm-tolerance*))
      (is-true (< (abs (- tnorthing northing)) *utm-tolerance*))
      (is-true (= zone tzone)))))


(test utm-to-lat-lon
  (dolist (coord *test-coords*)
    (let* ((lat (aref coord 0))
           (lon (aref coord 1))
           (ellipsoid (aref coord 2))
           (zone (aref coord 3))
           (easting (aref coord 4))
           (northing (aref coord 5))
           (translated (utm-to-lat-lon easting northing zone :ellipsoid ellipsoid))
           (tlat (car translated))
           (tlon (cadr translated)))
      (is-true (< (abs (- tlat lat)) *utm-tolerance*))
      (is-true (< (abs (- tlon lon)) *utm-tolerance*)))))
