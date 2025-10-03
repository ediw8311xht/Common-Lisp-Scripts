#!/usr/bin/env -S sbcl --script

(load "~/.sbclrc")

(require :my-utils)
(require :drakma)
(require :uiop)
(require :yason)
(defpackage :get-spot-price
  (:use :cl :my-utils :drakma :yason)
  (:import-from :uiop #:getenv #:getenvp #:split-string)
  (:export #:main))

(in-package :get-spot-price)

(defparameter *api-ninja-key* (uiop:getenvp "API_NINJAS_API_KEY"))

(defparameter *api-ninjas-url*              "https://api.api-ninjas.com/v1/stockprice")
(defparameter *metal-spot-urls*             (make-hash-table :test #'equalp))
(setf (gethash "SILVER" *metal-spot-urls* ) "https://data-asg.goldprice.org/GetData/USD-XAG/1")
(setf (gethash "GOLD" *metal-spot-urls* )   "https://data-asg.goldprice.org/GetData/USD-XAU/1")

(defun convert-char-vec (vec)
  (map 'string #'code-char vec))

(defun convert-json (vec)
  (yason:parse (convert-char-vec vec)))

(defun stock-price (stock &key (api-key *api-ninja-key*))
  (gethash "price"
           (convert-json 
             (drakma:http-request *api-ninjas-url* 
                                  :parameters `(("ticker" . ,stock)) 
                                  :additional-headers `(("X-Api-Key" . ,api-key))))))

(defun metal-spot-price (metal)
  (let ((url (gethash metal *metal-spot-urls*)))
    (unless url
      (error "Metal, ~A, not a valid metal." metal))
    (second (my-utils:split-by-char 
              (car (convert-json (drakma:http-request url)))))))

(defun handle (type query)
  (cond 
    ((string-equal type "STOCK") (stock-price query))
    ((string-equal type "METAL") (metal-spot-price query))
    (t                           (error "Invalid argument, ~A" type))))

(defun main (args)
  (loop for (type query) on args by #'cddr
        do (format t "~f" (handle type query))))

(main (cdr sb-ext:*posix-argv*))

