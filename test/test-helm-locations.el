;; test-helm-locations.el --- Tests for helm-locations.el
;;
;; Author: Minae Yui <minae.yui.sain@gmail.com>
;; Version: 0.1
;; URL: todo
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Tests for helm-locations.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'buttercup)
(require 'dired-helm-locations)

(describe "dired-helm-locations-add"
  (before-each
   (dired-helm-locations-clear))

  (it "Can add locations"
    (expect (length (dired-helm-locations-get-all))
            :to-be 0)
    (dired-helm-locations-add "temp" "/tmp")
    (expect (length (dired-helm-locations-get-all))
            :to-be 1))

  (it "Can add complex locations"
    (expect (length (dired-helm-locations-get-all))
            :to-be 0)
    (dired-helm-locations-add "temp" (car '("/tmp" "/tmp")))
    (expect (length (dired-helm-locations-get-all))
            :to-be 1))

  (it "Can add several locations at once"
    (expect (length (dired-helm-locations-get-all))
            :to-be 0)
    (dired-helm-locations-add "temp1" (car '("/tmp" "/tmp"))
                              "temp2" (cadr '("/tmp" "/tmp")))
    (expect (length (dired-helm-locations-get-all))
            :to-be 2)))

(describe "dired-helm-locations-get-candidates"
  (before-each
   (dired-helm-locations-clear))
  (it "Returns candidates for helm"
    (dired-helm-locations-add "temp" "/tmp")
    (expect (dired-helm-locations-get-candidates)
            :to-have-same-items-as '("temp"))
    (dired-helm-locations-add "temp-2" "/tmp-2")
    (expect (dired-helm-locations-get-candidates)
            :to-have-same-items-as '("temp" "temp-2"))))

;; Local Variables:
;; eval: (put 'describe 'lisp-indent-function 'defun)
;; eval: (put 'it       'lisp-indent-function 'defun)
;; End:
