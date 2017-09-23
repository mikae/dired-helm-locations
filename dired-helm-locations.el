;; dired-helm-locations.el --- Helm location candidates for dired buffers.
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
;;             Displays location candidates for dired buffers with helm.
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
(defconst dired-helm-locations-version "0.1"
  "Version of dired-helm-locations.")

(defvar dired-helm-locations--alist ()
  "This variable is alist.
((string-name . (elisp-code-to-get-locations))).")

(defvar dired-helm-locations-source
  `((name . "Open location in dired")
    (candidates . ,(mapcar 'car dired-helm-locations--alist))
    (action . (lambda (candidate)
                (let ((--path (eval (cdr (assoc candidate dired-helm-locations--alist)))))
                  (dired --path))
                ))))

(defmacro dired-helm-locations-add (location-name location)
  "Add new (LOCATION-NAME . LOCATION) pair."
  `(setq dired-helm-locations--alist
         (cons '(,location-name . ,location) dired-helm-locations--alist)))

(defun dired-helm-locations-get-all ()
  "Get location alist."
  dired-helm-locations--alist)

(defun dired-helm-locations-clear ()
  "Clear location alist."
  (setq dired-helm-locations--alist ()))

(defun dired-helm-locations-get-candidates ()
  "Get candidates."
  (mapcar 'car dired-helm-locations--alist))

(defun dired-helm-locations-open ()
  "Select location from helm buffer, and open it in dired buffer."
  (interactive)
  (helm :sources `((name       . "Open location in Dired")
                   (candidates . dired-helm-locations-get-candidates)
                   (action     . (lambda (candidate)
                                   (let ((--path (eval (cdr (assoc candidate dired-helm-locations--alist)))))
                                     (dired --path)))))))

(provide 'dired-helm-locations)
;;; dired-helm-locations.el.el ends here
