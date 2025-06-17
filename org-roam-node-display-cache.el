;;; org-roam-node-display-cache.el --- Cache display values for Org Roam nodes  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-node-display-cache/
;; Version: 0.2.1
;; Keywords:
;; Package-Requires: ((emacs \"26.1\") (org-roam "20250527.1558") (ok "0.5.2"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The library adds a cache layer to speed up completing read in Org Roam.
;;
;;; Code:

(require 'ok)
(require 'org-roam)

(defvar org-roam-node-display-cache--cache (make-hash-table :test #'equal)
  "In-memory cache for org-roam-node display.")

(defun org-roam-node-display-cache--get (node total-width)
  "Get the cached item for NODE and TOTAL-WIDTH from in-memory cache."
  (if-let* ((file (org-roam-node-file node))
            (id (org-roam-node-id node))
            (title (org-roam-node-title node))
            (entries (gethash file org-roam-node-display-cache--cache))
            (value (cdr (assoc `(,id ,title ,total-width) entries))))
      value))

(defun org-roam-node-display-cache--save (value node total-width)
  "Save VALUE to in-memory cache for NODE and TOTAL-WIDTH."
  (let* ((file (org-roam-node-file node))
         (id (org-roam-node-id node))
         (title (org-roam-node-title node))
         (entries (gethash file org-roam-node-display-cache--cache)))
    (push (cons `(,id ,title ,total-width) value) entries)
    (puthash file entries org-roam-node-display-cache--cache)))

(defun org-roam-node-display-cache--remove (node)
  "Remove NODE item from the in-memory cache."
  (remhash (org-roam-node-file node) org-roam-node-display-cache--cache))

(defun org-roam-node-display-cache--clear ()
  "Clear the in-memory cache."
  (clrhash org-roam-node-display-cache--cache))

(defun org-roam-node-display-cache--maybe-remove ()
  "Potentially remove in-memory cached item on file update."
  (if-let ((_ org-roam-node-display-cache--cache)
           (_ (derived-mode-p '(org-mode)))
           (node (org-roam-node-at-point t)))
      (org-roam-node-display-cache--remove node)))

(defun org-roam-node-display-cache--ad (fun node &rest rest)
  "Cache rendered value from display template for NODE.
Around-advise a function FUN in `org-roam-node-display-template'."
  (if-let* ((total-width (frame-width))
            (cached (org-roam-node-display-cache--get node total-width)))
      cached
    (let ((rendered (apply fun `(,node ,@rest))))
      (org-roam-node-display-cache--save rendered node total-width)
      rendered)))

;; TODO(2025-06-08): Implement builtin cache persistence mechanism.

(defun org-roam-node-display-cache--delete-file-ad (fun file &rest rest)
  "Maintain cache consistency on FILE deletion by FUN."
  (let ((org-roam-file-p (and (not (auto-save-file-name-p file))
                              (not (backup-file-name-p file))
                              (org-roam-file-p file))))
    (apply fun `(,file ,@rest))
    (when (and org-roam-file-p (not (file-exists-p file)))
      (remhash file org-roam-node-display-cache--cache))))

;;; Minor Mode Interface

(defun org-roam-node-display-cache-mode--activate ()
  "Activate `org-roam-node-display-cache-mode'."
  (add-hook 'after-save-hook #'org-roam-node-display-cache--maybe-remove)
  (advice-add #'vc-delete-file :around #'org-roam-node-display-cache--delete-file-ad)
  (advice-add #'delete-file :around #'org-roam-node-display-cache--delete-file-ad)
  (when (functionp org-roam-node-display-template)
    (advice-add org-roam-node-display-template :around
                #'org-roam-node-display-cache--ad)))

(defun org-roam-node-display-cache-mode--deactivate ()
  "Deactivate `org-roam-node-display-cache-mode'."
  (when (functionp org-roam-node-display-template)
    (advice-remove org-roam-node-display-template
                   #'org-roam-node-display-cache--ad))
  (advice-remove #'delete-file #'org-roam-node-display-cache--delete-file-ad)
  (advice-remove #'vc-delete-file #'org-roam-node-display-cache--delete-file-ad)
  (remove-hook 'after-save-hook #'org-roam-node-display-cache--maybe-remove))

;;;###autoload
(define-minor-mode org-roam-node-display-cache-mode
  "Add in-memory cache layer to speed up Org Roam completing read."
  :init-value nil
  :lighter "org-roam-node-display-cache"
  :global t
  :group 'org-roam
  (if org-roam-node-display-cache-mode
      (org-roam-node-display-cache-mode--activate)
    (org-roam-node-display-cache-mode--deactivate)))

(provide 'org-roam-node-display-cache)
;;; org-roam-node-display-cache.el ends here
