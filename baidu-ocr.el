;;; baidu-ocr.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020  lee

;; Author: lee;;; -*- lexical-binding: t; -*- <loyalpartner@163.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defcustom baidu-ocr-request-url
  "https://aip.baidubce.com/rest/2.0/ocr/v1/general_basic"
  "request url")

(defcustom baidu-ocr-grant-type "client_credentials" "")

(defcustom baidu-ocr-client-id nil "")

(defcustom baidu-ocr-seceret-key nil "")

(defcustom baidu-ocr-buffer-name "*baidu-ocr*" "")

(defcustom baidu-ocr-region t "ocr region by selected" )

(defcustom baidu-ocr-access-token nil "")

(defun baidu-ocr--base64-encode-image (image)
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents image)
     (buffer-string))))

(defun baidu-ocr--http-get (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (when (not (string-match "200 OK" (buffer-string)))
      (error "Problem connecting to the server"))
    (re-search-forward "^$" nil 'move)
    (buffer-substring-no-properties (point) (point-max))))

(defun baidu-ocr--http-post (url data)
  (let* ((url-request-method        "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (buffer-substring-no-properties (point) (point-max)))))

(defun baidu-ocr--get-access-token-from-response (content)
  (let* ((json (json-read-from-string content))
         (access-token (alist-get 'access_token json nil)))
    (when access-token access-token)))

(defun baidu-ocr-get-access-token ()
  (interactive)
  (setq baidu-ocr-access-token
        (baidu-ocr--get-access-token-from-response
         (baidu-ocr--http-get
          (format "https://aip.baidubce.com/oauth/2.0/token?grant_type=%s&client_id=%s&client_secret=%s"
                  baidu-ocr-grant-type
                  baidu-ocr-client-id
                  baidu-ocr-seceret-key)))))

(defun baidu-ocr--get-ocr-result (image)
  (let* ((url (format "%s?access_token=%s" baidu-ocr-request-url baidu-ocr-access-token))
         (data (format "image=%s" (url-hexify-string (baidu-ocr--base64-encode-image image))))
         json
         ocr-result)
    (setq json (json-read-from-string (baidu-ocr--http-post url data)))
    (setq ocr-result (alist-get 'words_result json))
    (when ocr-result
      (mapconcat (lambda (json)
                   (concat (assoc-default 'words json)))
                 ocr-result "\n"))))

(defun baidu-ocr-ocr-image (image)
  ;; TODO read image
  (interactive)
  (unless baidu-ocr-access-token (baidu-ocr-get-access-token))

  (with-current-buffer (get-buffer-create baidu-ocr-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (baidu-ocr--get-ocr-result image))
    (visual-line-mode 1)
    ;; (setq buffer-read-only t)
    (goto-char (point-min))
    (when (featurep 'baidu-translator) (baidu-translator-translate-mode +1))
    (pop-to-buffer (current-buffer))))

(defun baidu-ocr-screenshot ()
  (let ((filename (format "/tmp/%s.png" (format-time-string "%Y%m%d%H%M%S"))))
    (shell-command (format "spectacle -r -b -n -o %s"  filename))
    filename))

(defun baidu-ocr-ocr-page ()
  (interactive)
  (let ((image (baidu-ocr-screenshot)))
    (delete-other-frames)
    (baidu-ocr-ocr-image image)
    (delete-file image)))

(provide 'baidu-ocr)
