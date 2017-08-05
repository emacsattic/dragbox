;;; dragbox.el --- draw a bounding box interactively

;; License: GPL-3+
;;; Commentary:

;; Draw a bounding box on an image interactively.

;; Use m-x dragbox-start to try it out, enter a the name of an image
;; file compatible with SVG, such as jpg or png.

;; Then mark the upper left corner with the left mouse button.
;; Mark the lower right corner with the right mouse button.

;; The bounding box will be drawn as a grey rectangle over the
;; image. This requires an Emacs compiled with SVG support.

;; An ocr application of the  bounding box is included,
;; which will require tesseract, gocr, or ocrad installed.

;; for instance, select an image region as per above, then do:
;;    m-x dragbox-ocr-gocr-image-region
;; if everything works as it should,
;; the image region should be ocr:ed and the text shown in the
;; message area. The ocr:ed text will also be put in the kill ring.

;; Please not that this is only alpha quality, feedback appreciated.

;; Author: Joakim Verona, (C) FSF 2009, GPL

;;; History:
;;

;; TODO

;; - support more than one session probably with buffer local variables
;; - use imagemagick to convert the entered file name to something compatible with svg
;; - url encode img name properly to avoid file name encoding issues
;; - some type of local minor mode so as not to pollute image-mode
;;   - unbind mouse up events in this mode
;; - this code is meant as an api for emacs apps wanting interactive bounding boxes,
;;   so provide nice api:
;;   - dragbox-start image-file ; start interactive box finding
;;   - dragbox-get-box  ; get the actual box coords
;;       - return image coords rather than svg coords
;;   - maybe some "done" callback for clients to use
;; - set moise pointer to "arrow" over image

;; futureish:
;; - support zooming in the image for better placing of box
;; - investigate MON KEY:s idea to use artist.el
;; - ability to define a set of boxes, for ocr
;; - be able to draw a line in the image as a guide for deskewing

(require 'image-mode)
(require 'xml)

(if (not (image-type-available-p 'svg))
    (error "No svg support available!"))

;;; Code:

;;Image size. currently calculated from the image we are working with
(defvar dragbox-image-width 0)
(defvar dragbox-image-height 0)

(defvar dragbox-x1y1 '(0 . 0) "Top left corner of bounding box.")
(defvar dragbox-x2y2 '(100 . 100) "Bottom right corner.")

(defvar dragbox-image-url "" "Which image to work with.")


(defun dragbox-make-image-url (image-file)
  (if
      (string-match "\\(\\.png\\'\\)\\|\\(\\.jpg\\'\\)" image-file)
      (concat "file://" (expand-file-name image-file))
    (progn
      (call-process-shell-command (format "convert %s /tmp/xxx.png" image-file))
      (concat "file://" (expand-file-name "/tmp/xxx.png"))
      )))

(defun dragbox-start (image-file box-do-callback)
  "Start here with an IMAGE-FILE suitable for svg embedding.
execute BOX-DO-callback on middle-mouse(for instance)
"
  (interactive "fImage file:")

  (get-buffer-create "*dragbox*")
  (switch-to-buffer  "*dragbox*")

  (setq dragbox-image-url (dragbox-make-image-url image-file))

  (setq dragbox-image-width (car (dragbox-image-size  dragbox-image-url)))
  (setq dragbox-image-height (cdr (dragbox-image-size  dragbox-image-url)))
  (setq dragbox-action-callback box-do-callback)
  
  (dragbox-update-box-from-state))


(defun dragbox-make-svg-data (x y width height image-url)
  "Return svg describing a image file with a bounding box on top.
X Y WIDTH HEIGHT describes the box, IMAGE-URL which image to draw on."
  `((svg
         ((xmlns:xlink . "http://www.w3.org/1999/xlink")
          (xmlns . "http://www.w3.org/2000/svg")
          (width . ,(number-to-string dragbox-image-width))
          (height . ,(number-to-string dragbox-image-height)))
         (g
          ((id . "layer1"))
          (rect
           ((style . "fill:#cfcfcf;fill-opacity:1")
            (width . ,(number-to-string dragbox-image-width))
            (height . ,(number-to-string dragbox-image-height))
            (x . "0")
            (y . "0")))
          (image ((y . "0")
                  (x . "0")
                  (width . ,(number-to-string dragbox-image-width))
                  (height . ,(number-to-string dragbox-image-height))
                  (xlink:href . ,image-url)
                 ))
          (rect
           ((style . "color:#000000;fill:#000000;fill-opacity:0.5;fill-rule:nonzero;stroke:#000000;stroke-width:1;marker:none;visibility:visible;display:inline;overflow:visible;enable-background:accumulate;stroke-opacity:0.5")
            (id . "dragbox")
            (width . , (number-to-string width) )
            (height . ,(number-to-string height))
            (x . ,(number-to-string x))
            (y . ,(number-to-string y))))
          ))))

(defun dragbox-lmb-click-handler ()
  "Set upper left coords for bounding box."
            (interactive)
            (setq dragbox-x1y1 (dragbox-extract-event-coords last-input-event))
            (dragbox-update-box-from-state)
            )

(defun dragbox-rmb-click-handler ()
  "Set lower right coords for bounding box."
            (interactive)
            (setq dragbox-x2y2 (dragbox-extract-event-coords last-input-event))
            (dragbox-update-box-from-state)
            )


(defun dragbox-mmb-click-handler ()
  "Do something with the bounding box."
            (interactive)
            (apply (lambda (x1 y1 w h)
                     (message "(%d %d) w:%d h:%d %s" x1 y1  w h
                              (dragbox-extract-event-coords last-input-event)))
                   (dragbox-get-box))
            (funcall dragbox-action-callback)
            )

(defun dragbox-extract-event-coords (event)
  "Get the coordinates from click EVENT."
   (nth 8 (cadr last-input-event))
  )

;;bind the handlers to lmb and rmb
(define-key image-mode-map [down-mouse-1] 'dragbox-lmb-click-handler)
(define-key image-mode-map [down-mouse-2] 'dragbox-mmb-click-handler)
(define-key image-mode-map [down-mouse-3] 'dragbox-rmb-click-handler)


(defun dragbox-update-box (x y width height)
  "Redraw the bounding box, given X Y WIDTH and HEIGHT ontop of the image."
  ;;this implementation doesn't seem very efficient TODO improve
  (fundamental-mode)
  (erase-buffer)
  (xml-print (dragbox-make-svg-data x y width height dragbox-image-url))
  (image-mode))

(defun dragbox-update-box-from-state ()
  "Redraw bounding box from global state ontop of image."
  (apply 'dragbox-update-box (dragbox-get-box)))


(defun dragbox-get-box ()
  "Return x,y,w,h from the box."
  (let*
      ((x1 (car dragbox-x1y1))
       (y1 (cdr dragbox-x1y1))
       (x2 (car dragbox-x2y2))
       (y2 (cdr dragbox-x2y2))
       (w (- x2 x1))
       (h (- y2 y1)))
    (list x1 y1 w h)
    ))

;;image size hacks
;;identify -verbose -ping /home/joakim/Desktop/xwidget_demo_screenshot.png
;; grep for:   Geometry: 992x957+0+0
;; without verbose less easy parsing:
;; /home/joakim/Desktop/xwidget_demo_screenshot.png PNG 992x957 992x957+0+0 8-bit DirectClass 166kb
;; it would be possible to open an image in a buffer and use the image-size defun, but that seems wasteful, and
;; we still need imagemagick for any practical application.

(defun dragbox-image-size (image-file)
  "Return the size of IMAGE-FILE as a cons."
  (with-current-buffer (get-buffer-create "*imagemagic identify*")
    (erase-buffer)
    (call-process "identify" nil "*imagemagic identify*" nil "-verbose" image-file) ;; "-ping" sometimes segfaults for me
    (goto-char (point-min))
    (re-search-forward "Geometry: \\([0-9]+\\)x\\([0-9]+\\)")
    (cons (string-to-number (match-string 1))
          (string-to-number (match-string 2)))))

(defun dragbox-get-real-box ()
  "Like dragbox-get-box but image coordinates rather than screen coordinates."
  ;;currently no-op since we show image 1:1
  )

(defun dragbox-get-box-geometry ()
  "The box as an x and imagemagick compatible geometry string."
  (let ((box (dragbox-get-box)))
    (format "%sx%s+%s+%s" (nth 2 box) (nth 3 box) (nth 0 box) (nth 1 box))))


;; support for ocr of contents of bounding box

;; should use temp files like (make-temp-file "/tmp/" nil ".xxx")

(defvar dragbox-image-options "-density 150x150 -compress none -monochrome")
;;-monochrome -resize 200%  -density 150x150 -fill white -tint 50 -level 20%,80%,1.0 -sigmoidal-contrast 30,50% -sharpen 0x2 -compress none
(defun dragbox-crop (crop-file image-url &optional  image-options)
  "Crop selected image region to CROP-FILE."
  (unless image-options (setq image-options ""))
  (call-process-shell-command (format "convert %s  -crop %s %s %s "
                                      image-options
                                      (dragbox-get-box-geometry)
                                      image-url
                                      crop-file  )))

(defun dragbox-ocr-file-to-kill-ring ()
  "Put /tmp/ocr.txt in kill ring."
  (with-temp-buffer
    (insert-file-contents "/tmp/ocr.txt")
    (copy-region-as-kill (point-min)(point-max))
    (message "%s" (car kill-ring))))


(defun dragbox-ocr-tesseract-image-region ()
  "Ocr region with tesseract."
  (interactive)
  (dragbox-crop "/tmp/dragbox-crop.tif" dragbox-image-url dragbox-image-options)
  (call-process-shell-command "rm  /tmp/ocr.txt;tesseract /tmp/dragbox-crop.tif /tmp/ocr")
  (dragbox-ocr-file-to-kill-ring))


(defun  dragbox-ocr-ocrad-image-region ()
  "Ocr region with ocrad."
  (interactive)
  (dragbox-crop "/tmp/dragbox-crop.pbm" dragbox-image-url  dragbox-image-options)
  (call-process-shell-command "rm  /tmp/ocr.txt;ocrad /tmp/dragbox-crop.pbm -x /tmp/x.orf -o /tmp/ocr.txt")
  (dragbox-ocr-file-to-kill-ring))

(defun  dragbox-ocr-gocr-image-region ()
  "Ocr region with gocr."
  (interactive)
  (dragbox-crop "/tmp/dragbox-crop.pbm" dragbox-image-url  dragbox-image-options)
  (call-process-shell-command "rm  /tmp/ocr.txt;gocr -i /tmp/dragbox-crop.pbm -o /tmp/ocr.txt")
  (dragbox-ocr-file-to-kill-ring))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPERIMENTAL functions to rename a bunch of files using cropped
;; thumbnails in imagedired.

;;tentative usage:
;; - dragbox-start on a file in an image directory
;; - select an image region where an interesting feature like a page number is
;;   were assuming the region will be the same in all image files in the directory
;; - m-x dragbox-imagedired-start-crop-rename
;;   all image files will be cropped into a crop dir.
;; - show this dir in imagedired thumbail mode
;; for each image you want to rename according to info in the crop do:
;; m-x dragbox-imagedired-rename-original

;;BUG: image-dired doesnt regenerate thumbnails reliably!

(defun dragbox-imagedired-generate-crops (image-directory)


  ;;TODO mkdir crop dir, clean it if its already there
  ;; crops will go into <image-directory>/crop/*png currently
  (mapcar
   (lambda (file)
     (dragbox-crop (concat
                    image-directory "/crop/" file ".png") file ) )
   (directory-files image-directory)) )

(defun dragbox-imagedired-rename-original (rename-to)
  (interactive "Mrename to:")
  (let*
      ((file-name     (file-name-nondirectory (image-dired-original-file-name)))
       (org-file-name (substring (expand-file-name (concat "../" file-name)  ) 0 -4))
       (rename-to     (expand-file-name (concat "../" rename-to ".djvu")))
       )
    (message "rename %s to %s" org-file-name rename-to)
    (rename-file  org-file-name rename-to)

))


;;;;;;;;;;;;;;;;;;;
;;totaly untested



(provide 'dragbox)

;;; dragbox.el ends here
