;;; ipe-mode.el --- Minor mode for Ipe vector graphics integration -*- lexical-binding: t; -*-

;;; Commentary:
;; This minor mode provides functions to create, edit, and insert Ipe vector
;; graphics files with LaTeX integration, particularly useful in org-mode.

;;; Code:

(require 'org)

(defgroup ipe nil
  "Integration with Ipe vector graphics editor."
  :group 'applications
  :prefix "ipe-")

(defcustom ipe-executable "ipe"
  "Path to the Ipe executable."
  :type 'string
  :group 'ipe)

(defcustom ipe-assets-directory "assets"
  "Directory name for storing Ipe files, relative to project root."
  :type 'string
  :group 'ipe)

(defcustom ipe-default-extension ".ipe"
  "Default file extension for Ipe files."
  :type 'string
  :group 'ipe)

(defun ipe--find-project-root ()
  "Find the project root directory.
Looks for .git, .projectile, or falls back to current directory."
  (or (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory ".projectile")
      default-directory))

(defun ipe--assets-directory ()
  "Get the full path to the assets directory."
  (let ((project-root (ipe--find-project-root)))
    (expand-file-name ipe-assets-directory project-root)))

(defun ipe--ensure-assets-directory ()
  "Ensure the assets directory exists."
  (let ((assets-dir (ipe--assets-directory)))
    (unless (file-directory-p assets-dir)
      (make-directory assets-dir t))
    assets-dir))

(defun ipe--generate-filename (base-name)
  "Generate a unique filename for BASE-NAME in the assets directory."
  (let* ((assets-dir (ipe--ensure-assets-directory))
         (counter 1)
         (filename (concat base-name ipe-default-extension))
         (full-path (expand-file-name filename assets-dir)))
    (while (file-exists-p full-path)
      (setq filename (format "%s-%d%s" base-name counter ipe-default-extension))
      (setq full-path (expand-file-name filename assets-dir))
      (setq counter (1+ counter)))
    filename))

(defun ipe--relative-path (full-path)
  "Convert FULL-PATH to a path relative to the current buffer's directory."
  (let ((current-dir (file-name-directory (or buffer-file-name default-directory))))
    (file-relative-name full-path current-dir)))

(defun ipe--extract-filename-at-point ()
  "Extract filename from LaTeX includegraphics command at point."
  (save-excursion
    (let ((line (thing-at-point 'line)))
      (when (and line (string-match "\\\\includegraphics\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)}" line))
        (match-string 1 line)))))

(defun ipe-create-and-edit ()
  "Create a new Ipe file and open it for editing."
  (interactive)
  (let* ((base-name (read-string "Base filename (without extension): " 
                                 (format-time-string "figure-%Y%m%d")))
         (filename (ipe--generate-filename base-name))
         (full-path (expand-file-name filename (ipe--assets-directory))))
    
    ;; Create empty Ipe file with basic structure
    (with-temp-file full-path
      (insert "<?xml version=\"1.0\"?>
<!DOCTYPE ipe SYSTEM \"ipe.dtd\">
<ipe version=\"70218\" creator=\"Emacs ipe-mode\">
<info created=\"" (format-time-string "%Y%m%d %H:%M:%S") "\" modified=\"" (format-time-string "%Y%m%d %H:%M:%S") "\"/>
<ipestyle name=\"basic\">
</ipestyle>
<page>
<layer name=\"alpha\"/>
<view layers=\"alpha\" active=\"alpha\"/>
</page>
</ipe>"))
    
    ;; Open in Ipe
    (start-process "ipe" nil ipe-executable full-path)
    
    ;; Insert LaTeX code at point
    (ipe--insert-latex-code filename)
    
    (message "Created and opened %s" filename)))

(defun ipe--insert-latex-code (filename)
  "Insert LaTeX includegraphics code for FILENAME at point."
  (let ((relative-path (ipe--relative-path 
                        (expand-file-name filename (ipe--assets-directory)))))
    ;; Remove .ipe extension for LaTeX
    (when (string-suffix-p ipe-default-extension relative-path)
      (setq relative-path (substring relative-path 0 (- (length ipe-default-extension)))))
    
    (if (derived-mode-p 'org-mode)
        ;; Insert org-mode figure block
        (insert (format "#+BEGIN_FIGURE
#+ATTR_LaTeX: :width 0.8\\textwidth
\\includegraphics{%s}
#+CAPTION: 
#+NAME: fig:%s
#+END_FIGURE"
                        relative-path
                        (file-name-base filename)))
      ;; Insert plain LaTeX
      (insert (format "\\begin{figure}[htbp]
  \\centering
  \\includegraphics[width=0.8\\textwidth]{%s}
  \\caption{}
  \\label{fig:%s}
\\end{figure}"
                      relative-path
                      (file-name-base filename))))))

(defun ipe-edit-at-point ()
  "Open Ipe file referenced at point for editing."
  (interactive)
  (let ((filename (ipe--extract-filename-at-point)))
    (if filename
        (let* ((assets-dir (ipe--assets-directory))
               (ipe-file (if (file-name-absolute-p filename)
                            filename
                          (expand-file-name (concat filename ipe-default-extension) assets-dir))))
          (if (file-exists-p ipe-file)
              (progn
                (start-process "ipe" nil ipe-executable ipe-file)
                (message "Opened %s in Ipe" ipe-file))
            (if (y-or-n-p (format "File %s doesn't exist. Create it? " ipe-file))
                (progn
                  ;; Create the file with basic structure
                  (with-temp-file ipe-file
                    (insert "<?xml version=\"1.0\"?>
<!DOCTYPE ipe SYSTEM \"ipe.dtd\">
<ipe version=\"70218\" creator=\"Emacs ipe-mode\">
<info created=\"" (format-time-string "%Y%m%d %H:%M:%S") "\" modified=\"" (format-time-string "%Y%m%d %H:%M:%S") "\"/>
<ipestyle name=\"basic\">
</ipestyle>
<page>
<layer name=\"alpha\"/>
<view layers=\"alpha\" active=\"alpha\"/>
</page>
</ipe>"))
                  (start-process "ipe" nil ipe-executable ipe-file)
                  (message "Created and opened %s" ipe-file))
              (message "File %s not found" ipe-file))))
      (message "No includegraphics command found at point"))))

(defun ipe-insert-latex-code ()
  "Insert LaTeX code for loading an existing Ipe image."
  (interactive)
  (let* ((assets-dir (ipe--assets-directory))
         (ipe-files (directory-files assets-dir nil "\\.ipe$"))
         (selected-file (completing-read "Select Ipe file: " ipe-files)))
    (when selected-file
      (ipe--insert-latex-code selected-file))))

;;;###autoload
(define-minor-mode ipe-mode
  "Minor mode for Ipe vector graphics integration."
  :lighter " Ipe"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c i c") 'ipe-create-and-edit)
            (define-key map (kbd "C-c i e") 'ipe-edit-at-point)
            (define-key map (kbd "C-c i i") 'ipe-insert-latex-code)
            map)
  :group 'ipe)

;;;###autoload
(defun ipe-mode-enable-in-org ()
  "Enable ipe-mode in org-mode buffers."
  (when (derived-mode-p 'org-mode)
    (ipe-mode 1)))

;; Auto-enable in org-mode
(add-hook 'org-mode-hook 'ipe-mode-enable-in-org)

(provide 'ipe-mode)

;;; ipe-mode.el ends here