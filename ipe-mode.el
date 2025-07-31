;;; ipe-mode.el --- Minor mode for Ipe vector graphics integration -*- lexical-binding: t; -*-

;;; Commentary:
;; This minor mode provides functions to create, edit, and insert Ipe vector
;; graphics files with LaTeX integration, particularly useful in org-mode.
;; TODO: https://orgmode.org/manual/Images.html

;;; Code:

(require 'org)
(require 'project)

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

(defcustom ipe-auto-export t
  "Automatically export Ipe files to PDF when creating or editing."
  :type 'boolean
  :group 'ipe)

(defun ipe--find-project-root ()
  "Find the project root directory using project.el."
  (if-let ((project (project-current)))
      (project-root project)
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

(defun ipe--export-to-pdf (ipe-file)
  "Export IPE-FILE to PDF using ipetoipe command."
  (let* ((pdf-file (concat (file-name-sans-extension ipe-file) ".pdf"))
         (command (format "ipetoipe -pdf %s %s" ipe-file pdf-file)))
    (shell-command command)
    (if (file-exists-p pdf-file)
        (message "Exported %s to PDF" (file-name-nondirectory pdf-file))
      (message "Failed to export %s to PDF" (file-name-nondirectory ipe-file)))
    pdf-file))

(defun ipe--process-sentinel (process event ipe-file)
  "Sentinel function to export IPE-FILE to PDF when Ipe process exits."
  (when (and (memq (process-status process) '(exit signal))
             (file-exists-p ipe-file))
    (if ipe-auto-export
        (ipe--export-to-pdf ipe-file)
      (message "Ipe process finished. Use C-c i p to export to PDF."))))

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
    
    ;; Open in Ipe with process sentinel for auto-export
    (let ((process (start-process "ipe" nil ipe-executable full-path)))
      (when process
        (set-process-sentinel process
                              (lambda (proc event)
                                (ipe--process-sentinel proc event full-path)))))
    
    ;; Insert LaTeX code at point
    (ipe--insert-latex-code filename)
    
    (message "Created and opened %s" filename)))

(defun ipe--insert-latex-code (filename)
  "Insert LaTeX includegraphics code for FILENAME at point.
FILENAME should be the .ipe filename, but we insert the .pdf path."
  (let ((relative-path (ipe--relative-path 
                        (expand-file-name filename (ipe--assets-directory)))))
    ;; Change .ipe extension to .pdf for LaTeX
    (when (string-suffix-p ipe-default-extension relative-path)
      (setq relative-path (concat (substring relative-path 0 (- (length ipe-default-extension))) ".pdf")))
    
    (if (derived-mode-p 'org-mode)
        ;; Insert org-mode figure block
        (insert (format "#+begin_figure
#+ATTR_LaTeX: :width 0.8\\textwidth
\\includegraphics{%s}
#+CAPTION: 
#+NAME: fig:%s
#+end_figure"
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
  "Open Ipe file referenced at point for editing.
Extracts PDF filename from includegraphics and opens corresponding .ipe file."
  (interactive)
  (let ((filename (ipe--extract-filename-at-point)))
    (if filename
        (let* ((assets-dir (ipe--assets-directory))
               ;; Convert PDF filename to IPE filename
               (base-name (if (string-suffix-p ".pdf" filename)
                             (substring filename 0 -4)  ; Remove .pdf extension
                           filename))
               ;; Normalize paths and check if it starts with assets directory
               (normalized-base (expand-file-name base-name (ipe--find-project-root)))
               (normalized-assets (expand-file-name ipe-assets-directory (ipe--find-project-root)))
               (ipe-file (if (string-prefix-p (file-name-as-directory normalized-assets) 
                                             (file-name-as-directory normalized-base))
                            ;; Path already includes assets directory
                            (concat normalized-base ipe-default-extension)
                          ;; Simple filename - add to assets dir
                          (expand-file-name (concat base-name ipe-default-extension) assets-dir))))
          (if (file-exists-p ipe-file)
              (progn
                ;; Open in Ipe with process sentinel for auto-export
                (let ((process (start-process "ipe" nil ipe-executable ipe-file)))
                  (when process
                    (set-process-sentinel process
                                          (lambda (proc event)
                                            (ipe--process-sentinel proc event ipe-file)))))
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
                  ;; Open in Ipe with process sentinel for auto-export
                  (let ((process (start-process "ipe" nil ipe-executable ipe-file)))
                    (when process
                      (set-process-sentinel process
                                            (lambda (proc event)
                                              (ipe--process-sentinel proc event ipe-file)))))
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

(defun ipe-export-to-pdf ()
  "Export Ipe file at point or selected file to PDF."
  (interactive)
  (let ((filename (ipe--extract-filename-at-point)))
    (if filename
        (let* ((assets-dir (ipe--assets-directory))
               ;; Convert PDF filename to IPE filename
               (base-name (if (string-suffix-p ".pdf" filename)
                             (substring filename 0 -4)  ; Remove .pdf extension
                           filename))
               ;; Normalize paths and check if it starts with assets directory
               (normalized-base (expand-file-name base-name (ipe--find-project-root)))
               (normalized-assets (expand-file-name ipe-assets-directory (ipe--find-project-root)))
               (ipe-file (if (string-prefix-p (file-name-as-directory normalized-assets) 
                                             (file-name-as-directory normalized-base))
                            ;; Path already includes assets directory
                            (concat normalized-base ipe-default-extension)
                          ;; Simple filename - add to assets dir
                          (expand-file-name (concat base-name ipe-default-extension) assets-dir))))
          (if (file-exists-p ipe-file)
              (ipe--export-to-pdf ipe-file)
            (message "Ipe file %s not found" ipe-file)))
      ;; No file at point, let user select one
      (let* ((assets-dir (ipe--assets-directory))
             (ipe-files (directory-files assets-dir nil "\\.ipe$"))
             (selected-file (completing-read "Select Ipe file to export: " ipe-files)))
        (when selected-file
          (ipe--export-to-pdf (expand-file-name selected-file assets-dir)))))))

;;;###autoload
(define-minor-mode ipe-mode
  "Minor mode for Ipe vector graphics integration."
  :lighter " Ipe"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c i c") 'ipe-create-and-edit)
            (define-key map (kbd "C-c i e") 'ipe-edit-at-point)
            (define-key map (kbd "C-c i i") 'ipe-insert-latex-code)
            (define-key map (kbd "C-c i p") 'ipe-export-to-pdf)
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
