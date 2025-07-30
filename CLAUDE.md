# Ipe Mode Development Context

## Project Overview
Developed a complete Emacs minor mode (`ipe-mode`) for integrating Ipe vector graphics editor with org-mode and LaTeX. The mode provides seamless creation, editing, and PDF export of vector graphics figures.

## Key Features Implemented

### Core Functionality
- **`ipe-create-and-edit`** (`C-c i c`) - Creates new Ipe file in assets directory and opens in Ipe
- **`ipe-edit-at-point`** (`C-c i e`) - Opens Ipe file referenced by `\includegraphics` command under cursor
- **`ipe-insert-latex-code`** (`C-c i i`) - Inserts LaTeX code for existing Ipe files
- **`ipe-export-to-pdf`** (`C-c i p`) - Manually exports Ipe files to PDF

### Smart Path Handling
- Uses `project.el` for project root detection
- Handles various path formats: `./assets`, `assets/`, `../path/assets`, etc.
- Normalizes paths using Emacs built-in functions (`expand-file-name`, `file-name-as-directory`)
- Automatically converts between PDF references (for LaTeX) and IPE files (for editing)

### Export System
- **Process sentinel approach**: Only exports after Ipe process exits successfully
- Ensures PDF contains final saved version, not work-in-progress
- Configurable via `ipe-auto-export` variable
- Uses `ipetoipe -pdf` command for conversion

### Org-mode Integration
- Auto-enables in org-mode buffers
- Generates proper lowercase environments (`#+begin_figure`/`#+end_figure`)
- Includes LaTeX attributes, captions, and labels
- References PDF files in `\includegraphics` commands

## File Structure
```
/home/remi/Projects/Mine/emacs-config/
├── ipe-mode.el              # Main minor mode implementation
├── init.el                  # Updated to load ipe-mode
├── sample-document.org      # Example document with usage instructions
└── assets/
    ├── figure-20250731-1.ipe    # Example Ipe file
    ├── figure-20250731-1.pdf    # Exported PDF
    └── huynya-govna-1234.pdf    # Another test figure
```

## Configuration Variables
- `ipe-executable` - Path to Ipe executable (default: "ipe")
- `ipe-assets-directory` - Directory for storing Ipe files (default: "assets")
- `ipe-default-extension` - File extension for Ipe files (default: ".ipe")
- `ipe-auto-export` - Auto-export to PDF when process exits (default: t)

## Key Implementation Details

### Path Normalization Logic
```elisp
;; Normalize paths and check if it starts with assets directory
(normalized-base (expand-file-name base-name (ipe--find-project-root)))
(normalized-assets (expand-file-name ipe-assets-directory (ipe--find-project-root)))
(ipe-file (if (string-prefix-p (file-name-as-directory normalized-assets) 
                              (file-name-as-directory normalized-base))
             ;; Path already includes assets directory
             (concat normalized-base ipe-default-extension)
           ;; Simple filename - add to assets dir
           (expand-file-name (concat base-name ipe-default-extension) assets-dir)))
```

### Process Sentinel for Export
```elisp
(defun ipe--process-sentinel (process event ipe-file)
  "Sentinel function to export IPE-FILE to PDF when Ipe process exits."
  (when (and (memq (process-status process) '(exit signal))
             (file-exists-p ipe-file))
    (if ipe-auto-export
        (ipe--export-to-pdf ipe-file)
      (message "Ipe process finished. Use C-c i p to export to PDF."))))
```

## Usage Workflow
1. **Create figure**: `C-c i c` → Enter filename → Edit in Ipe → Close Ipe → Auto-export to PDF
2. **Edit existing**: Place cursor on `\includegraphics` line → `C-c i e` → Edit → Close → Auto-export
3. **Manual export**: `C-c i p` on any `\includegraphics` line
4. **Document export**: `C-c C-e l p` to export org document to PDF

## Recent Fixes
- Fixed path normalization to handle all variants (`./assets`, `assets/`, etc.)
- Changed to process sentinel approach for proper export timing
- Fixed org-mode environment case (lowercase `#+begin_figure`)
- Implemented proper PDF/IPE filename conversion logic

## Testing Status
- ✅ Basic file creation and editing
- ✅ Path normalization with various formats
- ✅ Process sentinel export on Ipe exit
- ✅ Org-mode integration with proper environments
- ✅ LaTeX export compatibility

## Next Steps (if needed)
- Add support for multi-page Ipe documents
- Implement figure template system
- Add integration with other LaTeX environments
- Consider adding figure numbering/referencing helpers