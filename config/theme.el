;;; theme.el --- Menudo theme (black backgrounds, gray text, blue highlights) -*- lexical-binding: t -*-

;;; Commentary:
;; Menudo theme - A custom dark theme featuring:
;; - Pure black backgrounds for code and line numbers
;; - Bright gray text for readability
;; - Yellow cursor with Evil mode cursor colors
;; - Green mode-line with bright text
;; - Blue and gold highlights for completion
;; - Red parentheses matching
;; - Comprehensive org-mode support with beautiful colors
;; - Rainbow delimiters for code
;;
;; Originally based on Fleury theme by Shams Parvez Arka:
;; https://github.com/ShamsParvezArka/fleury-theme.el

;;==============================================================================
;; MENUDO THEME
;;==============================================================================

(deftheme menudo "Menudo theme with black backgrounds, gray text, and blue highlights")

;; Color palette
(let* ((almost-white       "#cccccc")     ; Default text
       (charcoal-gray-lite "#1e1e1e")
       (amber-gold         "#fcaa05")
       (medium-gray        "#404040")
       (dim-gray           "#666666")
       (goldenrod          "#f0c674")
       (bright-orange      "#ffaa00")
       (dusty-rose         "#dc7575")
       (sunflower-yellow   "#edb211")
       (burnt-orange       "#de451f")
       (sky-blue           "#268bd2")
       (bright-red         "#dc322f")
       (fresh-green        "#66bc11")
       (lime-green         "#003939")
       (vivid-vermilion    "#f0500c")
        (golden-yellow      "#f0bb0c")
        (solarized-yellow   "#b58900")
       (pure-black         "#000000")
       (dusty-sage         "#9ba290")
       (coffee-brown       "#63523d")
       (bellpepper-green   "#355e3b")
       (soft-cyan          "#5fafaf")
       (lavender           "#af87d7")
       (teal               "#008080")
       
       (mode-line-foreground-active almost-white)
       (mode-line-background-active bellpepper-green)
       (mode-line-border            "#161616"))

  (custom-theme-set-faces
   'menudo

   ;;==========================================================================
   ;; UI ELEMENTS
   ;;==========================================================================
   `(default           ((t (:background ,pure-black :foreground ,almost-white))))
   `(cursor            ((t (:background "#b58900"))))
   `(region            ((t (:background ,lime-green))))
   `(highlight         ((t (:background ,lime-green))))
   `(fringe            ((t (:background ,pure-black))))
   `(vertical-border   ((t (:foreground ,pure-black))))
   `(shadow            ((t (:foreground ,medium-gray :background ,pure-black))))
   `(minibuffer-prompt ((t (:foreground ,amber-gold :weight bold))))

   ;;==========================================================================
   ;; LINE NUMBERS
   ;;==========================================================================
    `(line-number              ((t (:foreground ,solarized-yellow :background ,pure-black))))
   `(line-number-current-line ((t (:background ,pure-black :foreground ,almost-white))))

   ;;==========================================================================
   ;; FONT LOCK (CODE HIGHLIGHTING)
   ;;==========================================================================
   `(font-lock-comment-face       ((t (:foreground ,dim-gray))))
   `(font-lock-keyword-face       ((t (:foreground ,goldenrod))))
   `(font-lock-string-face        ((t (:foreground ,bright-orange))))
   `(font-lock-constant-face      ((t (:foreground ,bright-orange))))
   `(font-lock-builtin-face       ((t (:foreground ,dusty-rose))))
   `(font-lock-preprocessor-face  ((t (:foreground ,dusty-rose))))
   `(font-lock-type-face          ((t (:foreground ,sunflower-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,burnt-orange))))
   `(font-lock-variable-name-face ((t (:foreground ,almost-white))))
   `(font-lock-variable-use-face  ((t (:foreground ,sky-blue))))
   `(font-lock-warning-face       ((t (:foreground ,bright-red :weight bold))))
   `(font-lock-doc-face           ((t (:foreground ,fresh-green))))

   ;;==========================================================================
   ;; MODE LINE
   ;;==========================================================================
   `(mode-line          ((t (:background ,mode-line-background-active 
                            :foreground ,mode-line-foreground-active 
                            :box (:line-width 1 :color ,mode-line-border :style nil)))))
   `(mode-line-inactive ((t (:background ,dim-gray 
                            :foreground ,almost-white 
                            :box (:line-width 1 :color ,mode-line-border :style nil)))))
   `(mode-line-buffer-id ((t (:foreground ,bright-orange :weight bold))))

   ;;==========================================================================
   ;; GIT / MAGIT
   ;;==========================================================================
   `(magit-branch-local   ((t (:foreground ,fresh-green))))
   `(magit-branch-remote  ((t (:foreground ,sky-blue))))
   `(magit-branch-current ((t (:foreground ,amber-gold :weight bold))))
   `(vc-mode              ((t (:foreground ,amber-gold))))
   `(diff-hl-change       ((t (:background ,sky-blue :foreground ,sky-blue))))
   `(diff-hl-insert       ((t (:background ,fresh-green :foreground ,fresh-green))))
   `(diff-hl-delete       ((t (:background ,bright-red :foreground ,bright-red))))

   ;;==========================================================================
   ;; SEARCH & MATCHING
   ;;==========================================================================
   `(match           ((t (:background ,golden-yellow :foreground ,pure-black))))
   `(isearch         ((t (:background ,vivid-vermilion :foreground ,pure-black))))
   `(lazy-highlight  ((t (:background ,golden-yellow :foreground ,pure-black))))
   `(ido-first-match ((t (:foreground ,golden-yellow))))
   `(ido-only-match  ((t (:foreground ,vivid-vermilion))))

   ;;==========================================================================
   ;; COMPLETION (VERTICO, CORFU, ORDERLESS)
   ;;==========================================================================
   `(vertico-current            ((t (:background ,lime-green :foreground ,almost-white))))
   `(orderless-match-face-0     ((t (:foreground ,golden-yellow :weight bold))))
   `(orderless-match-face-1     ((t (:foreground ,sky-blue :weight bold))))
   `(orderless-match-face-2     ((t (:foreground ,fresh-green :weight bold))))
   `(orderless-match-face-3     ((t (:foreground ,dusty-rose :weight bold))))
   `(completions-first-difference ((t (:foreground ,amber-gold :weight bold))))
   `(completions-common-part    ((t (:foreground ,almost-white))))
   
   `(corfu-current ((t (:background ,lime-green :foreground ,pure-black))))
   `(corfu-default ((t (:background ,charcoal-gray-lite))))
   `(corfu-bar     ((t (:background ,sky-blue))))
   `(corfu-border  ((t (:background ,medium-gray))))

   ;;==========================================================================
   ;; PARENTHESES
   ;;==========================================================================
   `(show-paren-match    ((t (:background ,vivid-vermilion))))
   `(show-paren-mismatch ((t (:background ,dusty-sage))))

   ;;==========================================================================
   ;; RAINBOW DELIMITERS
   ;;==========================================================================
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,sky-blue))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,fresh-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,amber-gold))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dusty-rose))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,goldenrod))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,sunflower-yellow))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,bright-orange))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,vivid-vermilion))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,soft-cyan))))

   ;;==========================================================================
   ;; LSP & DIAGNOSTICS
   ;;==========================================================================
   `(eglot-highlight-symbol-face   ((t (:background ,charcoal-gray-lite))))
   `(eglot-diagnostic-error-face   ((t (:underline (:color ,bright-red :style wave)))))
   `(eglot-diagnostic-warning-face ((t (:underline (:color ,amber-gold :style wave)))))
   `(eglot-diagnostic-note-face    ((t (:underline (:color ,sky-blue :style wave)))))
   `(eglot-diagnostic-hint-face    ((t (:underline (:color ,fresh-green :style wave)))))
   
   `(flycheck-error   ((t (:underline (:color ,bright-red :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,amber-gold :style wave)))))
   `(flycheck-info    ((t (:underline (:color ,sky-blue :style wave)))))

   ;;==========================================================================
   ;; COMPILATION
   ;;==========================================================================
   `(compilation-error          ((t (:foreground ,bright-red))))
   `(compilation-info           ((t (:foreground ,fresh-green))))
   `(compilation-warning        ((t (:foreground ,coffee-brown :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,bright-red :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,fresh-green :weight bold))))

   ;;==========================================================================
   ;; ORG MODE
   ;;==========================================================================
   ;; Headings - red, yellow, green, blue, pink, purple, cyan hierarchy
   `(org-level-1 ((t (:foreground ,amber-gold :weight bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,bright-red :weight bold :height 1.2))))
   `(org-level-3 ((t (:foreground ,fresh-green :weight bold :height 1.1))))
   `(org-level-4 ((t (:foreground ,sky-blue :weight bold))))
   `(org-level-5 ((t (:foreground ,dusty-rose :weight bold))))
   `(org-level-6 ((t (:foreground ,lavender :weight bold))))
   `(org-level-7 ((t (:foreground ,soft-cyan :weight bold))))
   `(org-level-8 ((t (:foreground ,bright-orange :weight bold))))

   ;; Document structure
   `(org-document-title ((t (:foreground ,bright-red :weight bold :height 1.5))))
   `(org-document-info  ((t (:foreground ,soft-cyan))))
   `(org-document-info-keyword ((t (:foreground ,dim-gray))))

   ;; Lists and checkboxes
   `(org-list-dt ((t (:foreground ,amber-gold :weight bold))))
   `(org-checkbox ((t (:foreground ,amber-gold :weight bold))))
   `(org-checkbox-statistics-todo ((t (:foreground ,dusty-rose))))
   `(org-checkbox-statistics-done ((t (:foreground ,fresh-green))))

   ;; TODO keywords
   `(org-todo          ((t (:foreground ,bright-red :weight bold))))
   `(org-done          ((t (:foreground ,fresh-green :weight bold))))
   `(org-headline-todo ((t (:foreground ,dusty-rose))))
   `(org-headline-done ((t (:foreground ,dim-gray :strike-through t))))

   ;; Special keywords and properties
   `(org-special-keyword ((t (:foreground ,dim-gray))))
   `(org-property-value  ((t (:foreground ,soft-cyan))))
   `(org-drawer          ((t (:foreground ,dim-gray))))
   `(org-meta-line       ((t (:foreground ,dim-gray))))

   ;; Links
   `(org-link ((t (:foreground ,sky-blue :underline t))))
   `(org-tag  ((t (:foreground ,amber-gold :weight bold))))

   ;; Code blocks
   `(org-block            ((t (:background ,charcoal-gray-lite :foreground ,almost-white :extend t))))
   `(org-block-begin-line ((t (:foreground ,dim-gray :background ,pure-black :extend t))))
   `(org-block-end-line   ((t (:foreground ,dim-gray :background ,pure-black :extend t))))
   `(org-code             ((t (:foreground ,bright-orange :background ,charcoal-gray-lite))))
   `(org-verbatim         ((t (:foreground ,fresh-green :background ,charcoal-gray-lite))))

   ;; Tables
   `(org-table ((t (:foreground ,soft-cyan))))

   ;; Dates and timestamps
   `(org-date         ((t (:foreground ,lavender :underline t))))
   `(org-time-grid    ((t (:foreground ,golden-yellow))))
   `(org-upcoming-deadline ((t (:foreground ,bright-red))))
   `(org-scheduled        ((t (:foreground ,fresh-green))))
   `(org-scheduled-today  ((t (:foreground ,amber-gold :weight bold))))
   `(org-scheduled-previously ((t (:foreground ,dusty-rose))))

   ;; Priorities
   `(org-priority ((t (:foreground ,vivid-vermilion :weight bold))))

   ;; Agenda
   `(org-agenda-structure ((t (:foreground ,sky-blue :weight bold))))
   `(org-agenda-date      ((t (:foreground ,soft-cyan))))
   `(org-agenda-date-today ((t (:foreground ,amber-gold :weight bold :height 1.2))))
   `(org-agenda-done      ((t (:foreground ,dim-gray))))

   ;; Footnotes
   `(org-footnote ((t (:foreground ,lavender :underline t))))

   ;; Emphasis
   `(org-bold   ((t (:foreground ,almost-white :weight bold))))
   `(org-italic ((t (:foreground ,almost-white :slant italic))))

   ;;==========================================================================
   ;; ORG-MODERN (if installed)
   ;;==========================================================================
   `(org-modern-tag       ((t (:background ,bellpepper-green :foreground ,amber-gold))))
   `(org-modern-priority  ((t (:background ,charcoal-gray-lite :foreground ,vivid-vermilion))))
   `(org-modern-todo      ((t (:background ,charcoal-gray-lite :foreground ,bright-red :weight bold))))
   `(org-modern-done      ((t (:background ,charcoal-gray-lite :foreground ,fresh-green :weight bold))))
   `(org-modern-date      ((t (:background ,charcoal-gray-lite :foreground ,lavender))))
   `(org-modern-time      ((t (:background ,charcoal-gray-lite :foreground ,golden-yellow))))
   `(org-modern-statistics ((t (:background ,charcoal-gray-lite :foreground ,soft-cyan))))

   ;;==========================================================================
   ;; TOOLTIP & MISC
   ;;==========================================================================
   `(tooltip ((t (:background ,coffee-brown :foreground ,amber-gold))))
   ))

;;==============================================================================
;; THEME ACTIVATION
;;==============================================================================

;; Add directory to custom theme load path
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'menudo)

;; Enable the theme in graphical mode or daemon mode
(when (or (display-graphic-p) (daemonp))
  (enable-theme 'menudo)
  (message "Menudo theme enabled"))

;;==============================================================================
;; EVIL CURSOR COLORS
;;==============================================================================

(when (boundp 'evil-normal-state-cursor)
  (setq evil-normal-state-cursor   '("#b58900" box)
        evil-insert-state-cursor   '("#b58900" (bar . 6))
        evil-visual-state-cursor   '("#b58900" (bar . 12))
        evil-replace-state-cursor  '("#dc322f" box)
        evil-operator-state-cursor '("#b58900" hollow)))

;;==============================================================================
;; HL-LINE & CURSOR SETUP
;;==============================================================================

(require 'hl-line)
(set-face-attribute 'hl-line nil :background "#000000")

(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-in-non-selected-windows nil)
(setq-default cursor-type 'box)

;;==============================================================================
;; DAEMON MODE SUPPORT
;;==============================================================================

;; Ensure theme is enabled for new graphical frames (especially in daemon mode)
(defun menudo-enable-if-needed (&optional frame)
  "Enable Menudo theme if not already enabled and FRAME is graphical."
  (with-selected-frame (or frame (selected-frame))
    (when (and (display-graphic-p)
               (not (memq 'menudo custom-enabled-themes)))
      (enable-theme 'menudo)
      (message "Menudo theme enabled for new graphical frame"))))

(add-hook 'after-make-frame-functions #'menudo-enable-if-needed)

(provide 'theme)
;;; theme.el ends here
