;;; menudo-theme.el --- Menudo theme (black backgrounds, gray text, blue highlights) -*- lexical-binding: t -*-

;; Author: PrawnFarmerDev
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces themes
;; URL: https://github.com/prawnfarmerdev/menudo-theme

;;; Commentary:
;; Menudo is a dark theme featuring:
;; - Pure black backgrounds for code and line numbers
;; - Bright gray text for readability
;; - Gold cursor with Evil mode cursor color support
;; - Gold mode-line with bright text
;; - Blue and gold highlights for completion
;; - Red parenthesis matching
;; - Comprehensive org-mode support
;; - Rainbow delimiters for code

;;; Code:

(deftheme menudo "Menudo theme with black backgrounds, gray text, and blue highlights.")

(defvar menudo-cursor-color         "#b58900")
(defvar menudo-replace-cursor-color "#dc322f")

(let* (;; Neutrals
       (menudo-fg              "#cccccc")
       (menudo-bg              "#111111")
       (menudo-bg-alt          "#1e1e1e")
       (menudo-hl-line         "#2a2a2a")
       (menudo-gray-dark       "#404040")
       (menudo-gray            "#666666")
       (menudo-gray-light      "#9ba290")

       ;; Yellows / Golds
       (menudo-gold            "#fcaa05")
       (menudo-gold-dark       "#9f5600")
       (menudo-gold-dim        "#b58900")
       (menudo-yellow          "#f0c674")
       (menudo-yellow-bright   "#edb211")
       (menudo-yellow-vivid    "#f0bb0c")

       ;; Oranges
       (menudo-orange          "#ffaa00")
       (menudo-orange-burnt    "#de451f")
       (menudo-orange-vivid    "#f0500c")

       ;; Reds
       (menudo-red             "#b3493f")
       (menudo-red-dusty       "#dc7575")

       ;; Blues
       (menudo-blue            "#268bd2")

       ;; Greens
       (menudo-green           "#8ea032")
       (menudo-green-dark      "#003939")
       (menudo-green-pepper    "#355e3b")
       (menudo-green-lime      "#003939")

       ;; Purples / Cyans
       (menudo-cyan            "#5fafaf")
       (menudo-lavender        "#af87d7")
       (menudo-teal            "#008080")

       ;; Browns
       (menudo-brown           "#bf9948")


       ;; Mode line
       (menudo-modeline-fg     "#cccccc")
       (menudo-modeline-bg     "#9f5600")
       (menudo-modeline-border "#161616"))

  (custom-theme-set-faces
   'menudo

   ;;=========================================================================
   ;; UI
   ;;=========================================================================
   `(default           ((t (:background ,menudo-bg              :foreground ,menudo-fg))))
   `(cursor            ((t (:background ,menudo-gold-dim))))
   `(region            ((t (:background ,menudo-green-dark))))
   `(highlight         ((t (:background ,menudo-green-dark))))
   `(fringe            ((t (:background ,menudo-bg))))
   `(vertical-border   ((t (:foreground ,menudo-bg))))
   `(shadow            ((t (:foreground ,menudo-gray-dark       :background ,menudo-bg))))
   `(minibuffer-prompt ((t (:foreground ,menudo-gold            :weight bold))))
   `(hl-line           ((t (:background ,menudo-hl-line))))

   ;;=========================================================================
   ;; LINE NUMBERS
   ;;=========================================================================
   `(line-number              ((t (:foreground ,menudo-gray-dark :background ,menudo-bg))))
   `(line-number-current-line ((t (:foreground ,menudo-gold-dark :background ,menudo-bg))))

   ;;=========================================================================
   ;; FONT LOCK
   ;;=========================================================================
   `(font-lock-comment-face       ((t (:foreground ,menudo-gray))))
   `(font-lock-keyword-face       ((t (:foreground ,menudo-yellow))))
   `(font-lock-string-face        ((t (:foreground ,menudo-orange))))
   `(font-lock-constant-face      ((t (:foreground ,menudo-orange))))
   `(font-lock-builtin-face       ((t (:foreground ,menudo-red-dusty))))
   `(font-lock-preprocessor-face  ((t (:foreground ,menudo-red-dusty))))
   `(font-lock-type-face          ((t (:foreground ,menudo-yellow-bright))))
   `(font-lock-function-name-face ((t (:foreground ,menudo-orange-burnt))))
   `(font-lock-variable-name-face ((t (:foreground ,menudo-fg))))
   `(font-lock-variable-use-face  ((t (:foreground ,menudo-lavender))))
   `(font-lock-warning-face       ((t (:foreground ,menudo-red      :weight bold))))
   `(font-lock-doc-face           ((t (:foreground ,menudo-green))))

   ;;=========================================================================
   ;; MODE LINE
   ;;=========================================================================
   `(mode-line
     ((t (:background ,menudo-modeline-bg
          :foreground ,menudo-modeline-fg
          :box (:line-width 1 :color ,menudo-modeline-border :style nil)))))
   `(mode-line-inactive
     ((t (:background ,menudo-gray
          :foreground ,menudo-fg
          :box (:line-width 1 :color ,menudo-modeline-border :style nil)))))
   `(mode-line-buffer-id ((t (:foreground ,menudo-orange :weight bold))))

   ;;=========================================================================
   ;; GIT / VC
   ;;=========================================================================
   `(magit-branch-local   ((t (:foreground ,menudo-green))))
   `(magit-branch-remote  ((t (:foreground ,menudo-blue))))
   `(magit-branch-current ((t (:foreground ,menudo-gold   :weight bold))))
   `(vc-mode              ((t (:foreground ,menudo-gold))))
   `(diff-hl-change       ((t (:background ,menudo-blue   :foreground ,menudo-blue))))
   `(diff-hl-insert       ((t (:background ,menudo-green  :foreground ,menudo-green))))
   `(diff-hl-delete       ((t (:background ,menudo-red    :foreground ,menudo-red))))

   ;;=========================================================================
   ;; SEARCH & MATCHING
   ;;=========================================================================
   `(match          ((t (:background ,menudo-yellow-vivid  :foreground ,menudo-bg))))
   `(isearch        ((t (:background ,menudo-orange-vivid  :foreground ,menudo-bg))))
   `(lazy-highlight ((t (:background ,menudo-yellow-vivid  :foreground ,menudo-bg))))
   `(ido-first-match ((t (:foreground ,menudo-yellow-vivid))))
   `(ido-only-match  ((t (:foreground ,menudo-orange-vivid))))

   ;;=========================================================================
   ;; COMPLETION (VERTICO / CORFU / ORDERLESS)
   ;;=========================================================================
   `(vertico-current              ((t (:background ,menudo-green-pepper :foreground ,menudo-fg))))
   `(orderless-match-face-0       ((t (:foreground ,menudo-yellow-vivid :weight bold))))
   `(orderless-match-face-1       ((t (:foreground ,menudo-blue         :weight bold))))
   `(orderless-match-face-2       ((t (:foreground ,menudo-green        :weight bold))))
   `(orderless-match-face-3       ((t (:foreground ,menudo-red-dusty    :weight bold))))
   `(completions-first-difference ((t (:foreground ,menudo-gold         :weight bold))))
   `(completions-common-part      ((t (:foreground ,menudo-fg))))

   `(corfu-current ((t (:background ,menudo-green-lime :foreground ,menudo-bg))))
   `(corfu-default ((t (:background ,menudo-bg-alt))))
   `(corfu-bar     ((t (:background ,menudo-blue))))
   `(corfu-border  ((t (:background ,menudo-gray-dark))))

   ;;=========================================================================
   ;; PARENTHESES
   ;;=========================================================================
   `(show-paren-match    ((t (:background ,menudo-gray-dark))))
   `(show-paren-mismatch ((t (:background ,menudo-lavender))))

   ;;=========================================================================
   ;; RAINBOW DELIMITERS
   ;;=========================================================================
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,menudo-yellow))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,menudo-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,menudo-red))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,menudo-red-dusty))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,menudo-gold))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,menudo-yellow-bright))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,menudo-orange))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,menudo-orange-vivid))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,menudo-cyan))))

   ;;=========================================================================
   ;; LSP & DIAGNOSTICS
   ;;=========================================================================
   `(eglot-highlight-symbol-face   ((t (:background ,menudo-bg-alt))))
   `(eglot-diagnostic-error-face   ((t (:underline (:color ,menudo-red    :style wave)))))
   `(eglot-diagnostic-warning-face ((t (:underline (:color ,menudo-gold   :style wave)))))
   `(eglot-diagnostic-note-face    ((t (:underline (:color ,menudo-blue   :style wave)))))
   `(eglot-diagnostic-hint-face    ((t (:underline (:color ,menudo-green  :style wave)))))

   `(flycheck-error   ((t (:underline (:color ,menudo-red  :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,menudo-gold :style wave)))))
   `(flycheck-info    ((t (:underline (:color ,menudo-blue :style wave)))))

   ;;=========================================================================
   ;; COMPILATION
   ;;=========================================================================
   `(compilation-error          ((t (:foreground ,menudo-red))))
   `(compilation-info           ((t (:foreground ,menudo-green))))
   `(compilation-warning        ((t (:foreground ,menudo-brown  :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,menudo-red    :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,menudo-green  :weight bold))))

   ;;=========================================================================
   ;; ORG MODE
   ;;=========================================================================
   `(org-level-1 ((t (:foreground ,menudo-gold         :weight bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,menudo-brown        :weight bold :height 1.2))))
   `(org-level-3 ((t (:foreground ,menudo-red          :weight bold :height 1.1))))
   `(org-level-4 ((t (:foreground ,menudo-blue         :weight bold))))
   `(org-level-5 ((t (:foreground ,menudo-red-dusty    :weight bold))))
   `(org-level-6 ((t (:foreground ,menudo-lavender     :weight bold))))
   `(org-level-7 ((t (:foreground ,menudo-cyan         :weight bold))))
   `(org-level-8 ((t (:foreground ,menudo-orange       :weight bold))))

   `(org-document-title        ((t (:foreground ,menudo-red  :weight bold :height 1.5))))
   `(org-document-info         ((t (:foreground ,menudo-cyan))))
   `(org-document-info-keyword ((t (:foreground ,menudo-gray))))

   `(org-list-dt                  ((t (:foreground ,menudo-gold  :weight bold))))
   `(org-checkbox                 ((t (:foreground ,menudo-gold  :weight bold))))
   `(org-checkbox-statistics-todo ((t (:foreground ,menudo-red-dusty))))
   `(org-checkbox-statistics-done ((t (:foreground ,menudo-green))))

   `(org-todo          ((t (:foreground ,menudo-red       :weight bold))))
   `(org-done          ((t (:foreground ,menudo-green     :weight bold))))
   `(org-headline-todo ((t (:foreground ,menudo-red-dusty))))
   `(org-headline-done ((t (:foreground ,menudo-gray      :strike-through t))))

   `(org-special-keyword ((t (:foreground ,menudo-gray))))
   `(org-property-value  ((t (:foreground ,menudo-cyan))))
   `(org-drawer          ((t (:foreground ,menudo-gray))))
   `(org-meta-line       ((t (:foreground ,menudo-gray))))

   `(org-link ((t (:foreground ,menudo-blue :underline t))))
   `(org-tag  ((t (:foreground ,menudo-gold :weight bold))))

   `(org-block            ((t (:background ,menudo-bg-alt :foreground ,menudo-fg :extend t))))
   `(org-block-begin-line ((t (:foreground ,menudo-gray   :background ,menudo-bg :extend t))))
   `(org-block-end-line   ((t (:foreground ,menudo-gray   :background ,menudo-bg :extend t))))
   `(org-code             ((t (:foreground ,menudo-orange  :background ,menudo-bg-alt))))
   `(org-verbatim         ((t (:foreground ,menudo-green   :background ,menudo-bg-alt))))

   `(org-table ((t (:foreground ,menudo-cyan))))

   `(org-date                 ((t (:foreground ,menudo-lavender :underline t))))
   `(org-time-grid            ((t (:foreground ,menudo-yellow-vivid))))
   `(org-upcoming-deadline    ((t (:foreground ,menudo-red))))
   `(org-scheduled            ((t (:foreground ,menudo-green))))
   `(org-scheduled-today      ((t (:foreground ,menudo-gold    :weight bold))))
   `(org-scheduled-previously ((t (:foreground ,menudo-red-dusty))))

   `(org-priority ((t (:foreground ,menudo-orange-vivid :weight bold))))

   `(org-agenda-structure  ((t (:foreground ,menudo-blue         :weight bold))))
   `(org-agenda-date       ((t (:foreground ,menudo-cyan))))
   `(org-agenda-date-today ((t (:foreground ,menudo-gold         :weight bold :height 1.2))))
   `(org-agenda-done       ((t (:foreground ,menudo-gray))))

   `(org-footnote ((t (:foreground ,menudo-lavender :underline t))))

   `(org-bold   ((t (:foreground ,menudo-fg :weight bold))))
   `(org-italic ((t (:foreground ,menudo-fg :slant italic))))

   ;;=========================================================================
   ;; ORG-MODERN
   ;;=========================================================================
   `(org-modern-tag        ((t (:background ,menudo-green-pepper :foreground ,menudo-gold))))
   `(org-modern-priority   ((t (:background ,menudo-bg-alt       :foreground ,menudo-orange-vivid))))
   `(org-modern-todo       ((t (:background ,menudo-bg-alt       :foreground ,menudo-red    :weight bold))))
   `(org-modern-done       ((t (:background ,menudo-bg-alt       :foreground ,menudo-brown  :weight bold))))
   `(org-modern-date       ((t (:background ,menudo-bg-alt       :foreground ,menudo-lavender))))
   `(org-modern-time       ((t (:background ,menudo-bg-alt       :foreground ,menudo-yellow-vivid))))
   `(org-modern-statistics ((t (:background ,menudo-bg-alt       :foreground ,menudo-cyan))))

   ;;=========================================================================
   ;; TOOLTIP
   ;;=========================================================================
   `(tooltip ((t (:background ,menudo-brown :foreground ,menudo-gold))))))

;;=========================================================================
;; THEME REGISTRATION
;;=========================================================================

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'menudo)

(when (or (display-graphic-p) (daemonp))
  (enable-theme 'menudo))

;;=========================================================================
;; EVIL CURSOR COLORS
;;=========================================================================

(when (boundp 'evil-normal-state-cursor)
  (setq evil-normal-state-cursor   `(,menudo-cursor-color box)
        evil-insert-state-cursor   `(,menudo-cursor-color (bar . 6))
        evil-visual-state-cursor   `(,menudo-cursor-color (bar . 12))
        evil-replace-state-cursor  `(,menudo-replace-cursor-color box)
        evil-operator-state-cursor `(,menudo-cursor-color hollow)))

;;=========================================================================
;; HL-LINE & CURSOR SETUP
;;=========================================================================

(add-hook 'prog-mode-hook #'hl-line-mode)
(setq-default cursor-in-non-selected-windows nil
              cursor-type 'box)

;;=========================================================================
;; DAEMON MODE SUPPORT
;;=========================================================================

(defun menudo--enable-for-frame (&optional frame)
  "Enable Menudo theme for FRAME if it is a graphical frame."
  (with-selected-frame (or frame (selected-frame))
    (when (and (display-graphic-p)
               (not (memq 'menudo custom-enabled-themes)))
      (enable-theme 'menudo))))

(add-hook 'after-make-frame-functions #'menudo--enable-for-frame)

(provide 'menudo-theme)
;;; menudo-theme.el ends here
