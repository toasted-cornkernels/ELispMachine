(defvar elispm/gc-cons-threshold gc-cons-threshold)
(defvar elispm/gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(setq package-enable-at-startup nil) ; I don't use package.el
