
(ns respo.app.comp.zero (:require [respo.macros :refer [defcomp div]]))

(defcomp comp-zero () (div {:inner-text 0}))
