(ns game.gui-main
  "AOT entry point for the packaged GUI jar.

  This namespace is deliberately tiny and gen-class'd so it can serve as the
  jar's Main-Class. It requires game.gui at *runtime* rather than at the top of
  the namespace, which keeps cljfx/JavaFX from being loaded during AOT
  compilation (loading cljfx starts the JavaFX toolkit and breaks AOT)."
  (:gen-class))

(defn -main [& args]
  (require 'game.gui)
  (apply (resolve 'game.gui/-main) args))
