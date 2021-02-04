(import path)

(defn os/is-dir? [path] 
    (if-let [info (os/stat path)]
        (= (info :mode) :directory)
        false))
        
(defn os/is-file? [path] 
    (if-let [info (os/stat path)]
        (= (info :mode) :file)
        false))
        
(defn array/shift [arr] 
    (def ret (arr 0))
    (array/remove arr 0)
    ret
)

(var *exclude-exts* @{
    ".png" true
    ".docx" true
    ".ttf" true
    ".jpg" true
    ".jpeg" true
    ".so" true
    ".exe" true
    ".dll" true
})

(var *commands* @{})
(var *command-order* @[])

(defn add-command [func name desc &opt long-desc] 
    (array/push *command-order* name)
    (put *commands* name { 
        :name name
        :short-desc desc
        :long-desc (or long-desc "")
        :func func 
    })
)

(defn- proc-anno-match [& args] 
    (def args (array (splice args)))
    (if (< (length args) 2) 
        (break nil)
    )
    (def start-pos (array/shift args))
    (def end-pos (array/pop args))
    (def type (string/trim (array/shift args)))
    (def args (map (fn [x] (string/trim x " \t\n:")) args))
    
    (def dict-args @{})
    (def pos-args @[])
    # So, now that we've shifted everything off, we can do some stuff
    (loop 
        [i :range [0 (length args) 2]]
        (def k (args i))
        (def v (args (+ i 1)))
        (match k 
            "" (array/push pos-args v)
            _  (put dict-args k v)
        )
    )
    
    (def ret 
    @{ 
        :range ~(,start-pos ,end-pos)
        :type type
        :args @{
            :kv dict-args
            :pos pos-args
        }
    })
    ret
)


(defn anno-scan [data] 
    (def anno-pat (peg/compile ~{
        :id-char (* (+ "-" "+" (range "09" "az" "AZ")))
        :anno-key (* (some :id-char) ":")
        :anno-value (* 
            (any " ")
            (any 
                (+ 
                    (* "[" :anno-value "]") 
                    (if-not (+ "]" "|") 1)
        )))
        :anno-pair (* 
            (capture (any :anno-key)) 
            (capture :anno-value)
            (any "|")
        )
        :type (capture (some :id-char))
        :anno-args (* "[" :s* (any :anno-pair) "]" )
        :main (any (+ (cmt (* ($) "@" :type :anno-args ($) ) ,proc-anno-match) 1))
    }))
    
    (peg/match anno-pat data)
)


(defn proc-statusable-entry [path hit spec] 
    (defn hit-data [key &opt dflt] 
        (get-in hit [:args :kv key] dflt)
    )
    (def status (hit-data (spec :status-field)))
    
    (if ((spec :show?) hit)
    {
        :id (hit-data "id")
        :filename (path/join (splice (array/slice (path/parts path) -3 -1)))
        :desc (string/join (get-in hit [:args :pos]) "\r\n")
        :status status
        :pp (spec :pp) 
    })
)


(defn base-show-file? [path] 
    (not (or
        (string/has-prefix? "." (path/basename path))
        (get *exclude-exts* (path/ext path) false)
        (if (string/find ".git" path) true false)
    ))
)

(defn base-list-dir? [path] 
    (and 
        (= (slice ((os/stat path) :permissions) 0 3) "rwx")
        (not (or
            (string/has-prefix? "." (path/basename path))
            (if (string/find ".git" path) true false)
            (os/is-file? (path/join path ".anno-ignore"))
        ))
    )
)


(defn walk-rec [path func &opt list-dir? show-file?] 
    (if (not= (string/find ".git" path) nil)
        (break)
    )
    (default list-dir? base-list-dir?)
    (default show-file? base-show-file?)
    (each p (os/dir path)
        (def currP (path/abspath (path/join path p)))
        (def info (os/stat currP))
        (match (info :mode)
            :file (if (show-file? currP) (func currP))
            :directory (if (list-dir? currP) (walk-rec currP func))
            _ false
        )
    )
)

(defn array/push-not-nil [arr val] 
    (match val
        nil ()
        x (array/push arr val)
    )
)

(defn print-hits [hits spec] 
    (if (> (length hits) 0)
        (do
            (print "")
            (print (spec :title))
            (print "")
            (each h hits 
                (:pp h)
            )
        )
        (print (spec :if-empty))
    )
)

(defn entry-fn [proc-hit] 
    (fn [path]
        (def to-search (slurp path))
        (def results (anno-scan to-search))
        (each hit results
            (proc-hit path hit)
        )
    )
)
