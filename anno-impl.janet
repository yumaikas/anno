(import "path")
(import "date")

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

(defn- os/is-dir? [path] 
    (if-let [info (os/stat path)]
        (= (info :mode) :directory)
        false))
        
(defn- os/is-file? [path] 
    (if-let [info (os/stat path)]
        (= (info :mode) :file)
        false))

(defn- base-show-file? [path] 
    (not (or
        (string/has-prefix? "." (path/basename path))
        (get *exclude-exts* (path/ext path) false)
        (if (string/find ".git" path) true false)
    ))
)

(defn- base-list-dir? [path] 
    (not (or
        (string/has-prefix? "." (path/basename path))
        (if (string/find ".git" path) true false)
        (os/is-file? (path/join path ".anno-ignore"))
    ))
)

(defn- walk-rec [path func &opt list-dir? show-file?] 
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

(defn- walk-rec-dirs [path func &opt list-dir?] 
    (if (not= (string/find ".git" path) nil)
        (break)
    )
    (default list-dir? base-list-dir?)
    (each p (os/dir path)
        (def currP (path/abspath (path/join path p)))
        (def info (os/stat currP))
        (match (info :mode)
            :directory (if 
                (list-dir? currP) 
                (do 
                    (func currP)
                    (walk-rec currP func)
                )
            )
            _ false
        )
    )
)

(defn- print-rec [path]
    (walk-rec path (fn [x] (print x)))
)

(defn array/shift [arr] 
    (def ret (arr 0))
    (array/remove arr 0)
    ret
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

(defn- scan-file [path] 
    (def to-search (slurp path))
    (def results (anno-scan to-search))
    (if (> (length results) 0) 
        (print path)
    )
    (each hit results
        (print (hit :type))
        (put hit :range nil)
        (pp (table/to-struct hit))
    )
)

(defn- scan-rec [path]
    (walk-rec path scan-file)
)

(defn- is-todo-valid? [todo] 
   true 
)

(defn- array/push-not-nil [arr val] 
    (match val
        nil ()
        x (array/push arr val)
    )
)

(defn- proc-dated-entry [path hit spec] 
    (defn hit-data [key &opt dflt] 
        (get-in hit [:args :kv key] dflt)
    )
    (var date-on nil)
    (var date-ts nil)
    (if-let [
        l-date-on (hit-data (spec :datefield))
        parsed-date-on (date/parse-ymdstr l-date-on)]
        (do 
            (set date-on (os/date parsed-date-on))
            (set date-ts parsed-date-on)
        )
        (break nil)
    )
    (def saturday (date/next "Saturday"))
    (def last-saturday (date/add-time saturday {:days -7}))
    
    (if (<= last-saturday date-ts saturday)
    {
        :filename (path/join (splice (array/slice (path/parts path) -3 -1)))
        :desc (string/join (get-in hit [:args :pos]) "\r\n")
        (keyword (spec :prefix) "-ts") date-ts
        (keyword (spec :prefix) "-on") (date/to-ymdstr date-on)
        :pp (spec :pp) 
    })
)


(defn- print-hits [hits spec] 
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


(defn- review [path] 
    # ; Today I learned something
    (def til @[])
    
    # ; Closes over the til collection
    (defn proc-hit [path hit]
        (match (hit :type)
            "til" (array/push-not-nil til (proc-dated-entry path hit {
                :prefix "learned"
                :datefield "on"
                :pp (fn [self] 
                    (print (string "date: " (self :learned-on) " | " (self :desc) " | filename: " (self :filename)))
                )
            }))
        )
    )

    (walk-rec path (entry-fn proc-hit))
    
    (print-hits til 
        {:title "# Learning review"
        :if-empty "No TILs this week"})
)

(defn- proc-flaggable-entry [path hit spec] 
    (defn hit-data [key &opt dflt] 
        (get-in hit [:args :kv key] dflt)
    )
    (def status (hit-data (spec :status-field)))
    
    (if ((spec :show?) status)
    {
        :id (hit-data "id")
        :project (hit-data "project" nil)
        :filename (path/join (splice (array/slice (path/parts path) -3 -1)))
        :desc (string/join (get-in hit [:args :pos]) "\r\n")
        :status status
        :pp (spec :pp) 
    })
)

(defn- scan-tasks [path] 
    (def tasks @[])
    
    (defn proc-hit [path hit]
        (match (hit :type)
            "task" (array/push-not-nil tasks (proc-flaggable-entry path hit {
                :show? (fn [status] (not= status "done"))
                :status-field "status"
                :pp (fn [self] 
                    (prin (string "id: " (self :id) " | " (self :desc)))
                    (if (self :project) 
                        (prin (string " | project: " (self :project))))
                    (prin (string " | filename: " (self :filename)))
                    (print "")
                )
            }))
        )
    )
    (walk-rec path (entry-fn proc-hit))
    (print-hits tasks 
        {:title "# Tasks"
        :if-empty "No tasks are currently incomplete"})
)

(defn- scan-projects [path]
    (def projects @[])
    
    (defn proc-hit [path hit]
        (match (hit :type)
            "project" (array/push-not-nil projects (proc-flaggable-entry path hit {
                :show? (fn [status] (not= status "done"))
                :status-field "status"
                :pp (fn [self] 
                    (prin (string "id: " (self :id) " | " (self :desc)))
                    (if (self :project) 
                        (prin (string " | project: " (self :project))))
                    (prin (string " | filename: " (self :filename)))
                    (print "")
                )
            }))
        )
    )
    (walk-rec path (entry-fn proc-hit))
    (print-hits projects 
        {:title "# Projects"
        :if-empty "You don't have any projects defined"})
)

(defn- agenda [path] 
    # ; Todos are things that should be done during a given period of time
    (def todos @[])
    # ; Appointments, which Will Not Be Missed
    (def appt @[])
    
    (defn proc-hit [path hit]
        (match (hit :type)
            "todo" (array/push-not-nil todos (proc-dated-entry path hit {
                :datefield "due"
                :prefix "due"
                :pp (fn [self] 
                    (print (string "due: " (self :due-on) " | " (self :desc) " | filename: " (self :filename)))
                )
            })) 
            "appt" (array/push-not-nil appt (proc-dated-entry path hit {
                :prefix "date-of"
                :datefield "date"
                :pp (fn [self] 
                    (print (string "date: " (self :date-of-on) " | " (self :desc) " | filename: " (self :filename)))
                )
            }))
        )
    )

    (walk-rec path (entry-fn proc-hit))
    
    (print-hits appt 
        {:title "# Appointments"
        :if-empty ""})
    (print-hits todos 
        {:title "# Todos this week"
        :if-empty ""})
)

(defn- jd-areas [dir] 
    (def area-name-pat (peg/compile ~{
        :jd-dig (between 1 2 (range "09"))
        :jd-prefix (* :jd-dig "-" :jd-dig)
        :main (any (+ (* ($) :jd-prefix) 1))
    }))
    
    (defn is-area-name? [p] 
        (def m (peg/match area-name-pat p))
        (match (peg/match area-name-pat p)
            @[0] true
            _ false))
    
    (defn print-if-area [p] 
        (def dir (array/slice (path/parts p) -2 -1))
        # ; (pp ~(,dir ,(is-area-name? (dir 0))))
        (if (is-area-name? (dir 0)) 
            (print (path/basename p))
        )
    )
    (walk-rec-dirs dir print-if-area)
)


(defn- usage [] 
    (comment 
        "Commands to add:
        decimals          - Prints out a list of the individual JD topics\n
        cap(ture)         - Capture a @todo to \"$JD_FOLDER/1 Captures\"\n
        "
    )
    (print "anno <subcommand> args\n
\n
Supported subcommands:\n
    areas             - Prints out a list of the JD areas in $JD_FOLDER\n
    debug-dump        - Prints out a list of all recognized annotations and their data\n
    agenda            - Prints out @todos and @appts (appointments)\n
    review            - Prints out @tils 
    help <subcommand> - Shows help for a given subcommand\n
    ")
)

(defn- detailed-help [args] 
    (def help @[
        "Anno(tations) is a program that scans a directory laid out according to the"
        "Johnny Decimal system."
        "It scans for annotations that look like so: @todo[Get Milk|start:2021-1-4|due:2021-1-5]"
        ""
        "Annot(tations). Copyright 2020 Andrew Owen" 
    ])
    (each l help (print l))
)

(defn main [& args] 
    
    (def args (array/concat @[] args))
    (setdyn :jd-folder (os/getenv "JD_FOLDER" "."))
    # ; Pop off the script name
    (array/shift args)

    # ; Do we have any arguments left?
    (match (length args) 
        0 (do
            (print "No subcommand given")
            (usage)
            (break)
          )
        _ ()
    )
    
    # ; Get the script subcommand
    (def subcommand (array/shift args))
    (match subcommand 
        "areas" (jd-areas (dyn :jd-folder))
        "agenda" (agenda (dyn :jd-folder))
        "projects" (scan-projects (dyn :jd-folder))
        "tasks" (scan-tasks (dyn :jd-folder))
        "review" (review (dyn :jd-folder))
        "debug-dump" (scan-rec (dyn :jd-folder))
        "help" (detailed-help args)
        _ (do 
            (print (string "Unrecognized subcommand " subcommand))
            (usage)
            )
    )
)
