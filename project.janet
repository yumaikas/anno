(declare-project 
    :name "anno"
    :description "Read out annotations in text files"
    :dependencies ["https://github.com/janet-lang/path.git"]
)

# ; (declare-native :name "strptime" :source @["strptime.cpp"])

(declare-executable
    :name "anno"
    :entry "anno-impl.janet"
)

