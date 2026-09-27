let repository_url = "https://github.com/Fortiphyd/iec-checker"

let docs_url = repository_url ^ "/blob/master/docs/"

(* GitHub makes a heading's anchor from its text, in lower case. *)
let check_doc_url id = docs_url ^ "detectors.md#" ^ String.lowercase_ascii id
