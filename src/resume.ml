open Sexplib
open Sexplib.Std

type item = {
  name : string;
  role : string option; [@sexp.option]
  location : string option; [@sexp.option]
  description : string option; [@sexp.option]
  date : string option; [@sexp.option]
  link : string option; [@sexp.option]
  notes : string list option; [@sexp.option]
}
[@@deriving sexp]

type doc = {
  name : string;
  email : string;
  location : string option;
  summary : string option;
  languages : string list option;
  links : string list option;
  experience : item list option;
  education : item list option;
  projects : item list option;
  publications : string list option;
  skills : string list option;
  interests : string list option;
  learning : string list option;
}

let resume_of_sexp (sexps : Parsexp.Many.parsed_value) =
  sexps
  |> List.fold_left
       (fun doc sexp ->
         match sexp with
         | Sexp.List [ Sexp.Atom "name"; Sexp.Atom value ] -> { doc with name = value }
         | Sexp.List [ Sexp.Atom "email"; Sexp.Atom value ] -> { doc with email = value }
         | Sexp.List [ Sexp.Atom "summary"; Sexp.Atom value ] ->
           { doc with summary = Some value }
         | Sexp.List [ Sexp.Atom "location"; Sexp.Atom value ] ->
           { doc with location = Some value }
         | Sexp.List [ Sexp.Atom "link"; Sexp.Atom link ] ->
           { doc with links = Some (link :: Option.value doc.links ~default:[]) }
         | Sexp.List (Sexp.Atom "experience" :: value) ->
           let item = item_of_sexp (Sexp.List value) in
           {
             doc with
             experience = Some (item :: Option.value doc.experience ~default:[]);
           }
         | Sexp.List (Sexp.Atom "education" :: value) ->
           let item = item_of_sexp (Sexp.List value) in
           { doc with education = Some (item :: Option.value doc.education ~default:[]) }
         | Sexp.List (Sexp.Atom "project" :: value) ->
           let item = item_of_sexp (Sexp.List value) in
           { doc with projects = Some (item :: Option.value doc.projects ~default:[]) }
         | Sexp.List (Sexp.Atom "skills" :: skills) ->
           { doc with skills = Some (List.map string_of_sexp skills) }
         | Sexp.List (Sexp.Atom "interests" :: interests) ->
           { doc with interests = Some (List.map string_of_sexp interests) }
         | Sexp.List (Sexp.Atom "learning" :: learning) ->
           { doc with learning = Some (List.map string_of_sexp learning) }
         | Sexp.List (Sexp.Atom "languages" :: languages) ->
           { doc with languages = Some (List.map string_of_sexp languages) }
         | _ -> doc)
       {
         name = "";
         email = "";
         summary = None;
         location = None;
         links = None;
         languages = None;
         experience = None;
         education = None;
         projects = None;
         publications = None;
         skills = None;
         interests = None;
         learning = None;
       }
  |> fun doc ->
  {
    doc with
    links = Option.map List.rev doc.links;
    experience = Option.map List.rev doc.experience;
    education = Option.map List.rev doc.education;
    projects = Option.map List.rev doc.projects;
    publications = Option.map List.rev doc.publications;
  }
