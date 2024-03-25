(* from an opam file and an opam.locked file (and the opam-repository / state),
   we'd like to collect:
   - direct (non-build) dependencies of this package
   - transitively map dependencies of the deps

   the opam file serves us well for an initial set of direct deps
   the locked file serves us with picked version numbers
   the transitive map we need the repository state and use the picked versions
*)

let deps opam =
  let depends = OpamFile.OPAM.depends opam in
  let env var = match OpamVariable.Full.to_string var with
    | "monorepo" | "vendor" -> Some (OpamTypes.B true)
    | "build" | "with-test" | "dev" | "with-doc" | "post" -> Some (B false)
    | _ -> None
  in
  OpamFilter.filter_formula ~default:false env depends

let map_of_formula f =
  let v = function
    | OpamFormula.Atom (`Eq, v) -> v
    | _ -> invalid_arg "only supporting an atom with ="
  in
  let rec go acc = function
    | OpamFormula.Empty -> acc
    | Atom (name, version) ->
      OpamPackage.Name.Map.add name (OpamPackage.create name (v version)) acc
    | Block b -> go acc b
    | And (a, b) -> go (go acc a) b
    | Or _ -> invalid_arg "map_of_formula: or is not supported"
  in
  go OpamPackage.Name.Map.empty f

let set_of_formula map f =
  let rec find_in_map = function
    | OpamFormula.Empty -> None
    | Atom (name, _) ->
      if OpamPackage.Name.Map.mem name map then Some name else None
    | Block b -> find_in_map b
    | And _ -> None
    | Or (a, b) ->
      match find_in_map a with None -> find_in_map b | Some _ as x -> x
  in
  let rec go acc = function
    | OpamFormula.Empty -> acc
    | Atom (name, _) -> OpamPackage.Name.Set.add name acc
    | Block b -> go acc b
    | And (a, b) -> go (go acc a) b
    | Or _ as o ->
      (match find_in_map o with
       | None -> invalid_arg ("set_of_formula: couldn't figure out or " ^ OpamFormula.to_string o)
       | Some name -> OpamPackage.Name.Set.add name acc)
  in
  go OpamPackage.Name.Set.empty f

module M = Map.Make(String)

let build_graph st package opam_lock opam =
  let map = map_of_formula (deps opam_lock) in
  let q = Queue.create () in
  let rec go acc visited =
    match Queue.take_opt q with
    | None -> acc
    | Some name ->
      if OpamPackage.Name.Set.mem name visited then
        go acc visited
      else begin
        let acc, visited =
          try
            let pkg = OpamPackage.Name.Map.find name map in
            let opam =
              try
                OpamSwitchState.opam st pkg
              with Not_found ->
                let pkg = OpamSwitchState.get_package st name in
                OpamSwitchState.opam st pkg
            in
            let direct_deps = set_of_formula map (deps opam) in
            let acc =
              M.add (OpamPackage.Name.to_string name)
                (List.map OpamPackage.Name.to_string (OpamPackage.Name.Set.elements direct_deps)) acc
            in
            let visited = OpamPackage.Name.Set.add name visited in
            OpamPackage.Name.Set.iter (fun name -> Queue.push name q) direct_deps;
            acc, visited
          with Not_found ->
            invalid_arg ("build_graph: couldn't find package " ^ OpamPackage.Name.to_string name)
        in
        go acc visited
      end
  in
  let direct_deps =
    let direct_deps = set_of_formula map (deps opam) in
    if OpamPackage.Name.(Set.mem (of_string "mirage-solo5") direct_deps) &&
       OpamPackage.Name.(Map.mem (of_string "ocaml-solo5") map) then
      OpamPackage.Name.(Set.add (of_string "ocaml-solo5") direct_deps)
    else
      direct_deps
  in
  OpamPackage.Name.Set.iter (fun name -> Queue.push name q) direct_deps;
  let root_name = OpamPackage.name_to_string package in
  go
    (M.singleton root_name (List.map OpamPackage.Name.to_string (OpamPackage.Name.Set.elements direct_deps)))
    OpamPackage.Name.Set.empty

let deps_opam pos deps =
  let elements =
    let decorate pelem = OpamParserTypes.FullPos.{ pelem ; pos } in
    M.fold (fun k vs acc ->
        let vs =
          let els =
            List.map (fun v -> decorate (OpamParserTypes.FullPos.String v)) vs
          in
          decorate (OpamParserTypes.FullPos.List {pelem = els; pos})
        and k = decorate (OpamParserTypes.FullPos.String k)
        in
        let v =
          decorate (OpamParserTypes.FullPos.List { pelem = [ k ; vs ] ; pos })
        in
        v :: acc)
      deps []
  in
  OpamParserTypes.FullPos.{ pelem = List { pelem = elements ; pos } ; pos }
