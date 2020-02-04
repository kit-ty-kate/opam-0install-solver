let fop : Cudf_types.relop -> int -> int -> bool = function
  | `Eq -> (=)
  | `Neq -> (<>)
  | `Geq -> (>=)
  | `Gt -> (>)
  | `Leq -> (<=)
  | `Lt -> (<)

module Make (Context : S.CONTEXT) = struct
  module Role = struct
    type t = {
      context : Context.t;
      name : Cudf_types.pkgname;
    }
    let pp fmt t = Fmt.string fmt t.name
    let compare t t' = String.compare t.name t'.name
  end

  type impl = Cudf.package

  type command = |
  type command_name = private string
  let pp_command _ = function (_:command) -> .
  let command_requires _role = function (_:command) -> .
  let get_command _impl _command_name = None

  type restriction = {
    kind : [ `Ensure | `Prevent ];
    expr : (Cudf_types.relop * Cudf_types.version) list;
    (* NOTE: each list is a raw or the list is an OR case (see Cudf_types.vpkgforula) *)
  }

  type dependency = {
    drole : Role.t;
    importance : [ `Essential | `Recommended | `Restricts ];
    restrictions : restriction list;
  }

  type dep_info = {
    dep_role : Role.t;
    dep_importance : [ `Essential | `Recommended | `Restricts ];
    dep_required_commands : command_name list;
  }

  type requirements = {
    role : Role.t;
    command : command_name option;
  }

  type role_information = {
    replacement : Role.t option;
    impls : impl list;
  }

  type machine_group = private string
  let machine_group _impl = None

  type conflict_class = private string
  let conflict_class _impl = []

  type rejection = Context.rejection

  let rec ensure = function (* TODO: handle OR :( *)
    | [] -> []
    | []::_ -> assert false (* false *)
    | (x::_)::xs -> x::ensure xs

  let prevent l = l

  let requires role impl =
    let dependencies =
      let make_dep importance kind (name, constr) =
        let drole = {role with Role.name} in
        let expr = match constr with
          | None -> []
          | Some c -> [c]
        in
        let restrictions = [{kind; expr}] in
        {drole; importance; restrictions}
      in
      List.map (make_dep `Essential `Ensure) (ensure impl.Cudf.depends) @
      List.map (make_dep `Restricts `Prevent) (prevent impl.Cudf.conflicts)
    in
    (dependencies, [])

  let dep_info dep = {
    dep_role = dep.drole;
    dep_importance = dep.importance;
    dep_required_commands = [];
  }

  let pp_impl fmt impl =
    Fmt.string fmt (impl.Cudf.package^"."^string_of_int impl.Cudf.version)

  let implementations role =
    let impls =
      Context.candidates role.Role.context role.Role.name
      |> List.filter_map (function
        | _, Some _rejection -> None
        | version, None ->
            Some (Context.load role.Role.context (role.Role.name, version))
      )
    in
    {replacement = None; impls}

  let restrictions dep =
    dep.restrictions

  let meets_restriction impl {kind; expr} =
    let aux (c, v) = fop c impl.Cudf.version v in
    let res = List.exists aux expr in
    match kind with
    | `Ensure -> res
    | `Prevent -> not res

  let rejects role =
    let rejects =
      Context.candidates role.Role.context role.Role.name
      |> List.filter_map (function
        | _, None -> None
        | version, Some reason ->
            let impl = Context.load role.Role.context (role.Role.name, version) in
            Some (impl, reason)
      )
    in
    (rejects, [])

  let compare_version impl1 impl2 =
    compare (impl1.Cudf.version : int) impl2.Cudf.version

  let format_version impl =
    string_of_int impl.Cudf.version

  let user_restrictions role =
    match Context.user_restrictions role.Role.context role.Role.name with
    | [] -> None
    | expr -> Some { kind = `Ensure; expr }

  let id_of_impl impl =
    impl.Cudf.package^"."^string_of_int impl.Cudf.version

  let format_machine _impl = "(src)"

  let string_of_op = function
    | `Eq -> "="
    | `Geq -> ">="
    | `Gt -> ">"
    | `Leq -> "<="
    | `Lt -> "<"
    | `Neq -> "<>"

  let string_of_version_formula l =
    String.concat " & " (
      List.map (fun (rel, v) ->
        Printf.sprintf "%s %s" (string_of_op rel) (string_of_int v)
      ) l
    )

  let string_of_restriction = function
    | { kind = `Prevent; expr = [] } -> "conflict with all versions"
    | { kind = `Prevent; expr } -> Fmt.strf "not(%s)" (string_of_version_formula expr)
    | { kind = `Ensure; expr } -> string_of_version_formula expr

  let describe_problem _impl = Fmt.to_to_string Context.pp_rejection

  let dummy_impl = {
    Cudf.package = "*";
    version = 1;
    depends = [];
    conflicts = [];
    provides = [];
    installed = false;
    was_installed = false;
    keep = `Keep_none;
    pkg_extra = [];
  }

  let role context name =
    {Role.context; name}

  let version impl =
    Some (impl.Cudf.package, impl.Cudf.version)

  let virtual_role ~context impl =
    {Role.context; name = impl.Cudf.package}

  let virtual_impl ~depends () =
    let depends = List.map (fun pkg -> [(pkg, None)]) depends in
    {dummy_impl with depends}
end
