module Make (Context : S.CONTEXT) = struct
  (* Note: [OpamFormula.neg] doesn't work in the [Empty] case, so we just
     record whether to negate the result here. *)
  type restriction = {
    kind : [ `Ensure | `Prevent ];
    expr : (Cudf_types.relop * Cudf_types.version) list;
    (* NOTE: each list is a raw or the list is an OR case (see Cudf_types.vpkgforula) *)
  }

  type real_role = {
    context : Context.t;
    name : Cudf_types.pkgname;
  }

  type role =
    | Real of real_role               (* A role is usually an opam package name *)
    | Virtual of int * impl list      (* (int just for sorting) *)
  and real_impl = {
    context : Context.t;
    pkg : Cudf.package;
    requires : dependency list;
  }
  and dependency = {
    drole : role;
    importance : [ `Essential | `Recommended | `Restricts ];
    restrictions : restriction list;
  }
  and impl =
    | RealImpl of real_impl                     (* An implementation is usually an opam package *)
    | VirtualImpl of int * dependency list      (* (int just for sorting) *)
    | Dummy                                     (* Used for diagnostics *)

  let rec format_version = function
    | RealImpl impl -> string_of_int impl.pkg.Cudf.version
    | VirtualImpl (_i, deps) -> String.concat "&" (List.map (fun d -> Fmt.to_to_string pp_role d.drole) deps)
    | Dummy -> "(no version)"
  and pp_impl f = function
    | RealImpl impl -> Fmt.string f (impl.pkg.Cudf.package^"."^string_of_int impl.pkg.Cudf.version)
    | VirtualImpl _ as x -> Fmt.string f (format_version x)
    | Dummy -> Fmt.string f "(no solution found)"
  and pp_role f = function
    | Real t -> Fmt.string f t.name
    | Virtual (_, impls) -> Fmt.pf f "%a" Fmt.(list ~sep:(unit "|") pp_impl) impls

  module Role = struct
    type t = role

    let pp = pp_role

    let compare a b =
      match a, b with
      | Real a , Real b -> String.compare a.name b.name
      | Virtual (ia, _), Virtual (ib, _) -> compare ia ib
      | _, _ -> compare a b
  end

  let role context name = Real { context; name }

  let fresh_id =
    let i = ref 0 in
    fun () ->
      incr i;
      !i

  let virtual_impl ~context ~depends () =
    let depends = depends |> List.map (fun name ->
        let drole = role context name in
        { drole; importance = `Essential; restrictions = []}
      ) in
    VirtualImpl (fresh_id (), depends)

  let virtual_role impls =
    Virtual (fresh_id (), impls)

  type command = |          (* We don't use 0install commands anywhere *)
  type command_name = private string
  let pp_command _ = function (_:command) -> .
  let command_requires _role = function (_:command) -> .
  let get_command _impl _command_name = None

  type dep_info = {
    dep_role : Role.t;
    dep_importance : [ `Essential | `Recommended | `Restricts ];
    dep_required_commands : command_name list;
  }

  type requirements = {
    role : Role.t;
    command : command_name option;
  }

  let dummy_impl = Dummy

  (* Turn an opam dependency formula into a 0install list of dependencies. *)
  let list_deps ~context ~importance deps =
    let open OpamTypes in
    let rec aux = function
      | [] -> []
      | [[(name, restrictions)]] ->
        let drole = role context name in
        [{ drole; restrictions; importance }]
      | [x]::y -> aux [x] @ aux y
      | [_::_::_] as o ->
        let impls = group_ors o in
        let drole = virtual_role impls in
        (* Essential because we must apply a restriction, even if its
           components are only restrictions. *)
        [{ drole; restrictions = []; importance = `Essential }]
    and group_ors = function
      | [x::y] -> group_ors [x] @ group_ors y
      | expr -> [VirtualImpl (fresh_id (), aux expr)]
    in
    aux deps

  let requires _ = function
    | Dummy -> [], []
    | VirtualImpl (_, deps) -> deps, []
    | RealImpl impl -> impl.requires, []

  let dep_info { drole; importance; restrictions = _ } =
    { dep_role = drole; dep_importance = importance; dep_required_commands = [] }

  type role_information = {
    replacement : Role.t option;
    impls : impl list;
  }

  type machine_group = private string   (* We don't use machine groups because opam is source-only. *)
  let machine_group _impl = None

  type conflict_class = string
  let conflict_class _ = [] (* NOTE: I don't think you can define conflict classes in CUDF *)

  (* Opam uses conflicts, e.g.
       conflicts if X {> 1} OR Y {< 1 OR > 2}
     whereas 0install uses restricts, e.g.
       restrict to X {<= 1} AND Y {>= 1 AND <= 2}

     Warning: [OpamFormula.neg _ Empty = Empty], so does NOT reverse the result in this case.
     For empty conflicts this is fine (don't conflict with anything, just like an empty depends
     list). But for the version expressions inside, it's wrong: a conflict with no expression
     conflicts with all versions and should restrict the choice to nothing, not to everything.
     So, we just tag the formula as [`Prevent] instead of negating it. *)
  let prevent l =
    List.map (fun pkg -> { kind = `Prevent; expr = [pkg] }) l

  let ensure l =
    List.map (fun pkg -> { kind = `Ensure; expr = pkg }) l (* TODO: Might contain depopts? *)

  (* Get all the candidates for a role. *)
  let implementations role =
    match role with
    | Virtual (_, impls) -> { impls; replacement = None }
    | Real role ->
      let context = role.context in
      let impls =
        Context.candidates context role.name
        |> List.filter_map (function
            | _, Some _rejection -> None
            | version, None ->
              let pkg = (role.name, version) in
              let p = Context.load role.context pkg in
              (* Note: we ignore depopts here: see opam/doc/design/depopts-and-features *)
              let requires =
                let make_deps importance xform data =
                  data
                  |> xform
                  |> list_deps ~context ~importance
                in
                make_deps `Essential ensure p.Cudf.depends @
                make_deps `Restricts prevent p.Cudf.conflicts
              in
              Some (RealImpl { context; pkg = p; requires })
          )
      in
      { impls; replacement = None }

  let restrictions dependency = dependency.restrictions

  let meets_restriction impl { kind; expr } =
    match impl with
    | Dummy -> true
    | VirtualImpl _ -> assert false        (* Can't constrain version of a virtual impl! *)
    | RealImpl impl ->
      let result = OpamFormula.check_version_formula expr (OpamPackage.version impl.pkg) in
      match kind with
      | `Ensure -> result
      | `Prevent -> not result

  type rejection = Context.rejection

  let rejects role =
    match role with
    | Virtual _ -> [], []
    | Real role ->
      let context = role.context in
      let rejects =
        Context.candidates context role.name
        |> List.filter_map (function
            | _, None -> None
            | version, Some reason ->
              let pkg = OpamPackage.create role.name version in
              let opam = Context.load role.context pkg in
              Some (RealImpl { context; pkg; opam; requires = [] }, reason)
          )
      in
      let notes = [] in
      rejects, notes

  let compare_version a b =
    match a, b with
    | RealImpl a, RealImpl b -> OpamPackage.compare a.pkg b.pkg
    | VirtualImpl (ia, _), VirtualImpl (ib, _) -> compare ia ib
    | a, b -> compare a b

  let user_restrictions = function
    | Virtual _ -> None
    | Real role ->
      match Context.user_restrictions role.context role.name with
      | None -> None
      | Some f -> Some ({ kind = `Ensure; expr = OpamFormula.Atom f })

  let id_of_impl = function
    | RealImpl impl -> impl.pkg.Cudf.package^"."^string_of_int impl.pkg.Cudf.version
    | VirtualImpl _ -> "virtual"
    | Dummy -> "-"

  let format_machine _impl = "(src)"

  let string_of_op = function
    | `Eq -> "="
    | `Geq -> ">="
    | `Gt -> ">"
    | `Leq -> "<="
    | `Lt -> "<"
    | `Neq -> "<>"

  let string_of_version_formula = List.map (fun (rel, v) ->
      Printf.sprintf "%s %s" (string_of_op rel) (string_of_int v)
    )

  let string_of_restriction = function
    | { kind = `Prevent; expr = [] } -> "conflict with all versions"
    | { kind = `Prevent; expr } -> Fmt.strf "not(%s)" (string_of_version_formula expr)
    | { kind = `Ensure; expr } -> string_of_version_formula expr

  let describe_problem _ = Fmt.to_to_string Context.pp_rejection

  let version = function
    | RealImpl impl -> Some impl.pkg
    | VirtualImpl _ -> None
    | Dummy -> None
end
