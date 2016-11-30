open Format
open Ast

module Type = struct
  type t =
    | Mismatch of ty * ty
    | Undeclared_id of id
    | Undeclared_method of ty * id
    | Undeclared_class of ty
    | Wrong_number_of_arguments of ty * id * int * int
    | Non_class_instantiation of ty
    | Invalid_instantiation of ty
    | Invalid_shadowing of id
    | Match_subsumption of ty * ty
    | Match_non_class of ty
    | Match_mismatch of ty * ty
    | Unexpected_var
    | Operator_mismatch of op
    | Join_failure of ty * ty
    | Feature_redefinition of ty * feature * feature
    | Feature_override of ty * feature
    | Attribute_nothing of feature
    | Override_parameters_mismatch of feature
    | Override_return_mismatch of feature
    | Class_redefinition of classe * classe
    | Hierarchy_cycle of classe
    | Disallowed_inheritance of classe * ty
    | Native_superclass of classe * ty

  exception E of t

  let print ppf =
    let pr ppf = fprintf ppf "Type error: "; fprintf ppf in
  (*
    let ty_origin ty =
      let loc = loc_of_ty ty in
      if loc = Loc.ghost then "" else " (from " ^ (Loc.to_string loc) ^ ")" in
  *)
    let s_ty ty = "\"" ^ (string_of_ty ty) ^ "\"" in
    let s_id id = "\"" ^ (string_of_id id) ^ "\"" in
    function
    | Mismatch (ty_given, ty_expected) ->
        pr ppf "This expression has type %s but an expression was expected \
          of type %s" (s_ty ty_given) (s_ty ty_expected)
    | Undeclared_id id ->
        pr ppf "Undeclared identifier %s" (s_id id)
    | Undeclared_method (ty, f) ->
        pr ppf "The class %s has no method %s" (s_ty ty) (s_id f)
    | Undeclared_class ty ->
        pr ppf "Undeclared class %s" (s_ty ty)
    | Wrong_number_of_arguments (ty, f, n_given, n_expected) ->
        (if n_given < n_expected
          then pr ppf "Too few arguments to method %s of class %s"
          else pr ppf "Too many arguments to method %s of class %s")
          (s_id f) (s_ty ty);
        pr ppf "(%d given, %d expected)" n_given n_expected
    | Non_class_instantiation ty ->
        pr ppf "The type %s is not a class and can not be instantiated through \"new\""
          (s_ty ty)
    | Invalid_instantiation ty ->
        pr ppf "The class %s can not be instantiated through \"new\""
          (s_ty ty)
    | Invalid_shadowing id ->
        pr ppf "A local variable named %s cannot be declared because that \
          identifier is already bound in this scope" (s_id id)
    | Match_subsumption (ty, ty_shadowed) ->
        pr ppf "This branch is inaccessible because the type %s is superseded \
          by the type %s, which is used in a previous branch"
          (s_ty ty_shadowed) (s_ty ty)
    | Match_non_class ty ->
        pr ppf "The type %s is not a class type" (s_ty ty)
    | Match_mismatch(ty_given, ty0) ->
        pr ppf "This branch is inaccessible because the type %s does not \
          relate to the type of the expression being cased, which is %s"
          (s_ty ty_given) (s_ty ty0)
    | Unexpected_var ->
        pr ppf "Local variable declarations outside of a block expression"
    | Operator_mismatch op ->
        pr ppf "The operator %s is applied to the wrong number of operands"
          (string_of_op op)
    | Join_failure (ty1, ty2) ->
        pr ppf "Join operation between type %s and %s failed"
          (s_ty ty1) (s_ty ty2)
    | Feature_redefinition (ty, feat, _) ->
        (if is_attribute feat
          then pr ppf "The attribute %s is already defined in class %s"
          else pr ppf "The method %s is already defined in class %s")
          (s_id (feature_id feat)) (s_ty ty)
    | Feature_override (ty, feat) ->
        (if is_attribute feat
          then pr ppf "The attribute %s is already defined in an ancestor of \
            class %s"
          else pr ppf "The method %s is already defined in an ancestor of \
            class %s (use the keyword \"override\" to redefine a method in a \
            child class)")
          (s_ty ty) (s_id (feature_id feat))
    | Attribute_nothing feat ->
        pr ppf "Attribute %s has invalid type \"Nothing\""
          (s_id (feature_id feat))
    | Override_parameters_mismatch feat ->
        pr ppf "Wrong parameters when overriding method %s. Overriding method \
          must have the same number of parameters than the overridden method \
          and their type must match"
          (s_id (feature_id feat))
    | Override_return_mismatch feat ->
        pr ppf "Wrong return type when overriding method %s. Return type of \
          the overriding method must conform to the return type of the \
          overridden method"
          (s_id (feature_id feat))
    | Class_redefinition (classe, class0) ->
        pr ppf "Redeclaration of class %s (first declaration at %s)"
          (s_ty (class_type classe)) (Loc.to_string (loc_of_class class0))
    | Hierarchy_cycle classe ->
        pr ppf "Cyclic inheritance involving class %s"
          (s_ty (class_type classe))
    | Disallowed_inheritance (classe, ty_super) ->
        pr ppf "Class %s inherits from class %s, which is one of the basic \
          classes from Cool that cannot be inherited from\n\
          (those classes are: %s)"
          (s_ty (class_type classe)) (s_ty ty_super)
          (String.concat ", " (List.sort compare (List.map s_ty final_classes)))
    | Native_superclass (classe, ty_super) ->
        pr ppf "Class %s inherits from class %s, whose constructor is a native \
          method"
          (s_ty (class_type classe)) (s_ty ty_super)

  let to_string x = asprintf "%a" print x
end

module Eval = struct
  type backtrace = Loc.t list
  let empty_backtrace : backtrace = []
  let push_call (bt : backtrace) loc = loc :: bt

  type t' =
    | No_rule_applies
    | Null_dispatch
    | No_match
    | Division_by_zero
    | Out_of_range
    | Heap_overflow
    | Abort
  type t = t' * backtrace

  exception E of t

  let print ppf (err, bt) =
    let pr ppf = fprintf ppf "Runtime error: "; fprintf ppf in
    let pr_bt1 loc = fprintf ppf "Called from %s:\n"
      (String.uncapitalize (Loc.to_string loc)) in
    List.iter (fun loc -> if loc <> Loc.ghost then pr_bt1 loc) bt;
    match err with
    | No_rule_applies  -> pr ppf "No rule applies"
    | Null_dispatch    -> pr ppf "Null dispatch"
    | No_match         -> pr ppf "No matching branch in case statement"
    | Division_by_zero -> pr ppf "Division by zero"
    | Out_of_range     -> pr ppf "Index/size out of range"
    | Heap_overflow    -> pr ppf "Heap overflow"
    | Abort            -> pr ppf "Program terminated through call to abort()"

  let to_string x = asprintf "%a" print x
end

let () =
  let module M = Camlp4.ErrorHandler.Register(Type) in
  let module M = Camlp4.ErrorHandler.Register(Eval) in
  ()
