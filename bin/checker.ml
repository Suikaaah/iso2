open Types

let rec is_well_typed (a : base_type) (t : term) : bool =
  match (a, t) with
  | (Unit, Unit) -> true
  | (Named x, _)

