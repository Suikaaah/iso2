type base_type =
  | Unit
  | Sum of base_type list
  | Product of base_type list
  | Named of string
[@@deriving show { with_path = false }]

type iso_type =
  | BiArrow of { a : base_type; b : base_type }
  | Arrow of { t_1 : iso_type; t_2 : iso_type }
[@@deriving show { with_path = false }]

type value =
  | Unit
  | Named of string
  | Cted of { c : string; v : value }
  | Tuple of value list
[@@deriving show { with_path = false }]

type pat = Named of string | Tuple of pat list
[@@deriving show { with_path = false }]

type expr =
  | Value of value
  | Let of { p_1 : pat; omega : iso; p_2 : pat; e : expr }
[@@deriving show { with_path = false }]

and iso =
  | Pairs of (value * expr) list
  | Fix of { phi : string; omega : iso }
  | Lambda of { psi : string; omega : iso }
  | Named of string
  | App of { omega_1 : iso; omega_2 : iso }
  | Invert of iso
[@@deriving show { with_path = false }]

type term =
  | Unit
  | Named of string
  | Cted of { c : string; t : term }
  | Tuple of term list
  | App of { omega : iso; t : term }
  | Let of { p : pat; t_1 : term; t_2 : term }
[@@deriving show { with_path = false }]

type variant = Value of string | Iso of { c : string; a : base_type }
[@@deriving show { with_path = false }]

type typedef = { t : string; vs : variant list }
[@@deriving show { with_path = false }]

type program = { ts : typedef list; t : term }
[@@deriving show { with_path = false }]
