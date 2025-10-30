#import "@preview/curryst:0.5.1": prooftree, rule

#set text(size: 12pt, font: "New Computer Modern")

#show math.phi: math.phi.alt

#let unit = $mono("unit")$
#let bool = $mono("bool")$
#let i64 = $mono("i64")$
#let type = $mono("type")$
#let of = $mono("of")$
#let fix = $mono("fix")$
#let mylet = $mono("let")$
#let myin = $mono("in")$
#let case = $mono("case")$
#let fun = $mono("fun")$
#let invert = $mono("invert")$
#let iso = $mono("iso")$
#let tsum = $plus.circle$
#let tprod = $times.circle$
#let lalign(body) = align(box(body), left)
#let btree(body) = box(prooftree(body))
#let tack_sub(sub) = $thick #box($tack$) _sub thick$
#let rules(body) = align(center)[
  #set par(leading: 1em)
  #show math.equation: set par(leading: 0.25em)
  #body
]

*Grammar - Types*

#lalign[
  $
    & "(Base types)" && wide & A, B ::= & unit
                                          | A_1 tsum ... tsum A_n
                                          | A_1 tprod ... tprod A_n | \
    &                &&      &          & mu X . A
                                          | (A_1, ..., A_n) thick X
                                          | X \
    & "(Isos)"       &&      &    T ::= & A <-> B
                                          | T_1 -> T_2
                                          | X \
  $
]

\

*Grammar - Terms*

#lalign[
  $
    & "(Values)"      && wide &     v ::= & ()
                                            | x
                                            | c
                                            | c thick v
                                            | (v_1, ..., v_n) \
    & "(Expressions)" &&      &     e ::= & v
                                            | mylet v_1 = v_2 myin e
                                            | mylet v_1 = omega thick v_2 myin e \
    & "(Isos)"        &&      & omega ::= & (case v_1 <-> e_1 | ... | v_n <-> e_n) \
    &                 &&      &           & fix phi . omega
                                            | fun phi -> omega
                                            | phi
                                            | c
                                            | invert omega
                                            | omega_1 thick omega_2 \
    & "(Terms)"       &&      &     t ::= & ()
                                            | x
                                            | (t_1, ..., t_n)
                                            | omega thick t
                                            | mylet v = t_1 myin t_2
                                            | iso x = omega myin t
  $
]

\

*Typing Rules - Terms*

#rules[
  #btree(rule($Psi; emptyset tack (): unit$))
  #h(1em)
  #btree(rule($Psi; x: A tack x: A$))
  #h(1em)
  #btree(rule(
    $Psi; Delta tack (t_1, ..., t_n): A_1 tprod ... tprod A_n$,
    $Psi; Delta_1 tack t_1: A_1$,
    $...$,
    $Psi; Delta_n tack t_n: A_n$,
  ))
  #btree(rule(
    $Psi; Delta tack omega thick t: B$,
    $Psi #tack_sub($omega$) omega: A <-> B$,
    $Psi; Delta tack t: A$,
  ))
  #btree(rule(
    $Psi; Delta_1, Delta_2 tack mylet (x_1, .., x_n) = t_1 myin t_2: B$,
    $Psi; Delta_1 tack t_1: A_1 tprod ... tprod A_n$,
    $Psi; Delta_2 tack x_1: A_1, ..., x_n: A_n tack t_2: B$,
  ))
]

\

*Typing Rules - Isos*

#rules[
  #btree(rule($Psi; phi: T #tack_sub($omega$) phi: T$))
  #h(1em)
  #btree(rule(
    $Psi #tack_sub($omega$) omega_2 thick omega_1: T_2$,
    $Psi #tack_sub($omega$) omega_1: T_1$,
    $Psi #tack_sub($omega$) omega_2: T_1 -> T_2$,
  ))
  #h(1em)
  #btree(rule(
    $Psi #tack_sub($omega$) lambda phi . omega: T_1 -> T_2$,
    $Psi, phi: T_1 #tack_sub($omega$) omega: T_2$,
  ))
  #btree(rule(
    $Psi #tack_sub($omega$) { v_1 <-> e_1 | ... | v_n <-> e_n }: A <-> B$,
    $Psi; Delta_1 tack v_1: A quad ... quad Psi; Delta_n tack v_n: A quad forall i eq.not j, v_i perp v_j$,
    $Psi; Delta_1 tack e_1: B quad ... quad Psi; Delta_n tack e_n: B quad forall i eq.not j, e_i perp e_j$,
  ))
]
