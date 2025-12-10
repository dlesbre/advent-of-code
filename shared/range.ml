type t = {
  start: int; (** included *)
  stop: int;  (** excluded *)
  step: int
}

let upto stop = { start=0; stop; step=1 }
let interval start stop = { start; stop; step=1 }
let inclusive_interval start stop = interval start (stop+1)

let end_iteration r n = if r.step > 0 then n >= r.stop else n <= r.stop

let rec iter f range pos =
  if end_iteration range pos then ()
  else (f pos; iter f range (pos+range.step))
let iter f range = iter f range range.start

let rec forall f range pos =
  if end_iteration range pos then true
  else (f pos && forall f range (pos+range.step))
let forall f range = forall f range range.start

let rec exists f range pos =
  if end_iteration range pos then false
  else (f pos || exists f range (pos+range.step))
let exists f range = exists f range range.start

let rec fold f range pos acc =
  if end_iteration range pos then acc
  else (fold f range (pos+range.step) (f pos acc))
let fold f range acc = fold f range range.start acc

let sum f range = fold (fun i acc -> f i + acc) range 0
let count f range = fold (fun i acc -> if f i then acc+1 else acc) range 0

let pp fmt = function
  | { start; stop; step=1; } -> Format.fprintf fmt "[%d:%d[" start stop
  | { start; stop; step } -> Format.fprintf fmt "[%d:%d:%d[" start stop step
