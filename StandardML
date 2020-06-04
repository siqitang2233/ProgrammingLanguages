(*question 1*)
fun reverse (x::xs) = foldl (fn (x, acc) => x :: acc) [] (x::xs);

(*question 2*)
fun composelist x [] = x
| composelist x (y::ys) = composelist (y x) ys;

(*question 3*)
exception Badform;
fun foldmod x [] = raise Badform
| foldmod x (y::ys) =
    let fun deposit f a [] = a
    | deposit f a (b::bs) = deposit f (f b a) bs
    in
    deposit x y ys
    end;

(*question 4*)
exception Mismatch;
fun zip ([], []) = []
| zip ([], y::ys) = raise Mismatch
| zip (x::xs, []) = raise Mismatch
| zip (x::xs, y::ys) = (x,y)::zip(xs,ys);

(*question 5*)
fun unzip xs = foldl (fn ((x, y), (ax, ay)) => (ax@[x], ay@[y])) (nil, nil) xs;

(*question 6*)
fun add x y = x + y;

fun bind NONE b f = NONE
| bind a NONE f = NONE
| bind (SOME a) (SOME b) f = SOME (f a b);

(*question 7*)
fun getitem x [] = NONE
| getitem x (y::ys) = 
if x = y
then SOME y
else getitem x ys;

(*question 8*)
fun getitem2 x [] = NONE
| getitem2 NONE y = NONE
| getitem2 (SOME x) (y::ys) = 
if x = y
then SOME y
else getitem2 (SOME x) ys;

(*starting next question*)
signature MLPQUEUE =
sig
    type 'a mlqueue
    
exception Overflow
exception Empty
exception LevelNoExist
exception NotFound

val maxlevel: int
val new: 'a mlqueue
val enqueue: 'a mlqueue -> int -> int -> 'a -> 'a mlqueue
val dequeue: 'a mlqueue -> 'a * 'a mlqueue
val move: ('a -> bool) -> 'a mlqueue -> 'a mlqueue
val atlevel: 'a mlqueue -> int -> (int * 'a) list
val lookup: ('a -> bool) -> 'a mlqueue -> int * int
val isempty: 'a mlqueue -> bool
val empty: 'a mlqueue
val flatten: 'a mlqueue -> 'a list
end;

signature MLQPARAM = sig
    type element;
    val max: int;
end;


functor MakeQ(x:MLQPARAM):MLPQUEUE=
struct
type 'a mlqueue = (int * int * 'a) list
exception Overflow
exception Empty
exception LevelNoExist
exception NotFound

val maxlevel: int = x.max
val new: 'a mlqueue = []
val empty: 'a mlqueue = []
fun isempty (q:'a mlqueue) = null q
fun compare((lx, px), (ly, py)) =
    if lx < ly then true
    else
        if ly < lx then false
        else
            if px <= py then true
            else false
fun enqueue q l p e =
if l > maxlevel then raise LevelNoExist
else
if null q then q @ [(l, p, e)]
else
let fun ins (lx, px, _) = compare((lx, px), (l, p))
    val (lq, rq) = List.partition ins q
in
    lq@[(l, p, e)]@rq
end

fun dequeue [] = raise Empty
| dequeue ((_, _, x)::q) = (x, q)

fun move pred q =
    let
        val (mq, rem) = List.partition (fn (_, _, x) => pred(x)) q;
        val nmq = List.map (fn (l, p, e) => (l-1, p, e)) mq;
        val revq = List.foldl (fn ((l, p, e), q) => enqueue q l p e ) rem nmq;
    in
        revq
    end

fun equal (x, y) =
    if x > y then false else
        if y > x then false else true

fun atlevel q l =
        let
            fun filterf (li, _, _) = equal(li, l)
            val lq = List.filter filterf q
        in
            if null lq then raise LevelNoExist
            else List.map (fn (_, p, x) => (p, x)) lq
        end
        
fun lookup pred q =
    case (List.find (fn (_,_,x) => pred(x)) q) of
        SOME (li, pi, _) => (li, pi)
        | NONE => raise NotFound
            
fun flatten q = List.map (fn (_, _, x) => x) q
end;
   
  
(*Q1*)
structure MyQ:MLQPARAM=
struct
    type element = int
    val max = 2
end;

structure Q = MakeQ(MyQ);

(*Q2*)
open Q;
val q0 = [];
val q1 = enqueue q0 1 1 2;
val q2 = enqueue q1 0 0 3;
val q3 = enqueue q2 2 0 5;
val q4 = enqueue q3 2 2 1;
val q5 = enqueue q4 1 0 4;
val q6 = enqueue q5 2 1 6;

(*Q3*)
val exceed = enqueue q6 9 2 2;

(*Q4*)
val m = move (fn x => (x > 3)) q6;
val mf = flatten m;

(*Q5*)
val (x, d1) = dequeue q6;
val (y, d2) = dequeue d1;

(*Q6*)
val l1 = atlevel q6 1;

(*Q7*)
val (x, y) = lookup (fn x => x < 5) q1;
