(*
The given prelude defines three types, one for three dimensional points, another for velocity vectors in three dimensions, and another one representing moving objects in space.

    Write a function move : point -> dpoint -> point such that move p dp is the point p whose coordinates have been updated according to dp.
    (x is now x +. dx, y is now y +. dy, z is now z +. dz.
    Write a function next : physical_object -> physical_object such that next o is the physical object o at time t + dt.
    The position of next o is the position of o moved according to its velocity vector.
    Suppose that these objects are spheres whose radius is 1.0.
    Write a function will_collide_soon : physical_object -> physical_object -> bool that tells if at the next instant, the two spheres will intersect.

The given prelude
*)
type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

let move p dp =
  { x = p.x +. dp.dx ; y = p.y +. dp.dy ; z = p.z +. dp.dz } ;;

let next obj =
  { position = (move obj.position obj.velocity) ; velocity = obj.velocity } ;;

let will_collide_soon p1 p2 =
  let distx = (next p1).position.x -. (next p2).position.x 
  and disty = (next p1).position.y -. (next p2).position.y 
  and distz = (next p1).position.z -. (next p2).position.z 
  in
  sqrt (distx *. distx +. disty *. disty +. distz *. distz) < 2.
