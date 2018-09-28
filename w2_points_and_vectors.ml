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
