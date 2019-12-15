(* --- Day 3: Crossed Wires ---
 *
 * The gravity assist was successful, and you're well on your way to the
 * Venus refuelling station. During the rush back on Earth, the fuel
 * management system wasn't completely installed, so that's next on the
 * priority list.
 *
 * Opening the front panel reveals a jumble of wires. Specifically, two
 * wires are connected to a central port and extend outward on a
 * grid. You trace the path each wire takes as it leaves the central
 * port, one wire per line of text (your puzzle input).
 *
 * The wires twist and turn, but the two wires occasionally cross
 * paths. To fix the circuit, you need to find the intersection point
 * closest to the central port. Because the wires are on a grid, use the
 * Manhattan distance for this measurement. While the wires do
 * technically cross right at the central port where they both start,
 * this point does not count, nor does a wire count as crossing with
 * itself.
 *
 * For example, if the first wire's path is R8,U5,L5,D3, then starting
 * from the central port (o), it goes right 8, up 5, left 5, and finally
 * down 3:
 *
 * ...........
 * ...........
 * ...........
 * ....+----+.
 * ....|....|.
 * ....|....|.
 * ....|....|.
 * .........|.
 * .o-------+.
 * ...........
 *
 * Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6,
 * down 4, and left 4:
 *
 * ...........
 * .+-----+...
 * .|.....|...
 * .|..+--X-+.
 * .|..|..|.|.
 * .|.-X--+.|.
 * .|..|....|.
 * .|.......|.
 * .o-------+.
 * ...........
 *
 * These wires cross at two locations (marked X), but the lower-left one
 * is closer to the central port: its distance is 3 + 3 = 6.
 *
 * Here are a few more examples:
 *
 *     R75,D30,R83,U83,L12,D49,R71,U7,L72
 *     U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
 *     R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
 *     U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
 *
 * What is the Manhattan distance from the central port to the closest
 * intersection? *)

open Base

type direction =
  | Left of int
  | Right of int
  | Up of int
  | Down of int
  | Error

let read_lines filename =
  let open Stdio in
  In_channel.create filename |> In_channel.input_lines
;;

let get_direction_number string =
  Int.of_string (String.sub string ~pos:1 ~len:(String.length string - 1))
;;

let direction_of_string string =
  match string.[0] with
  | 'L' -> Left (get_direction_number string)
  | 'R' -> Right (get_direction_number string)
  | 'U' -> Up (get_direction_number string)
  | 'D' -> Down (get_direction_number string)
  | _ -> Error
;;

let compute_point point direction =
  let x, y = point in
  match direction with
  | Left n -> x - n, y
  | Right n -> x + n, y
  | Up n -> x, y + n
  | Down n -> x, y - n
  | Error -> 0, 0
;;

let rec points_of_directions point directions =
  match directions with
  | [] -> []
  | d :: ds ->
    let next_point = compute_point point d in
    next_point :: points_of_directions next_point ds
;;

let segment_intersection s1 s2 =
  try
    let (x1, y1), (x2, y2) = s1 in
    let (x3, y3), (x4, y4) = s2 in
    let intersection_x =
      ((((x1 * y2) - (y1 * x2)) * (x3 - x4)) - ((x1 - x2) * ((x3 * y4) - (y3 * x4))))
      / (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))
    in
    let intersection_y =
      ((((x1 * y2) - (y1 * x2)) * (y3 - y4)) - ((y1 - y2) * ((x3 * y4) - (y3 * x4))))
      / (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))
    in
    if (* s1 horizontal *)
       ( y1 = y2
       && x1 <> x2
       && y3 <> y4
       && x3 = x4
       && intersection_x > min x1 x2
       && intersection_x < max x1 x2
       && intersection_y > min y3 y4
       && intersection_y < max y3 y4 )
       || (* s2 horizontal *)
          ( y1 <> y2
          && x1 = x2
          && y3 = y4
          && x3 <> x4
          && intersection_x > min x3 x4
          && intersection_x < max x3 x4
          && intersection_y > min y1 y2
          && intersection_y < max y1 y2 )
    then Some (intersection_x, intersection_y)
    else None
  with Division_by_zero -> None
;;

(* Converts a line (list of points) into a list of line segments (pairs of points) *)
let rec segments_of_line line =
  match line with
  | [] -> []
  | p1 :: ps -> (match ps with [] -> [] | p2 :: _ -> (p1, p2) :: segments_of_line ps)
;;

(* Returns the intersections between two lists of points as a list of points *)
let find_intersections line1 line2 =
  let s1 = segments_of_line line1 in
  let s2 = segments_of_line line2 in
  let rec loop_s1 s1s =
    match s1s with
    | [] -> []
    | s1 :: ss ->
      let rec loop_s2 s2s =
        match s2s with
        | [] -> []
        | s2 :: rest ->
          (match segment_intersection s1 s2 with
          | None -> loop_s2 rest
          | Some intersection ->
            (* let () =
             *   let (x1, y1), (x2, y2) = s1 in
             *   Stdio.printf "S1: (%d, %d),(%d, %d)\n" x1 y1 x2 y2
             * in
             * let () =
             *   let (x1, y1), (x2, y2) = s2 in
             *   Stdio.printf "S2: (%d, %d),(%d, %d)\n" x1 y1 x2 y2
             * in
             * let () =
             *   let x, y = intersection in
             *   Stdio.printf "Intersection: (%d, %d)\n" x y
             * in *)
            intersection :: loop_s2 rest)
      in
      let intersections = loop_s2 s2 in
      intersections @ loop_s1 ss
  in
  loop_s1 s1
;;

let manhattan_distance p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  abs (x1 - x2) + abs (y1 - y2)
;;

let shortest_intersection_distance intersections =
  let closest_point =
    List.map intersections ~f:(fun i -> manhattan_distance (0, 0) i)
    |> List.sort ~compare:(fun a b -> a - b)
    |> List.hd
  in
  match closest_point with None -> 0 | Some p -> p
;;

let make_wire line = String.split line ~on:',' |> List.map ~f:direction_of_string
let make_wires lines = List.map ~f:make_wire lines

let part1 () =
  let lines = read_lines "input.txt" in
  let wire1 = match List.hd lines with None -> "" | Some w -> w in
  let wire2 =
    match List.tl lines with
    | None -> ""
    | Some l -> (match List.hd l with None -> "" | Some w -> w)
  in
  let l1 = make_wire wire1 |> points_of_directions (0, 0) in
  let l2 = make_wire wire2 |> points_of_directions (0, 0) in
  find_intersections l1 l2 |> shortest_intersection_distance
;;

(* --- Part Two ---
 *
 * It turns out that this circuit is very timing-sensitive; you actually
 * need to minimize the signal delay.
 *
 * To do this, calculate the number of steps each wire takes to reach
 * each intersection; choose the intersection where the sum of both
 * wires' steps is lowest. If a wire visits a position on the grid
 * multiple times, use the steps value from the first time it visits that
 * position when calculating the total value of a specific intersection.
 *
 * The number of steps a wire takes is the total number of grid squares
 * the wire has entered to get to that location, including the
 * intersection being considered. Again consider the example from above:
 *
 * ...........
 * .+-----+...
 * .|.....|...
 * .|..+--X-+.
 * .|..|..|.|.
 * .|.-X--+.|.
 * .|..|....|.
 * .|.......|.
 * .o-------+.
 * ...........
 *
 * In the above example, the intersection closest to the central port is
 * reached after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20
 * steps by the second wire for a total of 20+20 = 40 steps.
 *
 * However, the top-right intersection is better: the first wire takes
 * only 8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total of
 * 15+15 = 30 steps.
 *
 * Here are the best steps for the extra examples from above:
 *
 *     R75,D30,R83,U83,L12,D49,R71,U7,L72
 *     U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
 *     R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
 *     U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
 *
 * What is the fewest combined steps the wires must take to reach an
 * intersection? *)

(* a step-point is a 3-tuple with x, y, and the number of steps *)
let compute_step_point point direction =
  let x, y, s = point in
  match direction with
  | Left n -> x - n, y, s + n
  | Right n -> x + n, y, s + n
  | Up n -> x, y + n, s + n
  | Down n -> x, y - n, s + n
  | Error -> 0, 0, 0
;;

let rec step_points_of_directions point directions =
  match directions with
  | [] -> []
  | d :: ds ->
    let next_point = compute_step_point point d in
    next_point :: step_points_of_directions next_point ds
;;

let step_segment_intersection s1 s2 =
  try
    let (x1, y1, steps1), (x2, y2, _) = s1 in
    let (x3, y3, steps3), (x4, y4, _) = s2 in
    let intersection_x =
      ((((x1 * y2) - (y1 * x2)) * (x3 - x4)) - ((x1 - x2) * ((x3 * y4) - (y3 * x4))))
      / (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))
    in
    let intersection_y =
      ((((x1 * y2) - (y1 * x2)) * (y3 - y4)) - ((y1 - y2) * ((x3 * y4) - (y3 * x4))))
      / (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))
    in
    if (* s1 horizontal, s2 vertical *)
       y1 = y2
       && x1 <> x2
       && y3 <> y4
       && x3 = x4
       && intersection_x > min x1 x2
       && intersection_x < max x1 x2
       && intersection_y > min y3 y4
       && intersection_y < max y3 y4
    then
      Some
        ( intersection_x
        , intersection_y
        , steps1 + abs (intersection_x - x1) + steps3 + abs (intersection_y - y3) )
    else if (* s1 vertical, s2 horizontal *)
            y1 <> y2
            && x1 = x2
            && y3 = y4
            && x3 <> x4
            && intersection_x > min x3 x4
            && intersection_x < max x3 x4
            && intersection_y > min y1 y2
            && intersection_y < max y1 y2
    then
      Some
        ( intersection_x
        , intersection_y
        , steps1 + abs (intersection_y - y1) + steps3 + abs (intersection_x - x3) )
    else None
  with Division_by_zero -> None
;;

(* Returns the intersections between two lists of points as a list of step-points *)
let find_step_intersections line1 line2 =
  let s1 = segments_of_line line1 in
  let s2 = segments_of_line line2 in
  let rec loop_s1 s1s =
    match s1s with
    | [] -> []
    | s1 :: ss ->
      let rec loop_s2 s2s =
        match s2s with
        | [] -> []
        | s2 :: rest ->
          (match step_segment_intersection s1 s2 with
          | None -> loop_s2 rest
          | Some intersection ->
            (* let () =
             *   let (x1, y1, st1), (x2, y2, st2) = s1 in
             *   Stdio.printf "S1: (%d, %d, %d),(%d, %d, %d)\n" x1 y1 st1 x2 y2 st2
             * in
             * let () =
             *   let (x1, y1, st1), (x2, y2, st2) = s2 in
             *   Stdio.printf "S2: (%d, %d, %d),(%d, %d, %d)\n" x1 y1 st1 x2 y2 st2
             * in
             * let () =
             *   let x, y, st = intersection in
             *   Stdio.printf "Intersection: (%d, %d, %d)\n" x y st
             * in *)
            intersection :: loop_s2 rest)
      in
      let intersections = loop_s2 s2 in
      intersections @ loop_s1 ss
  in
  loop_s1 s1
;;

let shortest_intersection_steps intersections =
  let closest_point =
    List.map intersections ~f:(fun (_, _, steps) -> steps)
    |> List.sort ~compare:(fun a b -> a - b)
    |> List.hd
  in
  match closest_point with None -> 0 | Some p -> p
;;

let part2 () =
  let lines = read_lines "input.txt" in
  let wire1 = match List.hd lines with None -> "" | Some w -> w in
  let wire2 =
    match List.tl lines with
    | None -> ""
    | Some l -> (match List.hd l with None -> "" | Some w -> w)
  in
  let l1 = make_wire wire1 |> step_points_of_directions (0, 0, 0) in
  let l2 = make_wire wire2 |> step_points_of_directions (0, 0, 0) in
  find_step_intersections l1 l2 |> shortest_intersection_steps
;;

let () =
  Stdio.printf "Part 1 answer: %d\n" @@ part1 ();
  Stdio.printf "Part 2 answer: %d\n" @@ part2 ()
;;
