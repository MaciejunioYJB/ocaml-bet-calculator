open Printf
open Str

let json_file_name = "data.json";;

(*HELPERS*)
let load_file_into_string filename =
	let channel = open_in filename in
	let output_string = really_input_string channel (in_channel_length channel) in
	close_in channel;
	output_string;;

let contains s1 s2 =
	let pattern = Str.regexp_string s2
	in
			try ignore (Str.search_forward pattern s1 0); true
			with Not_found -> false;;

(*JSON PARSER FUNCTIONS*)
let rec load_stats l team_name =
	match l with 
	| h::t -> if contains h team_name then h else load_stats t team_name
	| [] -> "null";;

let rec get_stat l stat =
	match l with
	| h::t -> if contains h stat then Str.string_after h ((Str.search_forward (Str.regexp_string ": ") h 0) + 2) else get_stat t stat
	| [] -> "null";;

let rec get_points_from_form form (points:int) (n:int) = 
	if n > 4 then points
	else begin
		match form.[n] with 
		| 'W' -> get_points_from_form form (points + 3) (n + 1)
		| 'D' -> get_points_from_form form (points + 1) (n + 1)
		| 'L' -> get_points_from_form form points (n + 1)
		| null -> points
	end;;

(*PRINT FUNCTIONS*)
let print_stats rank name win draw lose goals_for goals_against points form =
	printf "\n%d. %s  (W:%d  D:%d  L:%d  Goals:%d-%d   Pts:%d  Form:%s)" rank name win draw lose goals_for goals_against points form;;

let print_bet home_bet away_bet draw_bet = 
	printf "\n\nHome win: %.2f\n" home_bet;
	printf "Draw:     %.2f\n" draw_bet;
	printf "Away win: %.2f\n\n" away_bet;;

(*BET ALGORITHM FUNCTIONS*)
let calculate_power_ratio (rank:int) (win_percentage:float) (goal_percentage:float) (ppg:float) (goals_diff:int) (form_points:int) =
	(0.22 *. win_percentage +. 4.0) +. 
	(0.184 *. goal_percentage +. 1.816) +. 
	(1.2 *. (float_of_int) form_points +. 2.0) +. 
	(4.33 *. ppg +. 2.0) +. 
	(-0.32 *. (float_of_int) rank +. 6.32) +. 
	(0.0663 *. (float_of_int) goals_diff +. 1.875);;

let get_team_power_ratio json_list team_name = 
	let stats = load_stats json_list team_name in
	let stats_list = Str.split(Str.regexp ",") stats in

	let rank = (int_of_string) (get_stat stats_list "rank") in
	let form = Str.global_replace (Str.regexp_string "\"") "" (get_stat stats_list "forme") in
	let matches_played = (float_of_string) (get_stat stats_list "matchsPlayed") in
	let win = (float_of_string) (get_stat stats_list "win") in
	let draw = (int_of_string) (get_stat stats_list "draw") in
	let lose = (int_of_string) (get_stat stats_list "lose") in
	let goals_for = (float_of_string) (get_stat stats_list "goalsFor") in
	let goals_against = (float_of_string) (get_stat stats_list "goalsAgainst") in
	let goals_diff = (int_of_string) (get_stat stats_list "goalsDiff") in
	let points = (float_of_string) (Str.global_replace (Str.regexp_string " ") "" (get_stat stats_list "points")) in

	let win_percentage = win /. matches_played *. 100.0 in
	let goal_percentage = goals_for /. (goals_for +. goals_against) *. 100.0 in
	let ppg = points /. matches_played in
	let form_points = get_points_from_form form 0 0 in

	let power_ratio = calculate_power_ratio rank win_percentage goal_percentage ppg goals_diff form_points in
	print_stats rank team_name ((int_of_float) win) draw lose ((int_of_float) goals_for) ((int_of_float) goals_against) ((int_of_float) points) form;
	power_ratio;;

let get_home_bet (home_ratio:float) (away_ratio:float) = 
	0.964 ** (home_ratio -. away_ratio -. 13.0) +. 1.0;;

let get_away_bet (home_ratio:float) (away_ratio:float) = 
	0.964 ** (away_ratio -. home_ratio -. 13.0) +. 1.0;;

let get_draw_bet (home_ratio:float) (away_ratio:float) = 
	0.002041 *. ((home_ratio -. away_ratio) ** 2.0) +. 2.8;;
 

(*MAIN FUNCTION*)
let main_loop =
	let quit_loop = ref false in
	while not !quit_loop do
	printf "\n~~ Premier League Predictions ~~\n";

	printf "\nEnter home team: ";
	let home_team = read_line () in
	printf "Enter away team: ";
	let away_team = read_line () in

	let json_file_string = load_file_into_string json_file_name in
	let json_list = Str.split(Str.regexp "[{}]") json_file_string in

	let home_power_ratio = get_team_power_ratio json_list home_team +. 3.0 in
	let away_power_ratio = get_team_power_ratio json_list away_team in

	let home_bet = get_home_bet home_power_ratio away_power_ratio in
	let away_bet = get_away_bet home_power_ratio away_power_ratio in
	let draw_bet = get_draw_bet home_power_ratio away_power_ratio in

	print_bet home_bet away_bet draw_bet;

	printf "Continue? (y/n):";
	let continue = read_line () in
	if continue.[0] = 'n' then quit_loop := true else quit_loop := false;

	done;;

main_loop;;