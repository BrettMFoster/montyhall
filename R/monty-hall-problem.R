#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}
#' @title
#' The contestant selects a door offered by the host.
#' @description
#' `select_door()` allows the contestant to select a random door between
#' door 1, door 2, and door 3.
#' @details
#' This function allows the contestant to select a door that will be used
#' in the next step when the host will open the door selected by the contestant.
#' @param
#' no params
#' @return
#' Returns a numeric value between 1 and 3 that correlates with door choice and
#' the number chosen will be matched to a door from create_game()
#' @examples
#' select_door() = 1
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}


#' @title
#' The host receives the door selection from contestant and opens a different door.
#' @description
#' `open_goat_door()` Makes it so the host will always open a door different from the door
#' selected by the contestant and the door the host opens will always have a goat behind it.
#' @details
#' The host opens one of the doors that the contestant did not select and the host opens will
#' always have a goat behind it. The contestant will be asked to stick with the original pick
#' or change the door the contestant selected.
#' @param
#' no param
#' @return
#' Returns a numeric value between 1 and 3 that correlates with a door with a goat behind it.
#' @examples
#' open_goat_door() selection()=1 therefore, host selects a door with a goat behind it from
#' the doors 2 or 3.
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Contestant given option to change door.
#' @description
#' `change_door()` The host gives the contestant the option to change the original
#' selection to the other remaining unselected door.
#' @details
#' The contestant has an option to stay with the original choice in select_door()
#' or to change the selection to the last remaining unselected door. Depending on
#' the game's paramaters, it is generally a better strategy to switch doors.
#' @param
#' no params
#' @return
#' Returns the number that corresponds to the unopened door that the contestant
#' did not select in select_door(). Changing the door prompts the next step where
#' the host reveals whether the contestant's final choice reveals a car or goat.
#' @examples
#' Select_door()=1; open_goat_door()=2; change_door()=3
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}


#' @title
#' Goat or No Goat
#' @description
#' `determine_winner()` The final step in the game is for the host to reveal
#' whether the contestant's final choice is a goat or car.
#' @details
#' The contestant has a slightly increased chance to pick a car if the contestant
#' switched in the previous step. This step reveals if switching doors wins a car or
#' a goat.
#' @param
#' no params
#' @return
#'If the final selection is a car, then 'WIN' is received; if not, then 'LOSE' is
#'received.
#' @examples
#' change_door()=3; select_game()='goat','goat','car'; and determine_winner()=
#' 'WIN'; else the contestant does not switch and change_door()=1...
#' determine_winner()='LOSE'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}


#' @title
#' Plays a full monty hall game and returns the results for one game.
#' @description
#' `play_game()` returns the results (i.e., win or lose) for one full monty hall
#' game.
#' @details
#' This function allows the user to test the results of one randomized monty hall
#' game; whether staying or switching won or lost.
#' @param
#' none
#' @return
#' Return indicates whether staying or switching won for this one game.
#' @examples
#' play_game(): Outcome: Stay: 1; Switch: 0.
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}

#' @title
#' Runs 100 games providing cumulative results.
#' @description
#' `play_n_games()` runs the monty hall game 100 times, compiles the results,
#' and provides a table that shows the percentages won or lost for each strategy.
#' @details
#' The results show the win or lose rate for each strategy given the parameters
#' of the game. This win determine which strategy is better over a large sample
#' of attempts.
#' @param
#' none.
#' @return
#' Indicates the percentage of wins or loses for staying or switching over 100
#' games.
#' @examples
#' Outcome: Staying--Won 30% vs Lose 70%. Switching--Won 41% vs. Lose 59%
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
