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
#'   Selecting a Door
#' @description
#'   A door is randomly selected. 
#' @details
#'   A vector is created assigning 1, 2, and 3 to "doors". 
#'   A door 1, 2, or 3 is randomly selected. 
#'   A selection is returned. 
#' @param 
#'   doors <- c(1,2,3) 
#'   a.pick <- sample( doors, size=1 )
#'   return( a.pick )  # number between 1 and 3
#' @return 
#'   a. pick, a number between 1 and 3
#' @examples
#'   1, 2, or 3
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open Goat Door
#' @description
#'   The host opens one of the remaining two doors. 
#' @details
#'   The host opens one of the remaining two doors not selected by
#'   the player in the previous step.The door selected has a goat 
#'   behind it. 
#' @param 
#'   if( game[ a.pick ] == "car" )
#'   goat.doors <- doors[ game != "car" ] 
#'   opened.door <- sample( goat.doors, size=1 )
#'   if( game[ a.pick ] == "goat" )
#' @return 
#'   A door 1, 2, or 3. Cannot be the same as the door selected above.
#' @examples
#'   1, 2, or 3. 
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
#'   Change or Stay
#' @description
#'   The player can choose to stay with the door they selected, 
#'   or change to one of the two remaining doors. The final 
#'   door is whichever door has not alreadybeen selected by 
#'   the player or the host. 
#' @details
#'  If the play stays, the final pick is the first door selected, 
#'  a.pick. If the player changes, the remaining door is final.pick,
#'  any door that is not the opened doors. 
#' @param 
#'  if( stay ), final.pick <- a.pick
#'  if( ! stay ), final.pick <- doors[ doors != opened.door & doors != a.pick 
#' @return 
#'  A door between 1 and 3 is returned. 
#' @examples
#'  1, 2 or 3
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
#'   Determine Winner
#' @description
#'   If the player selected the door with the car behind it, they win. 
#'   If the player selected a door with a goat behind it, they lose.
#' @details
#'   If final.pick is equal to "car", the player wins. 
#'   If fina.pick is equal to "goat", the player losers. 
#' @param 
#'   if( game[ final.pick ] == "car" ), return( "WIN" )
#'   if( game[ final.pick ] == "goat" ),return( "LOSE" )
#' @return 
#'   A message of "WIN" or "LOSE" is returned. 
#' @examples
#'  "WIN" or "LOSE"
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
#'   Game Strategy
#' @description
#'   The game is assigned to a function, play_game.
#' @details
#'   Each step to playing the game is list and assigned to vectors. 
#' @param 
#'   new.game <- create_game()
#'   first.pick <- select_door()
#'   opened.door <- open_goat_door( new.game, first.pick )
#'   final.pick.stay <- change_door( stay=T, opened.door, first.pick )
#'   final.pick.switch <- change_door( stay=F, opened.door, first.pick )
#'   outcome.stay <- determine_winner( final.pick.stay, new.game  )
#'   outcome.switch <- determine_winner( final.pick.switch, new.game )
#'   strategy <- c("stay","switch
#'   outcome <- c(outcome.stay,outcome.switch)
#'   game.results <- data.frame( strategy, outcome,stringsAsFactors=F )
#' @return 
#'   game.results are returned, "WIN" or "LOSE".
#' @examples
#'  "WIN" or "LOSE"
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
#'   Play the Game 100 Times
#' @description
#'   Uses a loop to simluate playing the game 100 times. 
#' @details
#'   A function is created that loops the game until it is played 100 times. 
#'   The results of the games are returned.
#' @param 
#'   play_n_games <- function( n=100 )
#'   results.list <- list()   # collector
#'   loop.count <- 1
#'   game.outcome <- play_game()
#'   results.list[[ loop.count ]] <- game.outcome
#'   loop.count <- loop.count + 1
#'   results.df <- dplyr::bind_rows( results.list )
#'   table( results.df )
#' @return 
#'   The results of 100 game plays are returned. 
#' @examples
#'   WIN: 70 LOSE: 30
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
