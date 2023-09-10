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
#' select a door for your first guess.
#'
#' @description
#' 'select_door" randomly selects a door for the contestant
#' to reveal a goat or a car.
#'
#' @details
#' there are three doors for a contestant to choose from, one of
#' which has a car behind it and two have goats.
#' The contestant selects a door, then the host opens a door to reveal
#'  a goat or a car.
#'
#' @param
#' If the door selected reveals a car, the game ends.
#' @return
#' The function returns one character vector of either a goat or car
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' The host reveals a goat.
#'
#' @description
#' 'open_goat_door' allows the host to reveal one goat by opening one
#' of the three doors. This leaves two doors remaining.
#'
#' @details
#' The host opens one of the three doors to reveal a goat. The contestant
#' is given an opportunity to switch to the unopened door or stay with
#' their original selection. The door reveals will always have a goat behind
#' it. If the contestant selected a car, then either door can be opened.
#' However, if a goat is selected, then there is only one door that can be
#' revealed.
#'
#'
#' @param
#' If contestant selected car, randomly select one of the two goats.
#' The parameter for this function is to open a door that is not the current
#' pick and does not contain a car.
#'
#'
#'
#' @return
#' This function will remove one door from the selection process by revealing
#' a goat from door 1,2, or 3.
#'
#' @examples
#' open_goat_door()
#'
#'
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
#' Option to change door selected.
#'
#' @description
#' the 'change_door'  function allows the contestant to change their initial
#' selection to the other door that is still closed.
#'
#'
#' @details
#' This function represents the playing strategy as the argument, staying or
#' switching. With this, the contestant is offered an opportunity to switch
#' to the closed door or stay with original selection.
#'
#'
#'
#' @param
#' In this, staying is represented as True or False
#'
#' @return
#' Function will return final pick for the contestant. They either stay with
#' initial pick (Stay= T), or change to closed door (Stay=F).
#'
#' @examples
#' change_door()
#'
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
#' Determine if the contestant is a winner
#'
#' @description
#' determine_winner function evaluates whether the final pick from the
#' contestant is a winner by determining if the door held a car or a goat.
#'
#' @details
#' This function determines if the final pick was a car or a goat. If it were
#' a car, then the game is marked as a win. If its a goat, then it returns
#' as a loss.
#'
#' @param
#' the parameters are, if final pick is goat=loss and if car = win.
#'
#' @return
#' The return is either a loss or a win.
#'
#' @examples
#' determine_winner ()
#'
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
#' create a new Monty Hall Problem game using the previous functions
#'
#' @description
#' play_game combines all the functions established above to play one round
#' of the Monty Hall game.
#'
#'
#' @details
#' This function will execute each step of a single game in order
#'
#' @param
#' no arguments are used in this function
#'
#' @return
#' play_game will run an entire single game of Monty Hall and produce a win or
#' loss determination depending on the strategy.
#'
#' @examples
#' play_game ()
#'
#'
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
#' run a simulation loop
#'
#' @description
#' play_n_games function will run a simulation for a specified number of loops.
#'
#'
#' @details
#' This function will run a simulation to establish the best strategy for
#' winning. If ran 10,000 times, the simulated statistic will converge close
#' to the actual theoretical value.
#'
#' @param
#' no argument
#'
#' @return
#' The function will return the number of wins and losses for each strategy:
#' staying with the initial door selected or switching after a goat door is
#' opened.
#'
#' @examples
#' play_n_games
#'
#'
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
