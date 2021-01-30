#' @title
#' Monty Hall Simulation
#'
#' @description
#' `game()` returns the probablity of winning while staying or switching doors. The user
#' is able to determine how many doors they would like to use by determining the amount of goats
#' and cars.
#'
#' @details
#' This is a function is a form of the famous gameshow "Let's Make a Deal" that was presented by the
#' famous Candian gameshow host Monty Hall. The original game consisted of three doors where two doors 
#' concealed goats and one door concealed a car. The player was asked to chose a door in hopes that it was 
#' the car. Once the door was selected, Monty Hall would then reveals one of the goats. This leaves the player's
#' door and another door. The player is then given the option to either keep their initial door or switch with the
#' remaining door. This simulation tests whether a player increases their odds by switching doors or staying with
#' their initial door.  
#'
#' @param ... This function does not take any arguments
#' 
#' @return The function will first ask the player to enter the number of goats and cars. Then the function will
#' return the probability of winning if a player choses to stay or switch.
#'
#' @examples
#' game()
#'
#'@export
game <- function()
{
  c2 <- readline("How many cars? ")
  g2 <- readline("How many goats? ")
  play_game <- function( )
  {
    `%notin%` <- Negate(`%in%`)
    c <- as.numeric(c2)
    c1 <- rep("car", c)
    g <- as.numeric(g2)
    g1 <- rep("goat", g)
    c1.g1 <- c(c1, g1)
    create_game <- function( )
    {
      a.game <- sample( c1.g1, size=(c+g), replace=F )
      return( a.game )
    } 
    select_door <- function( )
    {
      
      doors <- 1:(c+g)
      a.pick <- sample(doors, size = 1, replace = F) # YOUR CODE HERE...
      return( a.pick )  # number between 1 and 3
      
    }
    open_goat_door <- function( a.game, a.pick )
    {
      if(a.pick %in% which(a.game == "car")){
        
        r.door <- which(a.game == "car")
        num <- (1:(c+g))
        remainder <- num[-(r.door)]
        opened.door <- sample(remainder, size = 1, replace = F)
        
        return( opened.door )
        
      }else if(a.pick %notin% which(a.game == "car")){
        goats <- which(a.game == "goat")
        pick <- a.pick
        r.door <- goats[goats != pick]
        r.door <- as.character(r.door)
        r.door <- sample(r.door, size = 1, replace = F)
        r.door <- as.numeric(r.door)
        opened.door <- r.door
        
        return( opened.door )
      }
    }
    change_door <- function( stay=T, opened.door, a.pick )
    {
      
      # YOUR CODE HERE...
      if(stay==T){
        final.pick <- a.pick
        return( final.pick )
      }else {
        num <- 1:(c+g)
        available <- which(num != a.pick & num != opened.door)
        available <- as.character(available)
        final.pick <- sample(available, size = 1, replace = F)
        final.pick <- as.numeric(final.pick)
        return( final.pick )
      }
    }
    determine_winner <- function( final.pick, this.game )
    {
      
      if( final.pick %in% which(this.game == "car") )
      {
        return( "WIN" )
      }
      else
      {
        return( "LOSE" )
      }
    }
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
  play_game()
  
  
  results.df <- NULL   # collector
  
  for( i in 1:10000 )  # iterator
  {
    game.outcome <- play_game()
    # binding step
    results.df <- rbind( results.df, game.outcome )
  }
  
  table( results.df ) 
  #
  
  ####
  library(dplyr)
  table( results.df ) %>% 
    prop.table( margin=1 ) %>% 
    round( 2 )
}

#'@title
#'Play game with desired number of cars and goats
#'
#'@description
#'`play_game()` returns the results of the game given the number of cars and goats inputted by 
#'the player. 
#'
#'@details
#'This function is a wrapper function that runs through the game. If used independently, this 
#'function would only run through the game once as opposed to the full simulation. 
#'
#'@param ... This function does not take any arguments.
#'
#'@return
#'This function returns a character vector that states the results of one game.
#'
#'@examples
#'play_game()
#'
#'@export
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

#'@title
#'Creates vector of user inputs
#'
#'@description
#'This function creates a character vector of length three that consists of two goats and one car. 
#'The goats and car are randomly assembled to create unique combimations.
#'
#'@details
#'This function is the basis of game and is referenced many times throughout the game.
#'
#'@param ... This function does not take any arguments
#'
#'@return This function returns a character vector, a.game. 
#'
#'@examples
#'create_game()
#'
#'@export
create_game <- function( )
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}  

#'@title
#'Player selects a door
#'
#'@description
#'This function creates a numeric vector of length one. This vector represents the door that the 
#'player selected and is either 1, 2, or 3. 
#'
#'@details
#'The door is selected at random using `sample()` and will represent the either a goat or car based on
#'the positions in a.game
#'
#'@param ... This function does not take any arguments
#'
#'@return This function returns a numeric vector of length one called a.pick
#'
#'@examples
#'select_door()
#'
#'@export
select_door <- function( )
{
  
  doors <- c(1,2,3) 
  a.pick <- sample(doors, size = 1, replace = F) # YOUR CODE HERE...
  return( a.pick )  # number between 1 and 3
  
}

#'@title
#'Open a door that is not the player's door or car door
#'
#'@description
#'This function first determines which goat door should be opened. The door is neither the car door or 
#'the player's door. 
#'
#'@details
#'This function first determines whether the player chose the car door. If this is true, then a random 
#'goat door is selected. if the player did not chose the car door, then the door that is not the player's 
#'initial choice and the door that does not contain the car are excluded from the remaining doors. The remaining
#'door is then opened.  
#'
#'@param a.game A character vector of length three
#'@param a.pick A numeric vector of length one
#'
#'@return
#'This function returns a numeric vector that is the position of the remaining door
#'
#'@examples
#'open_goat_door(a.game = c("goat", "car", "goat"), a.pick = 1)
#'
#'open_goat_door(a.game = c("goat", "car", "goat"), a.pick = 2)
#'
#'@export
open_goat_door <- function( a.game, a.pick )
{
  if(a.pick == which(a.game == "car")){
    
    r.door <- which(a.game == "car")
    num <- (1:3)
    remainder <- num[-(r.door)]
    opened.door <- sample(remainder, size = 1, replace = F)
    
    return( opened.door )
    
  }else if(a.pick != which(a.game == "car")){
    goats <- which(a.game == "goat")
    pick <- a.pick
    r.door <- goats[goats != pick]
    opened.door <- r.door
    
    return( opened.door )
  }
}

#'@title
#'Switch or stay with initial door
#'
#'@description
#'If the player wishes to stay with their initial door, then their initial pick is made their final pick.
#'If the player wishes to switch to the remaining unopen door, then their final pick is made the unopened door.
#'
#'@details
#'This function determines whether or not the player wishes to stay through a logical argument. If true,
#'then the final pick is returned as the initial pick. If false, then the position of the door that was neither the 
#'player's first choice or opened door is determined. This value is then made the player's final pick.
#'
#'@param stay This argument's default vaule is TRUE but can be made FALSE.
#'@param opened.door This argument is the numeric vector of length one equal to the opened door.
#'@param a.pick This is a numeric vector of length one equal to the player's initial door.
#'
#'@return
#'This function returns the final pick of the player based on whether they chose to stay or switch.
#'
#'@examples
#'change_door(stay = T, opened.door = 3, a.pick = 1)
#'
#'change_door(stay = F, opened.door = 3, a.pick = 1)
#'
#'@export
change_door <- function( stay=T, opened.door, a.pick )
{
  if(stay==T){
    final.pick <- a.pick
    return( final.pick )
  }else {
    num <- 1:3
    final.pick <- which(num != a.pick & num != opened.door)
    return( final.pick )
  }
}

#'@title
#'Determine whether the final door is a winner
#'
#'@description
#'This function returns whether or not the player's final door choice contains the car or a goat. If
#'the door was concealing the car, the player wins. If the door was concealing a goat, the player loses.
#'
#'@details
#'This function checks whether final door number is equal to the position of the car in the game. If yes,
#'then "WIN" is returned. If no, then "LOSE" is returned. 
#'
#'@param final.pick A numeric vector of length one.
#'@param this.game A character vector of length three.
#'
#'@return
#'This function returns whether or not the player won or lost. 
#'
#'@examples
#'determine_winner(final.pick = 1, this.game = c("car", "goat", "goat"))
#'
#'determine_winner(final.pick = 2, this.game = c("goat", "car", "goat"))
#'
#'@export
determine_winner <- function( final.pick, this.game )
{
  
  if( final.pick == which(this.game == "car") )
  {
    return( "WIN" )
  }
  else
  {
    return( "LOSE" )
  }
}


