//////////////////////////////////////////////////////////////////////////////
//
//   tictactoe.j
//
//   Description
//
//   Started:           991228
//   Modifications:
//
//   Purpose:
//
//   Calling Sequence:
//
//
//   Inputs:
//
//
//   Outputs:
//
//
//   Example:
//
//   Notes:  The board is described by an array which represents a clockwise
//  spiral starting in the upper-left corner. This corner is element 0; the
//  center is element 8.
//
//////////////////////////////////////////////////////////////////////////////
var filled = 0;  //Represents which cells have been taken already.
var player = 0;  //Represents which cells are occupied by the human.
var user_turn = true;
var turn = 0;
var gameOver = false;

function choose_cell(cell_num) {
   if ( user_turn  &&  !gameOver ) {
      var index = 1 << cell_num;
   
      if ( filled & index ) {
         alert('The cell has already been filled!');
      } else {
         user_turn = false;  //Now it's the computer's turn.
         //
         //   Update the status words.
         //
         player |= index;
         filled |= index;

         turn++;
   
//         cell_dehighlight(cell_num);
         cell_highlight(cell_num);   //Display the image highlighted in the newly occupied cell.
         pc_turn();
      }
   }
}

function pc_turn() {
   var pc_choose, index;

   if ( turn == 1 ) {   //Take the center on the first turn if available.
      if ( filled & 0x100 ) {
         pc_choose = 0;  //Otherwise just take upper-left corner.
      } else {
         pc_choose = 8;
      }
   } else if ( turn == 2 ) {
      pc_choose = twoInRow(player);

      if ( pc_choose < 0 ) {
         pc_choose = sneakAttack(player);

         if ( pc_choose < 0 ) {
            pc_choose = anyOldCell();
         }
      }
   } else {
      winner = whoWon(player);

      if ( winner ) {
//        gameOver();
alert('You won');
gameOver = true;
         return;
      } else {
         pc_choose = twoInRow(player ^ filled);
   
         if ( pc_choose < 0 ) {
            pc_choose = twoInRow(player);
   
            if ( pc_choose < 0 ) {
               pc_choose = anyOldCell();
            }
         }
      }
   }

   //
   //   Update the status word.
   //
   index = 1 << pc_choose;
   filled |= index;
   //
   //   Display computer's move.
   //
//   cell_highlight(pc_choose);
   cell_dehighlight(pc_choose);

   winner = whoWon(player ^ filled);

   if ( winner ) {
//        gameOver();
alert('I won');
gameOver = true;
      return;
   } else if ( turn == 4 ) {
alert('Tie game');
gameOver = true;
   }

   user_turn = true;
}

function twoInRow(my_filled) {
   var patterns = new Array(0x06, 0xC0, 0x110,          //Fill cell 0
                            0x05, 0x120,                //Fill cell 1
                            0x03, 0x18, 0x140,          //Fill cell 2
                            0x180, 0x14,                //Fill cell 3
                            0x60, 0x0C, 0x101,          //Fill cell 4
                            0x50, 0x102,                //Fill cell 5
                            0x30, 0x81, 0x104,          //Fill cell 6
                            0x108, 0x41);               //Fill cell 7
   var i, j;
   var loop_length = 3;
   var index = 1;
   var this_pattern = 0;

   for (i = 0; i < 8; i++) {
      for (j = 0; j < loop_length; j++) {
         if ( index & filled ) {
            //
            //   If the current cell is already filled, no need to block it.
            //
            this_pattern += loop_length;
            break;
         }

         if ( (my_filled & patterns[this_pattern]) == patterns[this_pattern] ) {
            return i;
         }

         this_pattern++;
      }

      loop_length ^= 1;   //Toggle loop length between 3 and 2.
      index <<= 1;   //Check next cell.
   }
   //
   //   No match found.
   //
   return -1;
}

function sneakAttack(my_filled) {
   var sneakPatterns = new Array(0x84, 0x82, 0x42,         //Fill cell 0
                                 0x110, 0x09, 0x0A, 0x12,  //Fill cell 2
                                 0x11, 0x44,               //Fill cell 3
                                 0x24, 0x28, 0x48,         //Fill cell 4
                                 0x21, 0xA0, 0x90);        //Fill cell 6

   var loop_lengths = new Array(3, 0, 4, 2, 3, 0, 3);
   var i, j;
   var index = 1;
   var this_pattern = 0;

   for (i = 0; i < 7; i++) {
      for (j = 0; j < loop_lengths[i]; j++) {
         if ( index & filled ) {
            //
            //   If the current cell is already filled, no need to block it.
            //
            this_pattern += loop_lengths[i];
            break;
         }

         if ( (my_filled & sneakPatterns[this_pattern]) == sneakPatterns[this_pattern] ) {
            return i;
         }

         this_pattern++;
      }

      index <<= 1;   //Check next cell.
   }
   //
   //   No match found.
   //
   return -1;
}

function anyOldCell() {
   var preferences = new Array(0, 2, 4, 6, 1, 3, 5, 7);
   var i, index;
   i = 0;
   index = 1 << preferences[i];

   while ( filled & index ) {
      i++;
      index = 1 << preferences[i];
   }

   return preferences[i];
}

function whoWon(winner) {
   var winPatterns = new Array(0x07,            //Row 1
                               0x188,           //Row 2
                               0x70,            //Row 3
                               0xC1,            //Row 4
                               0x122,           //Row 5
                               0x1C,            //Row 6
                               0x144,           //Row 7
                               0x111);          //Row 8

   var i;

   for (i = 0; i < 8; i++) {
      if ( (winner & winPatterns[i]) == winPatterns[i] ) {
         return true;
      }
   }

   return false;
}

function new_game() {
   filled = 0;  //Represents which cells have been taken already.
   player = 0;  //Represents which cells are occupied by the human.
   user_turn = true;
   turn = 0;
   gameOver = false;

   for (var i = 0; i < 9; i++) {
      document['cell' + i].src = 'images/cell_blank.gif';
   }
}