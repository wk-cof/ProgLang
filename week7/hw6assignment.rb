# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.
class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  def key_bindings
    super
    @root.bind('u', proc do
      @board.rotate_clockwise
      @board.rotate_clockwise
    end)

    @root.bind('c', proc{@board.cheat})
  end
end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces.concat([rotations([[0,0],[0,1],[1,0],[1,1],[0,2]]),   #weird square
     [[[-2,0],[-1,0],[0,0],[1,0],[2,0]],[[0,-2],[0,-1],[0,0],[0,1],[0,2]]], #long line
     rotations([[0,0],[0,1],[1,0]])])              #angle

  def self.next_piece (board)
    if (!block_given?)
      MyPiece.new(All_My_Pieces.sample, board)
    else
      MyPiece.new(yield,board)
    end
  end
end

class MyBoard < Board
  def cheat
    if (@score >= 100 && !@cheat_enabled)
      @score -= 100
      @cheat_enabled = true
    end
  end

  def next_piece
    if (@cheat_enabled)
      @current_block = MyPiece.next_piece(self) {[[0,0]]}
      @cheat_enabled = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end
