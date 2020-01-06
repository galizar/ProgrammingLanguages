# University of Washington, Programming Languages, Homework 6, hw6runner.rb

require_relative './hw6provided.rb'

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2,1]]), # p
                                [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]], # extra-long, only needs two
                                 [[0, 2], [0, 1], [0, 0], [0, -1], [0, -2]]],
                                rotations([[1, 0], [0, 0], [0, 1]])] # small-l
  Cheat_Piece = [[[0,0]]]
  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

end

class MyBoard < Board
  # your enhancements here

  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheating = false
  end

  def next_piece
    cheating = @cheating
    if cheating
      @current_block = MyPiece.new(MyPiece::Cheat_Piece, self)
      @current_pos = nil
      @cheating = false
    else
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
        @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def attempt_cheat
    cheating = @cheating
    if @score >= 100 && !cheating
      @score -= 100
      @cheating = true
    end
  end

end

class MyTetris < Tetris
  # your enhancements here

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('c', proc {@board.attempt_cheat})
    @root.bind('u', proc {@board.rotate_clockwise;@board.rotate_clockwise})
  end

end


