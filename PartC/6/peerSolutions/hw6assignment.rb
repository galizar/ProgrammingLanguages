# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here

  All_My_Pieces = [
    # square (only needs one)
    [[[0, 0], [1, 0], [0, 1], [1, 1]]],
    # T
    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]),
    # long (only needs two)
    [
      [[0, 0], [-1, 0], [1, 0], [2, 0]],
      [[0, 0], [0, -1], [0, 1], [0, 2]]
    ],
    # L
    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]),
    # inverted L
    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]),
    # S
    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]),
    # Z
    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),
    # new pieces
    # fat L
    rotations([[-1, -1], [-1, 0], [0, 0], [0, -1], [1, 0]]),
    # long five (only needs two)
    [
      [[0, -1], [0, 0], [0, 1], [0, 2], [0, 3]],
      [[-1, 0], [0, 0], [1, 0], [2, 0], [3, 0]]
    ],
    # small L
    rotations([[0, -1], [0, 0], [1, 0]])
  ]

  Cheat_Piece = [[[0, 0]]]

  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_cheat_piece (board)
    MyPiece.new(Cheat_Piece, board)
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @cheat = false # flag for cheat
    @game = game
    @delay = 500
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index|  # modify for different sizes
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # gets the next piece
  def next_piece
    if @cheat
      @current_block = MyPiece.next_cheat_piece(self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  # rotates the current piece 180
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, -2)
    end
    draw
  end

  def cheat
    if !game_over? and @game.is_running? and (@score >= 100) and !@cheat
      @score -= 100
      @cheat = true
    end
    draw
  end
end

class MyTetris < Tetris
  # your enhancements here

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end
end
