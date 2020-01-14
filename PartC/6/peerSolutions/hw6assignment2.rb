class MyPiece < Piece

	# class method to choose the next piece
	def self.next_piece (board)
		MyPiece.new(All_My_Pieces.sample, board)
	end

	def self.cheat_piece (board)
		MyPiece.new([[[0,0]]], board)
	end

	# class array holding all the pieces and their rotations
	All_My_Pieces = All_Pieces + [rotations([[0, 0], [-1, 0], [-1, -1], [1, 0], [0, -1]]),
	                              [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], # long (only needs two)
	                               [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
	                              rotations([[0, 0], [-1, 0], [0, -1]])]

end

class MyBoard < Board

	def initialize (game)
		@grid = Array.new(num_rows) {Array.new(num_columns)}
		@current_block = MyPiece.next_piece(self)
		@score = 0
		@game = game
		@delay = 500
		@cheat_active = false
	end

	# gets the next piece
	def next_piece
		if @cheat_active then
			@current_block = MyPiece.cheat_piece(self)
			@cheat_active = false
		else
			@current_block = MyPiece.next_piece(self)
		end
		@current_pos = nil
	end

	# gets the information from the current piece about where it is and uses this
	# to store the piece on the board itself.  Then calls remove_filled.
	def store_current
		locations = @current_block.current_rotation
		displacement = @current_block.position
		locations.each_with_index{|current, index| 
			@grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
			@current_pos[index]
		}
		remove_filled
		@delay = [@delay - 2, 80].max
	end

	# rotates the current piece 180 degress
	def rotate_180
		if !game_over? and @game.is_running?
			@current_block.move(0, 0, 2)
		end
		draw
	end

	# activate cheate
	def cheat
		if not @cheat_active and @score >= 100 then
			@cheat_active = true
			@score -= 100
		end
	end

end

class MyTetris < Tetris

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


