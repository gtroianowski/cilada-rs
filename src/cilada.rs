use crate::cilada::Shape::{Circle, Plus, Square};
use itertools::Itertools;
use log::{debug, info, trace};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt;
use std::fmt::{Debug, Display, Formatter, Write};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::Deref;

const ALL_GAME_COMBINATIONS: [&str; 50] = [
    "AABCDDEFGIKN", "ABCDDEFFGJKM", "ABCCDEFFIJMN", "ABDDEEFFIKMN", "ABBCDEFFIJKM",
    "ABBCCCDFIJKM", "ABBCCCDDEFFKJ", "ABBCCCDDHKLN", "ABBBCCDDEFFHL", "ABBBCCDDEFFLN",
    "ABBBCCCDDFFGI", "AABCDEFFGIJM", "AABCDDEFGKNM", "AABCDEFFGHIJ", "AABCDEFFGJMN",
    "AABCCDEFIJKM", "AABCCDDEEFFHN", "AABCCCDDGHIL", "AABBCCDFGIJK", "AABBCCDFGJLN",
    "AABBCCCDEFFIJ", "AABBCCCDEFFJN", "AABBCCCDJKLM", "AABBCCCDDEFHK", "AABBCCCDEFFIJ",
    "AABBFGIJKMN", "AABBBCFFGJMN", "AABBBCCCDDFGL", "AAABCCDEEFFIJ", "AAABCCDEEFFHJ",
    "AAABCDDEEFFGN", "AAABCCDDEEFKM", "AAABBCEFJKMN", "AAABBEFFGJMN", "AAABBBCCCDFGH",
    "AAABBBCCEFFIJ", "AAABCDEFGIMN", "AAABCCDDEEFHL", "AAABCCCDGIJL", "AAABBCEFIJKM",
    "AAABBCCCDDEEFF", "AAABBCCDGKHM", "AAAABBCCDEFGI", "AAACCCDEIJLN", "AAABBCCCIJLN",
    "AAAABCCDEEFHM", "AAAABCCDEEFIM", "AAAABBBCCCGFJ", "AAAABBBCCDEEFF", "BBBDDEFFKLMN",
];

type PieceIndex = char;

const SQUARE_SHAPE_UTF: char = '▢';
const CIRCLE_SHAPE_UTF: char = '◯';
const PLUS_SHAPE_UTF: char = '✚';

pub enum ValidationStatus {
    Success,
    OverlapError(String),
    ShapeMismatch(String),
    IncompleteSolution(String),
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug)]
pub struct PieceCollection(BTreeSet<PlacedPiece>);


impl Deref for PieceCollection {
    type Target = BTreeSet<PlacedPiece>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Vec<&PlacedPiece>> for PieceCollection {
    fn from(value: Vec<&PlacedPiece>) -> Self {
        PieceCollection { 0: value.into_iter().cloned().collect() }
    }
}

impl Hash for PieceCollection {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut result = 0_u64;
        for element in self.0.iter() {
            let mut hasher = DefaultHasher::new();
            element.hash(&mut hasher);
            let element_hash = hasher.finish();
            result ^= element_hash;
        }
        state.write_u64(result);
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for element in data {
            element.hash(state);
        }
    }
}

struct PieceSetup {
    data: PieceData,
    count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Shape {
    Plus,
    Square,
    Circle,
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let char_repr = match self {
            Plus => PLUS_SHAPE_UTF,
            Circle => CIRCLE_SHAPE_UTF,
            Square => SQUARE_SHAPE_UTF
        };
        write!(f, "{}", char_repr)
    }
}

#[derive(Hash, Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
struct PieceData {
    height: u32,
    width: u32,
    data: Vec<Option<Shape>>,
}

impl PieceData {
    fn new(height: u32, width: u32, data: Vec<Option<Shape>>) -> Self {
        if data.len() != (height * width) as usize {
            panic!("Inconsistent data length: width: {}, height: {}, data: {:?}",
                   width,
                   height,
                   data);
        }
        PieceData { height, width, data }
    }

    fn get_non_empty_shapes_and_coords(&self) -> Vec<(u32, u32, Shape)> {
        let mut result = Vec::with_capacity((self.height * self.width) as usize);
        for row in 0..self.height {
            for column in 0..self.width {
                let option = self.data[(row * self.width + column) as usize].clone();
                if option.is_some() {
                    result.push((row, column, option.unwrap()));
                }
            }
        }
        result
    }

    /// creates a new PieceTemplate by rotating `self` counterclockwise.
    fn create_rotated_piece(&self) -> Self {
        let new_width = self.height;
        let new_height = self.width;
        let mut result = Vec::with_capacity(self.data.len());
        for column in (0..self.width).rev() {
            for row in 0..self.height {
                let option = self.data[(row * self.width + column) as usize].clone();
                result.push(option);
            }
        }
        PieceData { height: new_height, width: new_width, data: result }
    }

    fn get_all_rotated_placed_pieces(&self, piece_id: PieceIndex, board_location: &BoardLocation) -> HashSet<PlacedPiece> {
        let mut result = HashSet::new();
        let mut rotated_piece = self.clone();
        for _ in 0..4 {
            let next_piece = rotated_piece.create_rotated_piece();
            let location = (*board_location).clone();
            if !result.insert(PlacedPiece { location: location, piece_id: piece_id, piece_data: rotated_piece }) {
                return result;
            }
            rotated_piece = next_piece;
        }
        result
    }

    fn make_2_shape_piece(first_shape: Shape, second_shape: Shape) -> PieceData {
        let data = vec![Some(first_shape), Some(second_shape)];
        PieceData::new(1, 2, data)
    }

    fn make_3_shape_piece(first_shape: Shape, second_shape: Shape, third_shape: Shape) -> PieceData {
        let data = vec![Some(first_shape), Some(second_shape), Some(third_shape), None];
        PieceData { height: 2, width: 2, data }
    }

    fn get_default_pieces() -> HashMap<PieceIndex, PieceData> {
        let mut result = HashMap::new();
        result.insert('A', PieceData::make_2_shape_piece(Circle, Plus));
        result.insert('B', PieceData::make_2_shape_piece(Square, Plus));
        result.insert('C', PieceData::make_2_shape_piece(Square, Circle));
        result.insert('D', PieceData::make_2_shape_piece(Plus, Plus));
        result.insert('E', PieceData::make_2_shape_piece(Square, Square));
        result.insert('F', PieceData::make_2_shape_piece(Circle, Circle));
        result.insert('G', PieceData::make_3_shape_piece(Square, Circle, Square));
        result.insert('H', PieceData::make_3_shape_piece(Plus, Circle, Square));
        result.insert('I', PieceData::make_3_shape_piece(Circle, Square, Plus));
        result.insert('J', PieceData::make_3_shape_piece(Plus, Plus, Square));
        result.insert('K', PieceData::make_3_shape_piece(Circle, Circle, Square));
        result.insert('L', PieceData::make_3_shape_piece(Circle, Square, Circle));
        result.insert('M', PieceData::make_3_shape_piece(Plus, Square, Circle));
        result.insert('N', PieceData::make_3_shape_piece(Square, Plus, Circle));

        result
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, PartialOrd, Ord, Copy)]
struct BoardLocation {
    row: u32,
    column: u32,
}

impl Display for BoardLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.row, self.column)
    }
}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
pub struct PlacedPiece {
    location: BoardLocation,
    piece_id: PieceIndex,
    piece_data: PieceData,
}

impl Debug for PlacedPiece {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}


impl Display for PlacedPiece {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} [", self.piece_id)?;
        for (location, shape) in self.get_filled_squares().iter().sorted() {
            write!(f, "{}{}, ", shape, location)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl PlacedPiece {
    fn get_filled_squares(&self) -> BTreeSet<(BoardLocation, Shape)> {
        let mut result = BTreeSet::new();
        for (row_offset, column_offset, shape) in self.piece_data.get_non_empty_shapes_and_coords() {
            _ = result.insert(
                (BoardLocation { row: self.location.row + row_offset, column: self.location.column + column_offset }, shape)
            );
        }
        result
    }

    fn to_debug_string(&self) -> String {
        let mut result = String::new();
        _ = write!(&mut result, "{}", self.piece_id);
        for (location, _) in self.get_filled_squares().iter() {
            _ = write!(&mut result, ", {}", location);
        }
        result.into()
    }
}


struct Board {
    height: u32,
    width: u32,
    data: Vec<Shape>,
}

impl Board {
    fn new_default_board() -> Board {
        let data = vec![
            Square, Plus, Circle, Circle,
            Circle, Square, Square, Plus,
            Plus, Plus, Circle, Plus,
            Plus, Square, Circle, Square,
            Plus, Square, Plus, Plus,
            Circle, Square, Circle, Circle,
            Circle, Circle, Square, Square
        ];
        Board { height: 7, width: 4, data: data }
    }

    fn get_shape(&self, location: &BoardLocation) -> &Shape {
        &self.data[(location.row * self.width + location.column) as usize]
    }

    fn is_valid_position(&self, placed_piece: &PlacedPiece) -> bool {
        let location = &placed_piece.location;
        if location.row + placed_piece.piece_data.height > self.height ||
            location.column + placed_piece.piece_data.width > self.width {
            return false;
        }

        for (row_offset, column_offset, shape) in placed_piece.piece_data.get_non_empty_shapes_and_coords() {
            if *self.get_shape(&BoardLocation { row: location.row + row_offset, column: location.column + column_offset }) != shape {
                return false;
            }
        }
        true
    }

    fn get_all_valid_placed_pieces(&self, piece_id: &PieceIndex, piece: &PieceData) -> BTreeSet<PlacedPiece> {
        let mut result = BTreeSet::new();
        for row in 0..self.height {
            for column in 0..self.width {
                let all_placed_pieces_at_location = piece.get_all_rotated_placed_pieces(
                    piece_id.clone(), &BoardLocation { row, column },
                );
                for placed_piece in all_placed_pieces_at_location {
                    if self.is_valid_position(&placed_piece) {
                        result.insert(placed_piece);
                    }
                }
            }
        }
        result
    }
}

pub struct CiladaGame {
    board: Board,
    piece_setup: HashMap<PieceIndex, PieceSetup>,
}

impl CiladaGame {
    pub fn new_game_from_variant_number(variant_number: u32) -> Self {
        let default_board = Board::new_default_board();
        let piece_templates: HashMap<PieceIndex, PieceData> = PieceData::get_default_pieces();
        let maybe_piece_set_representation = ALL_GAME_COMBINATIONS.get((variant_number - 1) as usize);
        match maybe_piece_set_representation {
            Some(r) => { CiladaGame { board: default_board, piece_setup: CiladaGame::get_piece_setup_map(r, piece_templates) } }
            None => panic!("No variant {}. Must be an int between 1 and {}", variant_number, ALL_GAME_COMBINATIONS.len())
        }
    }

    fn find_valid_position_sequences<'a>(
        location_to_placed_pieces_map: HashMap<BoardLocation, BTreeSet<&'a PlacedPiece>>,
        positioned_pieces: Vec<&'a PlacedPiece>,
        occupied_squares: Vec<&BoardLocation>,
        piece_setup: &HashMap<PieceIndex, PieceSetup>,
    ) -> Vec<Vec<&'a PlacedPiece>> {
        if location_to_placed_pieces_map.is_empty() {
            let final_result = vec![positioned_pieces];
            info!("Found a valid path: {:?}", &final_result);
            return final_result;
        }

        for (location, placed_pieces) in location_to_placed_pieces_map.iter().sorted_by_key(|&(_loc, hash_set)| hash_set.len()) {
            debug!("Location: {} - number of placed pieces: {}", location, placed_pieces.len())
        }

        // one assumption of this function is that if placed piece P is in remaining_placed_pieces, then all the locations of P are in remaining_placed_pieces each of their
        // corresponding values contain P
        let (location, next_placed_pieces) = location_to_placed_pieces_map.iter().min_by_key(|&(_loc, pieces)| { pieces.len() }).unwrap();
        if location_to_placed_pieces_map.len() == 0 {
            debug!("... No option for location {}. No solution given the start {:?}", location, positioned_pieces);
            return vec![vec![]];
        }
        let mut result = Vec::new();

        info!("The location with the smallest number of overlapping pieces is {} with {} pieces", location, next_placed_pieces.len());

        'piece_loop: for next_placed_piece in next_placed_pieces {
            info!("Adding {} to the list of positioned pieces", next_placed_piece.to_debug_string());
            let piece_squares = next_placed_piece.get_filled_squares();
            // Those are the pieces that were placed in a way that overlapped with next_placed_pieces
            // and are no longer valid options.
            let mut curr_location_to_placed_pieces_map = location_to_placed_pieces_map.clone();
            let mut curr_positioned_pieces = positioned_pieces.clone();
            let mut curr_occupied_squares = occupied_squares.clone();
            for (location, _) in piece_squares.iter() {
                if occupied_squares.contains(&location) {
                    // if any of the squares which comprise `next_placed_piece` overlaps with currently
                    // occupied squares, then we cannot place it and go to the next one
                    continue 'piece_loop;
                }
                curr_occupied_squares.push(location);
                let invalid_placed_pieces = curr_location_to_placed_pieces_map.remove(location).unwrap();
                info!("Removing {} pieces: {:?}", invalid_placed_pieces.len(), invalid_placed_pieces);
                for invalid_placed_piece in invalid_placed_pieces {
                    for (location_to_update, _) in invalid_placed_piece.get_filled_squares().iter() {
                        let pieces_at_loc = curr_location_to_placed_pieces_map.get_mut(location_to_update);
                        match pieces_at_loc {
                            None => continue,
                            Some(set_of_pieces) => {
                                trace!("Removing {}", invalid_placed_piece);
                                _ = set_of_pieces.remove(invalid_placed_piece);
                            }
                        }
                    }
                }
            }
            curr_positioned_pieces.push(next_placed_piece);
            debug!("Placing {} on the board ({})", next_placed_piece, next_placed_piece.to_debug_string());
            debug!("Currently placed pieces: ");
            for _p in curr_positioned_pieces.iter() {
                debug!("\t{}", _p.to_debug_string())
            }
            let number_of_occurrences_of_piece = curr_positioned_pieces.iter().filter(|&x| { x.piece_id == next_placed_piece.piece_id }).count() as u32;
            let piece_count = piece_setup[&next_placed_piece.piece_id].count;
            if number_of_occurrences_of_piece == piece_count {
                // purge the occurrences of next_placed_piece.piece_id from the curr_location_to_placed_pieces_map passed to the recursive call
                let mut removed = 0u32;
                for placed_pieces in curr_location_to_placed_pieces_map.values_mut() {
                    let original_placed_pieces = placed_pieces.len();
                    placed_pieces.retain(|&placed_piece| { placed_piece.piece_id != next_placed_piece.piece_id });
                    removed += (original_placed_pieces - placed_pieces.len()) as u32;
                }
                info!("Piece {} supply is exhausted ({}). Removing {} candidates for other locations", next_placed_piece.piece_id, piece_count, removed);
            }
            result.extend(CiladaGame::find_valid_position_sequences(curr_location_to_placed_pieces_map, curr_positioned_pieces, curr_occupied_squares, piece_setup));
        }
        result
    }

    fn get_piece_setup_map(piece_representation: &str, piece_templates: HashMap<PieceIndex, PieceData>) -> HashMap<PieceIndex, PieceSetup> {
        let piece_counts = CiladaGame::get_piece_counts_from_string_representation(piece_representation);
        let mut piece_setup_map = HashMap::new();
        for (&piece_id, &count) in piece_counts.iter() {
            _ = piece_setup_map.insert(piece_id, PieceSetup { data: piece_templates[&piece_id].clone(), count })
        }
        piece_setup_map
    }

    fn get_piece_counts_from_string_representation(pieces: &str) -> HashMap<PieceIndex, u32> {
        let mut result = HashMap::new();
        for c in pieces.chars() {
            *result.entry(c).or_insert(0) += 1
        }
        result
    }

    pub fn solve(&self) -> HashSet<PieceCollection> {
        // let pieces_count: HashMap<_, _> = self.piece_counts.iter().map(|(piece_template, &count)| { (piece_template.id, count) }).collect();
        let mut valid_positions_per_piece = HashMap::new();

        for (piece_id, piece_setup) in self.piece_setup.iter() {
            let all_valid_placed_pieces = self.board.get_all_valid_placed_pieces(piece_id, &piece_setup.data);
            let piece_count = self.piece_setup[piece_id].count;
            if all_valid_placed_pieces.len() < (piece_count as usize) {
                panic!("There are more pieces than locations to place them. This should never happen")
            }
            valid_positions_per_piece.insert(piece_id, all_valid_placed_pieces);
        }

        let mut location_to_placed_pieces_map = HashMap::new();

        for all_valid_placed_pieces in valid_positions_per_piece.values() {
            for placed_piece in all_valid_placed_pieces {
                for (location, _) in placed_piece.get_filled_squares().iter() {
                    if !location_to_placed_pieces_map.contains_key(location) {
                        location_to_placed_pieces_map.insert(location.clone(), BTreeSet::new());
                    }
                    location_to_placed_pieces_map.get_mut(location).unwrap().insert(placed_piece);
                }
            }
        }

        let all_paths = CiladaGame::find_valid_position_sequences(location_to_placed_pieces_map, vec![], vec![], &self.piece_setup);

        let result: HashSet<PieceCollection> = all_paths.iter().map(|x| { PieceCollection::from(x.clone()) }).collect();
        result
    }


    pub fn validate_solution(&self, solution: &PieceCollection) -> ValidationStatus {
        let mut occupied_locations: HashSet<BoardLocation> = HashSet::new();
        for piece in solution.iter() {
            for (location, shape) in piece.get_filled_squares() {
                if !occupied_locations.insert(location) {
                    return ValidationStatus::OverlapError(format!("The location {} is covered twice", location));
                }
                let board_shape = self.board.get_shape(&location);
                if board_shape != &shape {
                    return ValidationStatus::ShapeMismatch(
                        format!("The shape at {} ({}) does not match the shape on the board ({})", location, shape, board_shape)
                    );
                }
            }
        }

        let squares_occupied = occupied_locations.len();
        let squares_on_the_board = (self.board.width * self.board.height) as usize;
        if squares_occupied != squares_on_the_board {
            return ValidationStatus::IncompleteSolution(
                format!("Only {} squares are covered when {} were expected to", squares_occupied, squares_on_the_board)
            );
        }
        ValidationStatus::Success
    }
}


#[cfg(test)]
mod tests {
    use crate::cilada::Shape::{Circle, Square};
    use crate::cilada::{Board, BoardLocation, CiladaGame, PieceData, PlacedPiece};
    use std::hash::Hash;

    #[test]
    fn test_get_non_empty_shapes_and_coords() {
        let piece_2 = PieceData::new(2, 2, [
            None, None, Some(Circle), None
        ].into());
        let non_empty_shapes = piece_2.get_non_empty_shapes_and_coords();
        assert_eq!(non_empty_shapes, [(1, 0, Circle)])
    }

    #[test]
    fn test_get_pieces_from_string_representation() {
        let hash_map = CiladaGame::get_piece_counts_from_string_representation("AAAABBCC".into());
        assert_eq!(hash_map.len(), 3);
        assert_eq!(hash_map[&'A'], 4);
        assert_eq!(hash_map[&'B'], 2);
        assert_eq!(hash_map[&'C'], 2);
    }

    #[test]
    fn test_get_all_rotated_placed_pieces() {
        let piece_1 = PieceData::new(1, 2, [
            Some(Square), Some(Circle)
        ].into());
        let all_rotations_piece_1 = piece_1.get_all_rotated_placed_pieces('A', &BoardLocation { row: 1, column: 1 });
        assert_eq!(all_rotations_piece_1.len(), 4);
        let piece_2 = PieceData::new(1, 2, [
            Some(Square), Some(Square)
        ].into());

        let all_rotations_piece_2 = piece_2.get_all_rotated_placed_pieces('A', &BoardLocation { row: 1, column: 1 });
        assert_eq!(all_rotations_piece_2.len(), 2);
    }

    #[test]
    fn test_board_operations() {
        let board_data = vec![
            Circle, Square,
            Circle, Square,
            Circle, Square,
            Circle, Circle
        ];
        let board = Board { height: 4, width: 2, data: board_data };

        assert_eq!(board.get_shape(&BoardLocation { row: 2, column: 1 }), &Square);
        assert_eq!(board.get_shape(&BoardLocation { row: 3, column: 0 }), &Circle);

        let piece_1 = PieceData::new(
            1, 2, vec![Some(Circle), Some(Square)],
        );
        // matches the first line of the board
        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 0, column: 0 }, piece_id: 'A', piece_data: piece_1.clone() }), true);

        // Does not fit: overflow to the right
        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 0, column: 1 }, piece_data: piece_1.clone(), piece_id: 'A' }), false);

        let piece_2 = PieceData::new(
            2, 2, vec![Some(Circle), None, Some(Circle), Some(Square)],
        );

        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 0, column: 0 }, piece_id: 'B', piece_data: piece_2.clone() }), true);
        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 1, column: 0 }, piece_id: 'B', piece_data: piece_2.clone() }), true);
        // does not fit: mismatch
        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 2, column: 0 }, piece_id: 'B', piece_data: piece_2.clone() }), false);
        // does not fit: overflow at the bottom
        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 3, column: 0 }, piece_id: 'B', piece_data: piece_2.clone() }), false);
        // does not fit: overflow to the right
        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 0, column: 1 }, piece_id: 'B', piece_data: piece_2.clone() }), false);

        let piece_3 = (&piece_2).create_rotated_piece();
        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 2, column: 0 }, piece_id: 'B', piece_data: piece_3.clone() }), true);
        assert_eq!(board.is_valid_position(&PlacedPiece { location: BoardLocation { row: 1, column: 0 }, piece_id: 'B', piece_data: piece_3.clone() }), false);
    }
}
