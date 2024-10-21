use clap::{Parser, ValueEnum};
use env_logger::{Builder, Target};

mod cilada;


/// Define a struct with the command-line options using `clap`'s derive macros
#[derive(Parser, Debug)]
#[command(
    name = "cilada-rs",
    about = "Program finding all the solutions for the Cilada puzzle game"
)]
struct Cli {
    #[arg(short, action = clap::ArgAction::Count)]
    verbose: u8,

    #[arg(short, long, default_value = "validate")]
    mode: Mode,

    #[arg(long, default_value = None)]
    variant: Option<u32>,
}

/// Define an enum for the command-line option with restricted values
#[derive(Copy, Clone, Debug, ValueEnum)]
enum Mode {
    Count,
    Explicit,
    Validate,
}


fn main() {
    let args = Cli::parse();
    let log_level = match args.verbose {
        0 => log::LevelFilter::Warn,
        1 => log::LevelFilter::Info,
        _ => log::LevelFilter::Debug,
    };

    Builder::new()
        .filter(None, log_level) // Set the minimum log level to `debug`
        .target(Target::Stdout)                // Log to stdout
        .init();

    let variants = match args.variant {
        None => 1..=50,
        Some(i) => i..=i
    };

    for variant in variants {
        let cilada_game = cilada::CiladaGame::new_game_from_variant_number(variant);
        let solutions = cilada_game.solve();

        match args.mode {
            Mode::Count => {
                println!("Number of solutions for variant {}: {}", variant, solutions.len());
            }
            Mode::Explicit => {
                println!("Solutions for variant {} ", variant);
                for solution in &solutions {
                    println!("{:?}", solution);
                }
                println!("\n\n")
            }
            Mode::Validate => {
                println!("Validating the solutions for variant {}:", variant);
                for (solution_idx, solution) in solutions.iter().enumerate() {
                    match cilada_game.validate_solution(&solution) {
                        cilada::ValidationStatus::Success => { println!("... solution {} is valid", solution_idx + 1) }
                        cilada::ValidationStatus::OverlapError(msg) => {
                            println!("One of the solution failed because of overlapping pieces: {}", msg)
                        }
                        cilada::ValidationStatus::ShapeMismatch(msg) => {
                            println!("One of the solution failed because of a mismatch in shapes: {}", msg)
                        }
                        cilada::ValidationStatus::IncompleteSolution(msg) => {
                            println!("One of the solution failed because of overlapping pieces: {}", msg)
                        }
                    }
                }
            }
        }
    }
}
