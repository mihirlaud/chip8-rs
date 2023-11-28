use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::{error::Error, io::{self, Read}, fs::File, time::Duration};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::Rect,
    widgets::{Block, Borders, canvas::{Canvas, Rectangle}},
    Frame, Terminal, style::Color
};
use rand::random;

const SCREEN_WIDTH: u16 = 64;
const SCREEN_HEIGHT: u16 = 32;

const FONT_SIZE: usize = 5 * 16;
const FONT: [u8; FONT_SIZE] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80  // F
];

struct Chip8 {
    ram: [u8; 4096],
    registers: [u8; 16],
    i_reg: u16,
    dt: u8,
    st: u8,
    pc: u16,
    sp: u8,
    stack: [u16; 16],
    screen: [bool; (SCREEN_WIDTH * SCREEN_HEIGHT) as usize],
    keyboard: [bool; 16],
}

impl Chip8 {
    pub fn new() -> Self {
        let mut new_chip8 = Self {
            ram: [0; 4096],
            registers: [0; 16],
            i_reg: 0,
            dt: 0,
            st: 0,
            pc: 0x200,
            sp: 0,
            stack: [0; 16],
            screen: [false; (SCREEN_WIDTH * SCREEN_HEIGHT) as usize],
            keyboard: [false; 16],
        };

        new_chip8.ram[0 .. FONT_SIZE].copy_from_slice(&FONT);

        new_chip8

    }

    fn push_stack(&mut self, val: u16) {
        if self.sp < 16 {
            self.stack[self.sp as usize] = val;
            self.sp += 1;
        }
    }

    fn pop_stack(&mut self) -> u16 {
        if self.sp > 0 {
            self.sp -= 1;
            let ret = self.stack[self.sp as usize];
            self.stack[self.sp as usize] = 0;
        
            ret
        } else {
            0
        }
    }

    fn reset(&mut self) {
        self.ram = [0; 4096];
        self.registers = [0; 16];
        self.i_reg = 0;
        self.dt = 0;
        self.st = 0;
        self.pc = 0x200;
        self.sp = 0;
        self.stack = [0; 16];
        self.screen = [false; (SCREEN_WIDTH * SCREEN_HEIGHT) as usize];
        self.keyboard = [false; 16];

        self.ram[0 .. FONT_SIZE].copy_from_slice(&FONT);
    }

    fn cycle(&mut self) {
        let op = self.fetch();
        self.execute(op);
    }

    fn advance_timers(&mut self) {
        if self.dt > 0 {
            self.dt -= 1;
        }

        if self.st > 0 {
            // buzz

            self.st -= 1;
        }
    }

    fn fetch(&mut self) -> u16 {
        let ret = ((self.ram[self.pc as usize] as u16) << 8) | (self.ram[self.pc as usize + 1] as u16);
        
        self.pc += 2;
        
        ret
    }

    fn keypress(&mut self, idx: usize, pressed: bool) {
        self.keyboard[idx] = pressed;
    }

    pub fn load_data(&mut self, data: &[u8]) {
        self.ram[0x200 .. 0x200 + data.len()].copy_from_slice(data);
    }

    fn execute(&mut self, op: u16) {
        let pos1 = (op & 0xF000) >> 12;
        let pos2 = (op & 0x0F00) >> 8;
        let pos3 = (op & 0x00F0) >> 4;
        let pos4 = op & 0x000F;

        match (pos1, pos2, pos3, pos4) {
            (0x0, 0x0, 0xE, 0x0) => {
                // Clear screen
                self.screen = [false; (SCREEN_WIDTH * SCREEN_HEIGHT) as usize];
            }
            (0x0, 0x0, 0xE, 0xE) => {
                // Return to previous address on stack
                self.pc = self.pop_stack();
            }
            (0x1, _, _, _) => {
                // Jump to address
                self.pc = op & 0xFFF;
            }
            (0x2, _, _, _) => {
                // Execute subroutine at address
                self.push_stack(self.pc);
                self.pc = op & 0xFFF;
            }
            (0x3, x, k1, k2) => {
                // Skip next instruction if Vx == kk
                if self.registers[x as usize] == ((k1 << 4) | k2) as u8 {
                    self.pc += 2;
                }
            }
            (0x4, x, k1, k2) => {
                // Skip next instruction if Vx != kk
                if self.registers[x as usize] != ((k1 << 4) | k2) as u8 {
                    self.pc += 2;
                }
            }
            (0x5, x, y, 0x0) => {
                // Skip next instruction if Vx == Vy
                if self.registers[x as usize] == self.registers[y as usize] {
                    self.pc += 2;
                }
            }
            (0x6, x, k1, k2) => {
                // Load kk into Vx
                self.registers[x as usize] = ((k1 << 4) | k2) as u8;
            }
            (0x7, x, k1, k2) => {
                // Add kk to Vx
                self.registers[x as usize] = self.registers[x as usize].wrapping_add(((k1 << 4) | k2) as u8);
            }
            (0x8, x, y, 0x0) => {
                // Load Vy into Vx
                self.registers[x as usize] = self.registers[y as usize];
            }
            (0x8, x, y, 0x1) => {
                // Set Vx as Vx | Vy
                self.registers[x as usize] |= self.registers[y as usize];
            }
            (0x8, x, y, 0x2) => {
                // Set Vx as Vx & Vy
                self.registers[x as usize] &= self.registers[y as usize];
            }
            (0x8, x, y, 0x3) => {
                // Set Vx as Vx ^ Vy
                self.registers[x as usize] ^= self.registers[y as usize];
            }
            (0x8, x, y, 0x4) => {
                // Set Vx as Vx + Vy, set VF to carry
                let (sum, carry) = self.registers[x as usize].overflowing_add(self.registers[y as usize]);
                self.registers[x as usize] = sum;
                self.registers[0xF] = if carry { 1 } else { 0 };
            }
            (0x8, x, y, 0x5) => {
                // Set Vx as Vx - Vy, set VF to NOT borrow
                let (sub, borrow) = self.registers[x as usize].overflowing_sub(self.registers[y as usize]);
                self.registers[x as usize] = sub;
                self.registers[0xF] = if borrow { 0 } else { 1 };
            }
            (0x8, x, _, 0x6) => {
                // Right shift Vx, set VF to least significant bit of Vx prior to shift
                self.registers[0xF] = self.registers[x as usize] & 0x1;
                self.registers[x as usize] >>= 1;
            }
            (0x8, x, y, 0x7) => {
                // Set Vx as Vy - Vx, set VF to NOT borrow
                let (sub, borrow) = self.registers[y as usize].overflowing_sub(self.registers[x as usize]);
                self.registers[x as usize] = sub;
                self.registers[0xF] = if borrow { 0 } else { 1 };
            }
            (0x8, x, _, 0xE) => {
                // Left shift Vx, set VF to most significant bit of Vx prior to shift
                self.registers[0xF] = (self.registers[x as usize] >> 7) & 0x1;
                self.registers[x as usize] <<= 1;
            }
            (0x9, x, y, 0x0) => {
                // Skip next instruction if Vx != Vy
                if self.registers[x as usize] != self.registers[y as usize] {
                    self.pc += 2;
                }
            }
            (0xA, _, _, _) => {
                // Set I to address
                self.i_reg = op & 0xFFF;
            }
            (0xB, _, _, _) => {
                // Jump to address + V0
                self.pc = (op & 0xFFF) + self.registers[0x0] as u16;
            }
            (0xC, x, k1, k2) => {
                // Set Vx to random byte & kk
                let random_byte: u8 = random();
                self.registers[x as usize] = (((k1 << 4) & k2) as u8) & random_byte;
            }
            (0xD, x, y, n) => {
                let x = self.registers[x as usize];
                let y = self.registers[y as usize];

                let mut flip = false;

                for dy in 0 .. n as u8 {
                    let pixels = self.ram[(self.i_reg + dy as u16) as usize];
                    for dx in 0 .. 8 {
                        if (pixels & (0b10000000 >> dx)) != 0 {
                            let x1 = (x + dx) as u16 % SCREEN_WIDTH;
                            let y1 = (y + dy) as u16 % SCREEN_HEIGHT;

                            let idx = x1 as u16 + SCREEN_WIDTH * (y1 as u16);

                            flip |= self.screen[idx as usize];
                            self.screen[idx as usize] ^= true;
                        }
                    }
                }

                self.registers[0xF] = if flip { 1 } else { 0 };
            }
            (0xE, x, 0x9, 0xE) => {
                // Skip next instruction if key with value of Vx is pressed
                if self.keyboard[self.registers[x as usize] as usize] {
                    self.pc += 2;
                }
            }
            (0xE, x, 0xA, 0x1) => {
                // Skip next instruction if key with value of Vx is not pressed
                if !self.keyboard[self.registers[x as usize] as usize] {
                    self.pc += 2;
                }
            }
            (0xF, x, 0x0, 0x7) => {
                // Load the delay timer value into Vx
                self.registers[x as usize] = self.dt;
            }
            (0xF, x, 0x0, 0xA) => {
                // Block execution until key is pressed, then record its value in Vx
                let mut pressed = false;
                for i in 0 .. self.keyboard.len() {
                    if self.keyboard[i] {
                        pressed = true;
                        self.registers[x as usize] = i as u8;
                    }
                }

                // If no key is pressed, move the PC backwards to redo the opcode
                if !pressed {
                    self.pc -= 2;
                }
            }
            (0xF, x, 0x1, 0x5) => {
                // Load Vx into delay timer
                self.dt = self.registers[x as usize];
            }
            (0xF, x, 0x1, 0x8) => {
                // Load Vx into sound timer
                self.st = self.registers[x as usize];
            }
            (0xF, x, 0x1, 0xE) => {
                // Set I to I + Vx
                self.i_reg = self.i_reg.wrapping_add(self.registers[x as usize] as u16);
            }
            (0xF, x, 0x2, 0x9) => {
                // Set I to location of sprite for Vx
                self.i_reg = (self.registers[x as usize]as u16) * 5;
            }
            (0xF, x, 0x3, 0x3) => {
                let num = self.registers[x as usize] as f32;

                let hundreds = (num / 100.0).floor() as u8;
                let tens = ((num % 100.0) / 10.0).floor() as u8;
                let ones = (num % 10.0) as u8;

                self.ram[self.i_reg as usize] = hundreds;
                self.ram[(self.i_reg + 1) as usize] = tens;
                self.ram[(self.i_reg + 2) as usize] = ones;
            }
            (0xF, x, 0x5, 0x5) => {
                // Store registers V0 to Vx in memory starting at I
                for i in 0 .. x {
                    self.ram[(self.i_reg + i) as usize] = self.registers[x as usize];
                }
            }
            (0xF, x, 0x6, 0x5) => {
                // Read registers V0 to Vx from memory starting at I
                for i in 0 .. x {
                    self.registers[x as usize] = self.ram[(self.i_reg + i) as usize];
                }
            }
            _ => {}
        }
    }

    pub fn run<B: Backend>(&mut self, terminal: &mut Terminal<B>) -> io::Result<()> {
    
        let args: Vec<String> = std::env::args().collect();
        if args.len() < 2 {
            println!("Must specify rom");
            return Ok(());
        } else {
            println!("Args: {:?}", args);
        }
    
        let mut rom = File::open(format!("roms/{}", args[1])).expect("Unable to open file");
        let mut buffer = vec![];
    
        rom.read_to_end(&mut buffer).unwrap();
        self.load_data(&buffer);
    
        loop {
            terminal.draw(|f| self.ui(f))?;
    
            if event::poll(Duration::from_millis(16))? {
                if let Event::Key(key) = event::read()? {
                    if key.code == KeyCode::Enter {
                        self.reset();
                        let args: Vec<String> = std::env::args().collect();
                        let mut rom = File::open(format!("roms/{}", args[1])).expect("Unable to open file");
                        let mut buffer = vec![];
    
                        rom.read_to_end(&mut buffer).unwrap();
                        self.load_data(&buffer);
                        continue;
                    }
    
                    if key.code == KeyCode::Esc {
                        return Ok(());
                    }
    
                    let idx = match key.code {
                        KeyCode::Char(c) => {
                            match c {
                                'x' => Some(0x0),
                                '1' => Some(0x1),
                                '2' => Some(0x2),
                                '3' => Some(0x3),
                                'q' => Some(0x4),
                                'w' => Some(0x5),
                                'e' => Some(0x6),
                                'a' => Some(0x7),
                                's' => Some(0x8),
                                'd' => Some(0x9),
                                'z' => Some(0xA),
                                'c' => Some(0xB),
                                '4' => Some(0xC),
                                'r' => Some(0xD),
                                'f' => Some(0xE),
                                'v' => Some(0xF),
                                _ => None,
                            }
                        }
                        _ => None,
                    };
    
                    match key.kind {
                        KeyEventKind::Press => {
                            match idx {
                                Some(i) => self.keypress(i, true),
                                None => {}
                            }
                        }
                        KeyEventKind::Release => {
                            match idx {
                                Some(i) => self.keypress(i, false),
                                None => {}
                            }
                        }
                        _ => {}
                    }
                }
            }
    
            for _ in 0 .. 10 {
                self.cycle();
            }
            self.advance_timers();
        }
    }

    fn ui<B: Backend>(&self, f: &mut Frame<B>) {
        let canvas_area = Rect::new(0, 0, SCREEN_WIDTH + 2, SCREEN_HEIGHT + 2);
        let block = Block::default()
            .title("CHIP 8")
            .borders(Borders::ALL);
    
        let canvas = Canvas::default().block(block)
            .x_bounds([-1.0, SCREEN_WIDTH as f64 + 1.0])
            .y_bounds([-1.0, SCREEN_HEIGHT as f64 + 1.0])
            .marker(tui::symbols::Marker::Block)
            .paint(|ctx| {
                for idx in 0 .. self.screen.len() {
                    let y = idx / (SCREEN_WIDTH as usize);
                    let x = idx % (SCREEN_WIDTH as usize);
            
                    ctx.draw(&Rectangle {
                        x: x as f64,
                        y: SCREEN_HEIGHT as f64 - 1.0 - y as f64,
                        width: 1.0,
                        height: 1.0,
                        color: if self.screen[idx] { Color::White } else { Color::Black },
                    });
                }
            });
    
        f.render_widget(canvas, canvas_area);
    
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // create app and run it
    let mut cpu = Chip8::new();
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Must specify rom");
        return Ok(());
    }

    let mut rom = File::open(format!("roms/{}", args[1])).expect("Unable to open file");
    let mut buffer = vec![];

    rom.read_to_end(&mut buffer).unwrap();
    cpu.load_data(&buffer);

    let res = cpu.run(&mut terminal);

    // restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{:?}", err)
    }

    Ok(())
}