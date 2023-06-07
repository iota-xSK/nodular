use raylib::prelude::*;
use rfd::FileDialog;
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::f32::consts;
use std::ffi::{CStr, CString};
use std::fmt::Display;
use std::fs::{self, File};
use std::io::prelude::*;
const WIDTH: i32 = 600;
const HEIGHT: i32 = 800;

use std::ops;
#[derive(Clone, Copy, Serialize, Deserialize, Debug)]
struct Vec2 {
    x: f32,
    y: f32,
}
impl Into<Vector2> for Vec2 {
    fn into(self) -> Vector2 {
        Vector2 {
            x: self.x,
            y: self.y,
        }
    }
}

impl From<Vector2> for Vec2 {
    fn from(value: Vector2) -> Self {
        Self {
            x: value.x,
            y: value.y,
        }
    }
}

impl Vec2 {
    fn new(x: f32, y: f32) -> Vec2 {
        Vec2 { x, y }
    }

    fn zero() -> Vec2 {
        Self { x: 0.0, y: 0.0 }
    }

    fn length(&self) -> f32 {
        (self.x * self.x + self.y * self.y).sqrt()
    }
}

impl ops::Add<Vec2> for Vec2 {
    type Output = Vec2;

    fn add(self, rhs: Vec2) -> Vec2 {
        Vec2::new(self.x + rhs.x, self.y + rhs.y)
    }
}

impl ops::AddAssign<Vec2> for Vec2 {
    fn add_assign(&mut self, rhs: Vec2) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl ops::SubAssign<Vec2> for Vec2 {
    fn sub_assign(&mut self, rhs: Vec2) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}

impl ops::MulAssign<f32> for Vec2 {
    fn mul_assign(&mut self, rhs: f32) {
        self.x *= rhs;
        self.y *= rhs;
    }
}

impl ops::DivAssign<f32> for Vec2 {
    fn div_assign(&mut self, rhs: f32) {
        self.x /= rhs;
        self.y /= rhs;
    }
}

impl ops::Sub<Vec2> for Vec2 {
    type Output = Vec2;

    fn sub(self, rhs: Vec2) -> Vec2 {
        Vec2::new(self.x - rhs.x, self.y - rhs.y)
    }
}

impl ops::Mul<f32> for Vec2 {
    type Output = Vec2;

    fn mul(self, rhs: f32) -> Vec2 {
        Vec2::new(self.x * rhs, self.y * rhs)
    }
}

impl ops::Div<f32> for Vec2 {
    type Output = Vec2;

    fn div(self, rhs: f32) -> Vec2 {
        Vec2::new(self.x / rhs, self.y / rhs)
    }
}

impl Vec2 {
    fn normalized(&self) -> Vec2 {
        let length = (self.x * self.x + self.y * self.y).sqrt();
        Vec2::new(self.x / length, self.y / length)
    }
}

fn main() {
    let wireworld = Ruleset::new(vec![
        Rules::new(vec![Rule {
            // head
            pattern: Pattern::Wildcard,
            replacement: State(1),
        }]),
        Rules::new(vec![Rule {
            // tail
            pattern: Pattern::Wildcard,
            replacement: State(2),
        }]),
        Rules::new(vec![
            Rule {
                // wire
                pattern: Pattern::Or(
                    Box::new(Pattern::Equal {
                        state: State(0),
                        number: 1,
                    }),
                    Box::new(Pattern::Equal {
                        state: State(0),
                        number: 2,
                    }),
                ),
                replacement: State(0),
            },
            Rule {
                pattern: Pattern::Wildcard,
                replacement: State(2),
            },
        ]),
    ]);

    let wireworld = Ruleset::new(vec![
        Rules::new(vec![
            Rule {
                // wire
                pattern: Pattern::Or(
                    Box::new(Pattern::Equal {
                        state: State(0),
                        number: 1,
                    }),
                    Box::new(Pattern::Equal {
                        state: State(0),
                        number: 2,
                    }),
                ),
                replacement: State(0),
            },
            Rule {
                pattern: Pattern::Wildcard,
                replacement: State(1),
            },
        ]),
        Rules::new(vec![
            Rule {
                // wire
                pattern: Pattern::Or(
                    Box::new(Pattern::Equal {
                        state: State(0),
                        number: 1,
                    }),
                    Box::new(Pattern::Equal {
                        state: State(0),
                        number: 2,
                    }),
                ),
                replacement: State(0),
            },
            Rule {
                pattern: Pattern::Wildcard,
                replacement: State(1),
            },
        ]),
    ]);

    // let mut app = App::new(
    //     wireworld,
    //     vec!["head".to_owned(), "tail".to_owned(), "wire".to_owned()],
    // );

    let mut app = App::new(wireworld, vec!["electron".to_owned(), "wire".to_owned()]);

    app.init_positions(640, 480);

    let (mut rl, thread) = raylib::init()
        .size(WIDTH, HEIGHT)
        .resizable()
        .title("Hello, World")
        .build();

    while !rl.window_should_close() {
        app.mainloop(&mut rl, &thread)
    }
}

#[derive(Clone, Debug)]
struct Copied {
    nodes: Vec<State>,
    positions: Vec<Vec2>,
    connections: Vec<Vec<usize>>,
}

impl Copied {
    fn new(app: &App) -> Self {
        let mut nodes = vec![];
        let mut positions = vec![];
        for selected in &app.selected {
            nodes.push(app.graph.nodes_read[*selected]);
            positions.push(app.node_positions[*selected]);
        }
        let mut connections = vec![];

        Self {
            nodes,
            positions,
            connections,
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
struct Ruleset {
    by_state: Vec<Rules>,
}

impl Ruleset {
    fn new(states: Vec<Rules>) -> Self {
        Self { by_state: states }
    }
    fn apply(&self, idx: usize, graph: &Graph) -> State {
        self.by_state[graph.nodes_read[idx].0]
            .apply(idx, &graph)
            .unwrap()
    }
}

#[derive(Clone, Serialize, Deserialize)]
struct Rules {
    rules: Vec<Rule>,
}

impl Rules {
    fn new(rules: Vec<Rule>) -> Self {
        Self { rules }
    }
    fn apply(&self, idx: usize, graph: &Graph) -> Option<State> {
        for rule in self.rules.iter() {
            if rule.pattern.pattern_match(idx, graph) {
                return Some(rule.replacement);
            }
        }
        None
    }
}

#[derive(Clone, Serialize, Deserialize)]
struct Rule {
    pattern: Pattern,
    replacement: State,
}

#[derive(Clone, Serialize, Deserialize)]
enum Pattern {
    Equal { state: State, number: u32 },
    Gth { state: State, number: u32 },
    Lth { state: State, number: u32 },
    Geq { state: State, number: u32 },
    Leq { state: State, number: u32 },
    Or(Box<Pattern>, Box<Pattern>),
    And(Box<Pattern>, Box<Pattern>),
    Not(Box<Pattern>),
    Wildcard,
}

impl Pattern {
    fn pattern_match(&self, node: usize, graph: &Graph) -> bool {
        let nbh = graph.nbh[node].iter().map(|i| graph.nodes_read[*i]);

        match self {
            Pattern::Equal { state, number } => {
                if nbh.filter(|a| a == state).count() as u32 == *number {
                    return true;
                } else {
                    return false;
                }
            }
            Pattern::Gth { state, number } => {
                if nbh.filter(|a| a == state).count() as u32 > *number {
                    return true;
                } else {
                    return false;
                }
            }
            Pattern::Lth { state, number } => {
                if (nbh.filter(|a| a == state).count() as u32) < *number {
                    return true;
                } else {
                    return false;
                }
            }
            Pattern::Geq { state, number } => {
                if nbh.filter(|a| a == state).count() as u32 >= *number {
                    return true;
                } else {
                    return false;
                }
            }
            Pattern::Leq { state, number } => {
                if nbh.filter(|a| a == state).count() as u32 <= *number {
                    return true;
                } else {
                    return false;
                }
            }
            Pattern::Or(left, right) => {
                return left.pattern_match(node, graph) || right.pattern_match(node, graph)
            }
            Pattern::And(left, right) => {
                return left.pattern_match(node, graph) && right.pattern_match(node, graph)
            }
            Pattern::Not(u) => return !u.pattern_match(node, graph),
            Pattern::Wildcard => return true,
        }
    }
}

#[derive(Copy, PartialEq, Clone, Serialize, Deserialize, Debug)]
struct State(usize);

#[derive(Serialize, Deserialize)]
struct Graph {
    ruleset: Ruleset,
    nodes_read: Vec<State>,
    nodes_write: Vec<State>,
    nbh: Vec<Vec<usize>>,
}

impl Display for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            self.nodes_read.iter().map(|a| a.0).collect::<Vec<usize>>()
        )
    }
}

impl Graph {
    fn new(nodes: Vec<State>, nbh: Vec<Vec<usize>>, ruleset: Ruleset) -> Self {
        Self {
            ruleset,
            nodes_read: vec![State(0); nodes.len()],
            nodes_write: nodes,
            nbh,
        }
    }

    fn step(&mut self) {
        std::mem::swap(&mut self.nodes_write, &mut self.nodes_read);

        for i in 0..self.nodes_read.len() {
            self.nodes_write[i] = self.ruleset.apply(i, &self)
        }
    }
    fn add_node(&mut self, state: State) {
        self.nodes_read.push(state);
        self.nodes_write.push(state);

        self.nbh.push(Vec::new());
    }
}

#[derive(Serialize, Deserialize)]
struct App {
    graph: Graph,
    node_positions: Vec<Vec2>,
    screen_offset: Vec2,
    zoom: f32,
    playing: bool,
    state_names: Vec<String>,
    selected_state: i32,
    dragging_from: Option<usize>,
    selected: Vec<usize>,
    type_scroll: i32,
    spring_simulation: bool,
    box_drag_start: Option<Vec2>,
    click_position: Vec2,
    dragging_node_positions: Option<Vec<Vec2>>,
}

impl App {
    fn new(ruleset: Ruleset, state_names: Vec<String>) -> Self {
        Self {
            graph: Graph::new(Vec::new(), Vec::new(), ruleset),
            node_positions: Vec::new(),
            screen_offset: Vec2::zero(),
            zoom: 1.0,
            playing: false,
            state_names,
            selected_state: 0,
            dragging_from: None,
            selected: vec![],
            type_scroll: 0,
            spring_simulation: false,
            box_drag_start: None,
            click_position: Vec2::zero(),
            dragging_node_positions: None,
        }
    }

    fn add_edge(&mut self, u: usize, v: usize) {
        if !self.graph.nbh[u].contains(&v) {
            self.graph.nbh[u].push(v);
        }
    }
    fn remove_edge(&mut self, u: usize, v: usize) {
        self.graph.nbh[u].retain(|a| *a != v);
    }

    fn add_node(&mut self, node: State, position: Vec2) {
        self.graph.add_node(node);
        self.node_positions.push(position);
    }

    fn render(&mut self, thread: &RaylibThread, rl: &mut RaylibHandle) {
        {
            let height = rl.get_screen_height();
            let _width = rl.get_screen_width();
            let mouse_position = rl.get_mouse_position();
            let mut d = rl.begin_drawing(&thread);

            d.clear_background(Color::color_from_hsv(0.5, 0.1, 1.0));

            for selected in &self.selected {
                d.draw_circle_v(
                    {
                        let this =
                            (self.node_positions[*selected] + self.screen_offset) * self.zoom;
                        Vector2 {
                            x: this.x,
                            y: this.y,
                        }
                    },
                    32.0 * self.zoom,
                    Color::BLACK,
                )
            }

            for node in 0..self.node_positions.len() {
                d.draw_circle(
                    (((self.node_positions[node].x) as i32 + self.screen_offset.x as i32) as f32
                        * self.zoom) as i32,
                    (((self.node_positions[node].y) as i32 + self.screen_offset.y as i32) as f32
                        * self.zoom) as i32,
                    30.0 * self.zoom,
                    // colors[self.graph.nodes_read[node].0],
                    Color::color_from_hsv(
                        distribute_hue(self.graph.nodes_write[node].0),
                        0.95,
                        0.95,
                    ),
                );
                for &j in &self.graph.nbh[node] {
                    let pos_j = self.node_positions[j];
                    // let spring = &self.springs[node];

                    Self::draw_spring_arrow(
                        &mut d,
                        (pos_j + self.screen_offset) * self.zoom,
                        (self.node_positions[node] + self.screen_offset) * self.zoom,
                        Color::BLACK,
                        30.0 * self.zoom,
                    );
                }
            }

            if let Some(drag_box) = self.box_drag_start {
                d.draw_rectangle_rec(
                    find_rect(
                        ((drag_box + self.screen_offset) * self.zoom).into(),
                        mouse_position,
                    ),
                    Color::new(10, 10, 255, 10),
                )
            }

            // gui-------------------------------------------------------------------------------
            d.gui_panel(rrect(0, 0, 100, height));

            let mut strings = vec![];

            for name in &self.state_names {
                strings.push(CString::new(name.clone()).unwrap());
            }
            self.selected_state = d.gui_list_view_ex(
                rrect(0, 60, 100, 300.min(height - 60)),
                &strings.iter().map(|a| a.borrow()).collect::<Vec<&CStr>>(),
                &mut 1,
                &mut self.type_scroll,
                // &mut self.selected_state,
                self.selected_state,
            );
            self.playing =
                d.gui_check_box(rrect(10, 10, 10, 10), Some(rstr!("playing")), self.playing);
            d.gui_line(rrect(10, 15, 80, 20), None);
            self.spring_simulation = d.gui_check_box(
                rrect(10, 30, 10, 10),
                Some(rstr!("Spring sim")),
                self.spring_simulation,
            );

            if d.gui_button(rrect(0, 365, 100, 30), Some(rstr!("open world"))) {
                if let Some(file) = FileDialog::new().pick_file() {
                    if let Ok(content) = fs::read_to_string(file) {
                        if let Ok(deserialized) = serde_json::from_str(&content) {
                            *self = deserialized;
                        }
                    } else {
                        println!("unable to read file")
                    }
                } else {
                    println!("unable to pick file")
                }
            }
            if d.gui_button(rrect(0, 400, 100, 30), Some(rstr!("save_world"))) {
                if let Some(file_choice) = FileDialog::new().save_file() {
                    if let Ok(mut file) = File::create(file_choice) {
                        if let Ok(parsed) = serde_json::to_string_pretty(&self) {
                            file.write_all(parsed.as_bytes())
                                .unwrap_or_else(|_| println!("unable to write to file"));
                        } else {
                            println!("unable to parse file")
                        }
                    } else {
                        println!("unable to create file")
                    }
                } else {
                    println!("unable to pick file to create")
                }
            }

            if d.gui_button(rrect(0, 435, 100, 30), Some(rstr!("step"))) {
                self.step();
            }

            //--------------------------------------------------------------------------------------------
        }

        // if rl.get_mouse_x() < 100 && rl.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) {
        //     if let Some(selected_node) = self.selected {
        //         self.graph.nodes_read[selected_node] = State(self.selected_state as usize);
        //     }
        // }
    }

    fn step(&mut self) {
        self.graph.step();
    }

    fn screen_coord_to_world_coor(&self, pos: Vec2) -> Vec2 {
        pos / self.zoom - self.screen_offset.into()
    }

    fn init_positions(&mut self, width: usize, height: usize) {
        let size = self.node_positions.len();
        for (i, position) in self.node_positions.iter_mut().enumerate() {
            *position = Vec2::new(
                (i as f32 / size as f32 * 2.0 * consts::PI).cos() * 100 as f32 + width as f32 / 2.0,
                (i as f32 / size as f32 * 2.0 * consts::PI).sin() * 100 as f32
                    + height as f32 / 2.0,
            )
        }
    }

    fn update_node_positions(&mut self, rl: &mut RaylibHandle) {
        let frame_time = rl.get_frame_time();
        for i in 0..self.graph.nbh.len() {
            let pos_i = self.node_positions[i];

            for &j in &self.graph.nbh[i] {
                let pos_j = self.node_positions[j];

                let delta = pos_j - pos_i;
                let distance = delta.length();
                let displacement = (distance - 100.0) * 0.70;

                let force = delta.normalized() * displacement - (delta * 0.1);

                self.node_positions[i] += force * frame_time;
            }

            // Apply repelling force
            for k in 0..self.graph.nbh.len() {
                if k != i {
                    let pos_k = self.node_positions[k];
                    let repulsion_factor = 50000.5; // Adjust this value to control the repelling force strength

                    let delta = pos_k - pos_i;
                    let distance = delta.length();
                    let repulsion_force =
                        delta.normalized() * (repulsion_factor / distance.powf(2.0));

                    self.node_positions[i] -= repulsion_force * frame_time;
                }
            }
        }
    }
    fn draw_spring_arrow(
        d: &mut RaylibDrawHandle,
        start: Vec2,
        end: Vec2,
        color: Color,
        radius: f32,
    ) {
        // Calculate arrowhead size based on spring length
        let spring_length = (end - start).length();
        let arrowhead_size = spring_length / 20.0;

        // Calculate normalized direction vector of the spring
        let direction = (end - start).normalized();

        // Calculate perpendicular vector to the spring direction
        let perpendicular = Vec2 {
            x: -direction.y,
            y: direction.x,
        };

        let control_point = (start + end) / 2.0 + perpendicular * (end - start).length() * 0.2;

        let arrow_direction = (end - control_point).normalized();
        let arrow_perpendicular = Vec2 {
            x: -arrow_direction.y,
            y: arrow_direction.x,
        };

        // Calculate arrowhead points
        let arrowhead_left =
            end - (arrow_direction * arrowhead_size) + (arrow_perpendicular * arrowhead_size);
        let arrowhead_right =
            end - (arrow_direction * arrowhead_size) - (arrow_perpendicular * arrowhead_size);

        // Draw arrowhead triangle
        d.draw_triangle(
            <Vec2 as Into<Vector2>>::into(arrowhead_left - direction * radius),
            <Vec2 as Into<Vector2>>::into(end - direction * radius),
            <Vec2 as Into<Vector2>>::into(arrowhead_right - direction * radius),
            color,
        );

        d.draw_line_bezier_quad(
            <Vec2 as Into<Vector2>>::into(start + direction * radius),
            <Vec2 as Into<Vector2>>::into(end - direction * radius),
            <Vec2 as Into<Vector2>>::into(control_point),
            1.0,
            color,
        )
    }

    fn mainloop(&mut self, rl: &mut RaylibHandle, thread: &RaylibThread) {
        self.render(&thread, rl);
        if self.spring_simulation {
            self.update_node_positions(rl);
        }
        if rl.is_key_pressed(KeyboardKey::KEY_SPACE) {
            self.playing = !self.playing;
        }
        if (rl.get_time() % 0.5) < rl.get_frame_time() as f64 && self.playing {
            self.step();
        }
        if !rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL)
            && !rl.is_key_down(KeyboardKey::KEY_LEFT_SHIFT)
        {
            self.screen_offset.y -= rl.get_mouse_wheel_move() * 10.0;
        }
        if rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) {
            self.screen_offset.x -= rl.get_mouse_wheel_move() * 10.0;
        }
        if rl.is_key_down(KeyboardKey::KEY_LEFT_SHIFT) {
            self.zoom += rl.get_mouse_wheel_move() * 0.1;
        }

        let mut on_blank = true;
        let mut coliding_with = None;
        for (i, node) in self.node_positions.iter().enumerate() {
            if (self.screen_coord_to_world_coor(rl.get_mouse_position().into()) - *node).length()
                < 30.0
                && rl.get_mouse_position().x > 100.0
            {
                on_blank = false;
                coliding_with = Some(i);
            }
        }
        if rl.is_key_pressed(KeyboardKey::KEY_A) {
            if on_blank {
                self.add_node(
                    State(self.selected_state as usize),
                    self.screen_coord_to_world_coor(rl.get_mouse_position().into())
                        .into(),
                );
            }
        }
        if rl.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) {
            self.click_position = rl.get_mouse_position().into();
            self.dragging_node_positions = Some(self.node_positions.clone());
        }

        if rl.is_mouse_button_released(MouseButton::MOUSE_LEFT_BUTTON) {
            if let Some(box_drag_start) = self.box_drag_start {
                let rect = find_rect(
                    box_drag_start.into(),
                    self.screen_coord_to_world_coor(rl.get_mouse_position().into())
                        .into(),
                );

                let x_1 = rect.x;
                let y_1 = rect.y;
                let x_2 = rect.width + rect.x;
                let y_2 = rect.height + rect.y;

                if !rl.is_key_down(KeyboardKey::KEY_LEFT_SHIFT) {
                    self.selected = vec![];
                }
                for (i, position) in self.node_positions.iter().enumerate() {
                    if position.x >= x_1
                        && position.x <= x_2
                        && position.y >= y_1
                        && position.y <= y_2
                    {
                        self.selected.push(i)
                    }
                }
            }
        }
        if rl.is_mouse_button_released(MouseButton::MOUSE_LEFT_BUTTON) {
            self.dragging_node_positions = None;
        }
        // moving multiple nodes
        if rl.is_mouse_button_down(MouseButton::MOUSE_LEFT_BUTTON) {
            match self.box_drag_start {
                None => {
                    if let Some(_) = coliding_with {
                        for selected in &self.selected {
                            if let Some(dragging) = self.dragging_node_positions.clone() {
                                self.node_positions[*selected] = dragging[*selected]
                                    + self
                                        .screen_coord_to_world_coor(rl.get_mouse_position().into())
                                    - self.screen_coord_to_world_coor(self.click_position.into())
                            } else {
                                println!("error")
                            }
                        }
                    }
                }
                _ => (),
            }
        }
        if rl.is_mouse_button_pressed(MouseButton::MOUSE_RIGHT_BUTTON) {
            if let Some(node) = coliding_with {
                self.dragging_from = Some(node)
            }
        }
        if let Some(from_node) = self.dragging_from {
            let mut dragging_to = None;
            if !rl.is_mouse_button_down(MouseButton::MOUSE_RIGHT_BUTTON) {
                dragging_to = coliding_with;
            }
            if let Some(to) = dragging_to {
                if !self.graph.nbh[to].contains(&from_node) {
                    self.add_edge(to, from_node);
                    self.dragging_from = None;
                } else {
                    self.remove_edge(to, from_node);
                    self.dragging_from = None;
                }
            }
        }

        if rl.is_key_pressed(KeyboardKey::KEY_DELETE) {
            self.remove_selected();
        }

        if rl.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) {
            if rl.get_mouse_x() > 100 {
                if !rl.is_key_down(KeyboardKey::KEY_LEFT_SHIFT) {
                    if let Some(coliding_with) = coliding_with {
                        self.selected = vec![coliding_with];
                    }
                } else {
                    if let Some(coliding_with) = coliding_with {
                        self.selected.push(coliding_with);
                    }
                }
            }
        }

        if rl.is_key_pressed(KeyboardKey::KEY_S) {
            for node in &self.selected {
                self.graph.nodes_read[*node] = State(self.selected_state as usize);
                self.graph.nodes_write[*node] = State(self.selected_state as usize);
            }
        }

        if rl.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) && on_blank {
            self.box_drag_start = Some(
                self.screen_coord_to_world_coor(rl.get_mouse_position().into())
                    .into(),
            );
        }
        if !rl.is_mouse_button_down(MouseButton::MOUSE_LEFT_BUTTON) {
            self.box_drag_start = None
        }
    }

    fn remove_selected(&mut self) {
        while let Some(node) = self.selected.pop() {
            self.remove_node(node)
        }
    }

    fn remove_node(&mut self, idx: usize) {
        self.graph.nodes_write.remove(idx);
        self.graph.nodes_read.remove(idx);
        self.node_positions.remove(idx);
        for node in self.graph.nbh.iter_mut() {
            node.retain(|a| *a != idx)
        }
        for node in self.graph.nbh.iter_mut() {
            for edge in node.iter_mut() {
                if *edge > idx {
                    *edge -= 1;
                }
            }
        }
        if let Some(ref mut node) = self.dragging_from {
            if *node > idx {
                *node -= 1;
            }
            if *node == idx {
                self.dragging_from = None;
            }
        }

        self.selected.retain(|a| *a != idx);
        for selection in self.selected.iter_mut() {
            if *selection > idx {
                *selection -= 1;
            }
        }

        self.graph.nbh.remove(idx);
    }
}

fn distribute_hue(index: usize) -> f32 {
    let golden_ratio_conjugate = 0.618033988749895;

    let hue = ((index as f32 * golden_ratio_conjugate) % 1.0) * 360.0;

    hue
}

fn find_rect(corner_1: Vector2, corner_2: Vector2) -> Rectangle {
    Rectangle::new(
        corner_1.x.min(corner_2.x),
        corner_1.y.min(corner_2.y),
        (corner_2.x - corner_1.x).abs(),
        (corner_2.y - corner_1.y).abs(),
    )
}
