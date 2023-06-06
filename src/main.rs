use raylib::prelude::*;
use std::borrow::Borrow;
use std::f32::consts;
use std::ffi::{CStr, CString};
use std::fmt::Display;

const WIDTH: i32 = 600;
const HEIGHT: i32 = 800;

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

    let mut app = App::new(
        wireworld,
        vec!["head".to_owned(), "tail".to_owned(), "wire".to_owned()],
    );

    for (_, state) in [
        State(2),
        State(2),
        State(2),
        State(0),
        State(1),
        State(2),
        State(2),
    ]
    .iter()
    .enumerate()
    {
        app.add_node(
            *state,
            Vector2 {
                x: get_random_value::<i32>(0, 640) as f32,
                y: get_random_value::<i32>(0, 480) as f32,
            },
        )
    }

    for edge in vec![
        (0, 1),
        (1, 0),
        (1, 2),
        (2, 1),
        (2, 3),
        (3, 2),
        (3, 4),
        (4, 3),
        (4, 0),
        (0, 4),
        (0, 5),
        (5, 0),
        (6, 5),
        (5, 6),
        (6, 2),
    ] {
        app.add_edge(edge.0, edge.1)
    }

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

struct Rule {
    pattern: Pattern,
    replacement: State,
}

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

#[derive(Clone, Copy, PartialEq)]
struct State(usize);

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

struct App {
    graph: Graph,
    node_positions: Vec<Vector2>,
    screen_offset: Vector2,
    zoom: f32,
    playing: bool,
    state_names: Vec<String>,
    selected_state: i32,
    dragging_from: Option<usize>,
    selected: Option<usize>,
    type_scroll: i32,
    spring_simulation: bool,
}

impl App {
    fn new(ruleset: Ruleset, state_names: Vec<String>) -> Self {
        Self {
            graph: Graph::new(Vec::new(), Vec::new(), ruleset),
            node_positions: Vec::new(),
            screen_offset: Vector2::zero(),
            zoom: 1.0,
            playing: false,
            state_names,
            selected_state: 0,
            dragging_from: None,
            selected: None,
            type_scroll: 0,
            spring_simulation: false,
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

    fn add_node(&mut self, node: State, position: Vector2) {
        self.graph.add_node(node);
        self.node_positions.push(position);
    }

    fn render(&mut self, thread: &RaylibThread, rl: &mut RaylibHandle) {
        {
            let height = rl.get_screen_height();
            let width = rl.get_screen_width();
            let mut d = rl.begin_drawing(&thread);

            d.clear_background(Color::color_from_hsv(0.5, 0.1, 1.0));

            if let Some(selected) = self.selected {
                d.draw_circle_v(
                    (self.node_positions[selected] + self.screen_offset) * self.zoom,
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
                        distribute_hue(self.graph.nodes_read[node].0),
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

            // gui-------------------------------------------------------------------------------
            d.gui_panel(rrect(0, 0, 100, height));
            self.playing =
                d.gui_check_box(rrect(10, 10, 10, 10), Some(rstr!("playing")), self.playing);
            d.gui_line(rrect(10, 15, 80, 20), None);
            self.spring_simulation = d.gui_check_box(
                rrect(10, 30, 10, 10),
                Some(rstr!("Spring sim")),
                self.spring_simulation,
            );

            let top_left = Vector2::new(
                (self
                    .node_positions
                    .iter()
                    .map(|a| a.x as i32)
                    .min()
                    .unwrap_or(0) as f32)
                    .min(self.screen_coord_to_world_coor(Vector2::zero()).x),
                (self
                    .node_positions
                    .iter()
                    .map(|a| a.y as i32)
                    .min()
                    .unwrap_or(0) as f32)
                    .min(
                        self.screen_coord_to_world_coor(Vector2 { x: 0.0, y: 0.0 })
                            .y,
                    ),
            );

            let bottom_right = Vector2::new(
                (self
                    .node_positions
                    .iter()
                    .map(|a| a.x as i32)
                    .max()
                    .unwrap_or(0) as f32)
                    .max(
                        self.screen_coord_to_world_coor(Vector2 {
                            x: width as f32,
                            y: height as f32,
                        })
                        .x,
                    ),
                (self
                    .node_positions
                    .iter()
                    .map(|a| a.y as i32)
                    .max()
                    .unwrap_or(0) as f32)
                    .max(
                        self.screen_coord_to_world_coor(Vector2 {
                            x: width as f32,
                            y: height as f32,
                        })
                        .y,
                    ),
            );

            let viewport_top_left = self.screen_coord_to_world_coor(Vector2::zero());
            let viewport_bottom_right =
                self.screen_coord_to_world_coor(Vector2::new(width as f32, height as f32));

            println!(
                "{:?}",
                (viewport_top_left.x) / (top_left.x - bottom_right.x)
            );

            let aspect_ratio = (top_left.x - bottom_right.x) / (top_left.y - bottom_right.y);
            let viewport_aspect_ratio = (viewport_top_left.x - viewport_bottom_right.x)
                / (viewport_top_left.y - viewport_bottom_right.y);

            d.draw_rectangle(
                width - (100.0 * aspect_ratio) as i32 - 20,
                height - (100.0 * aspect_ratio.recip()) as i32 - 20,
                (100.0 * aspect_ratio) as i32,
                (100.0 * aspect_ratio.recip()) as i32,
                Color::SKYBLUE,
            );

            d.draw_rectangle(
                width
                    - (100.0 * aspect_ratio
                        + (viewport_top_left.x - top_left.x) / (top_left.x - bottom_right.x)
                            * aspect_ratio
                            * 100.0) as i32
                    - 20,
                height
                    - (100.0 * aspect_ratio.recip()
                        + (viewport_top_left.y - top_left.y) / (top_left.y - bottom_right.y)
                            * aspect_ratio
                            * 100.0) as i32
                    - 20,
                (100.0 * viewport_aspect_ratio * aspect_ratio) as i32,
                (100.0 * viewport_aspect_ratio.recip() * aspect_ratio) as i32,
                Color::BLACK,
            );

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

    fn screen_coord_to_world_coor(&self, pos: Vector2) -> Vector2 {
        pos / self.zoom - self.screen_offset
    }

    fn init_positions(&mut self, width: usize, height: usize) {
        let size = self.node_positions.len();
        for (i, position) in self.node_positions.iter_mut().enumerate() {
            *position = Vector2::new(
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
        start: Vector2,
        end: Vector2,
        color: Color,
        radius: f32,
    ) {
        // Calculate arrowhead size based on spring length
        let spring_length = (end - start).length();
        let arrowhead_size = spring_length / 20.0;

        // Calculate normalized direction vector of the spring
        let direction = (end - start).normalized();

        // Calculate perpendicular vector to the spring direction
        let perpendicular = Vector2 {
            x: -direction.y,
            y: direction.x,
        };

        let control_point = (start + end) / 2.0 + perpendicular * (end - start).length() * 0.2;

        let arrow_direction = (end - control_point).normalized();
        let arrow_perpendicular = Vector2 {
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
            arrowhead_left - direction * radius,
            end - direction * radius,
            arrowhead_right - direction * radius,
            color,
        );

        d.draw_line_bezier_quad(
            start + direction * radius,
            end - direction * radius,
            control_point,
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
            if (self.screen_coord_to_world_coor(rl.get_mouse_position()) - *node).length() < 30.0
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
                    self.screen_coord_to_world_coor(rl.get_mouse_position()),
                );
            }
        }
        if rl.is_mouse_button_down(MouseButton::MOUSE_LEFT_BUTTON) {
            if let Some(node) = coliding_with {
                self.node_positions[node] =
                    self.screen_coord_to_world_coor(rl.get_mouse_position());
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

        if let Some(node) = self.selected {
            if rl.is_key_pressed(KeyboardKey::KEY_DELETE) {
                self.remove_node(node);
                self.selected = None;
            }
        }

        if rl.is_mouse_button_pressed(MouseButton::MOUSE_LEFT_BUTTON) {
            if rl.get_mouse_x() > 100 {
                self.selected = coliding_with;
            }
        }

        if rl.is_key_pressed(KeyboardKey::KEY_S) {
            if let Some(selected) = self.selected {
                self.graph.nodes_read[selected] = State(self.selected_state as usize);
                self.graph.nodes_write[selected] = State(self.selected_state as usize);
            }
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

        self.graph.nbh.remove(idx);
    }
}

fn distribute_hue(index: usize) -> f32 {
    let golden_ratio_conjugate = 0.618033988749895;

    let hue = ((index as f32 * golden_ratio_conjugate) % 1.0) * 360.0;

    hue
}
