#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use bevy::prelude::*;
use bevy_egui::{egui, EguiContexts, EguiPlugin};
use crossbeam_channel::{Receiver, Sender};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::time::{Duration, Instant};

#[derive(States, Debug, Clone, Eq, PartialEq, Hash, Default)]
enum AppMode { #[default] Game, Editor }

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct AnimMap { step_forward:String, step_back:String, step_left:String, step_right:String, idle:String, guard_idle:String, jab:String, cross:String, weave_left:String, weave_right:String }
#[derive(Resource)] struct AnimConfig { map: AnimMap, dirty: bool, path: String }
impl AnimConfig {
    fn load_or_default(path:&str)->Self{
        let map=fs::read_to_string(path).ok().and_then(|s|serde_json::from_str::<AnimMap>(&s).ok()).unwrap_or_default();
        Self{map,dirty:false,path:path.to_string()}
    }
    fn save_if_dirty(&mut self){ if self.dirty { if let Ok(json)=serde_json::to_string_pretty(&self.map){ let _=fs::create_dir_all(std::path::Path::new(&self.path).parent().unwrap()); let _=fs::write(&self.path,json);} self.dirty=false; } }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Resource)] struct IsHost(bool);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Resource)] struct PlayerId(u8);

#[derive(Debug, Clone, Serialize, Deserialize)]
struct InputMsg{ frame:u64, left:bool, right:bool, up:bool, down:bool, guard:bool, punch_left:bool, punch_right:bool }

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
struct SimPlayer{ x:f32,y:f32,z:f32, health:f32, punch_l:f32, punch_r:f32, guard:bool }

#[derive(Debug, Clone, Serialize, Deserialize)] struct SnapshotMsg{ frame:u64, p0:SimPlayer, p1:SimPlayer }
#[derive(Clone)] struct ToNet(pub InputMsg);
#[derive(Clone)] struct FromNet(pub SnapshotMsg);
#[derive(Resource)] struct NetChannels{ to_net:Sender<ToNet>, from_net:Receiver<FromNet> }

#[derive(Component)] struct Boxer{ id:u8 }
#[derive(Component,Default,Clone,Copy)] struct PunchState{ left_time:f32, right_time:f32 }
#[derive(Component,Default,Clone,Copy)] struct Health{ current:f32, max:f32 }
#[derive(Component,Default,Clone,Copy)] struct Guard(pub bool);
#[derive(Component)] struct Ring;
#[derive(Resource)] struct Names{ left:String, right:String }
#[derive(Resource)] struct RoundClock{ time_left:f32 }
#[derive(Component)] struct UiLeftBar; #[derive(Component)] struct UiRightBar; #[derive(Component)] struct UiTimer; #[derive(Component)] struct UiStatus;

const RING_SIZE:f32=16.0; const MOVE_SPEED:f32=4.5; const PUNCH_TIME:f32=0.28; const SNAPSHOT_HZ:f64=30.0;

fn main(){
    let mut role:&'static str="host"; let mut addr="127.0.0.1:5000".to_string(); let mut start_editor=false;
    let args=std::env::args().skip(1).collect::<Vec<_>>(); let mut i=0; while i<args.len(){ match args[i].as_str(){
        "--host"=>{role="host"; if i+1<args.len(){addr=args[i+1].clone(); i+=1;}},
        "--join"=>{role="join"; if i+1<args.len(){addr=args[i+1].clone(); i+=1;}},
        "--editor"=>{start_editor=true;},
        _=>{}
    } i+=1; }
    let is_host=role=="host";
    let (to_net_tx,to_net_rx)=crossbeam_channel::unbounded::<ToNet>();
    let (from_net_tx,from_net_rx)=crossbeam_channel::unbounded::<FromNet>();
    if is_host { let a=addr.clone(); std::thread::spawn(move||host_thread(a,to_net_rx,from_net_tx)); }
    else { let a=addr.clone(); std::thread::spawn(move||client_thread(a,to_net_rx,from_net_tx)); }

    let initial_state=if start_editor { AppMode::Editor } else { AppMode::Game };
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin{
            primary_window:Some(Window{
                title:if is_host{"Boxing - Host (F1 Editor)".into()}else{"Boxing - Client (F1 Editor)".into()},
                resolution:(1080.0,720.0).into(),
                ..Default::default()
            }),
            ..Default::default()
        }))
        .add_plugins(StatePlugin::<AppMode>::default())
        .add_plugins(EguiPlugin)
        .insert_state(initial_state)
        .insert_resource(IsHost(is_host))
        .insert_resource(PlayerId(if is_host {0}else{1}))
        .insert_resource(NetChannels{to_net:to_net_tx,from_net:from_net_rx})
        .insert_resource(Names{left:"Player One".into(), right:"Player Two".into()})
        .insert_resource(RoundClock{time_left:180.0})
        .insert_resource(AnimConfig::load_or_default("assets/config/anim_map.json"))
        .add_systems(Startup,(setup_scene,setup_ui,setup_status))
        .add_systems(Update,(toggle_editor,side_camera_follow,auto_face,local_input_system,apply_motion,ring_bounds,punch_decay,net_receive_system,ui_update,round_timer))
        .add_systems(Update,editor_ui.run_if(in_state(AppMode::Editor)))
        .run();
}

fn setup_scene(mut commands:Commands, mut meshes:ResMut<Assets<Mesh>>, mut materials:ResMut<Assets<StandardMaterial>>){
    commands.spawn((Camera3dBundle{ transform:Transform::from_translation(Vec3::new(0.0,9.0,18.0)).looking_at(Vec3::new(0.0,1.2,0.0),Vec3::Y), ..Default::default()},));
    commands.spawn((DirectionalLightBundle{ directional_light:DirectionalLight{shadows_enabled:false, illuminance:18_000.0, ..Default::default()}, transform:Transform::from_rotation(Quat::from_euler(EulerRot::XYZ,-0.9,0.8,0.0)), ..Default::default()},));
    let floor=materials.add(StandardMaterial{ base_color:Color::srgb(0.35,0.42,0.55), perceptual_roughness:0.9, ..Default::default()});
    commands.spawn((PbrBundle{ mesh:meshes.add(Mesh::from(bevy::math::primitives::Rectangle{half_size:Vec2::splat(RING_SIZE*0.5), ..Default::default()})), material:floor.clone(), transform:Transform::from_rotation(Quat::from_rotation_x(-std::f32::consts::FRAC_PI_2)), ..Default::default()}, Ring));
    let post_mat=materials.add(StandardMaterial{ base_color:Color::srgb(0.1,0.1,0.12), ..Default::default()});
    let post_positions=[Vec3::new(-RING_SIZE*0.5,0.0,-RING_SIZE*0.5), Vec3::new(RING_SIZE*0.5,0.0,-RING_SIZE*0.5), Vec3::new(RING_SIZE*0.5,0.0,RING_SIZE*0.5), Vec3::new(-RING_SIZE*0.5,0.0,RING_SIZE*0.5)];
    for p in post_positions { commands.spawn(PbrBundle{ mesh:meshes.add(Mesh::from(bevy::math::primitives::Cylinder{radius:0.12, half_height:1.2})), material:post_mat.clone(), transform:Transform::from_translation(p+Vec3::Y*1.2), ..Default::default()}); }
    let rope_mat=materials.add(StandardMaterial{ base_color:Color::srgb(0.85,0.05,0.05), ..Default::default()});
    let heights=[0.6,0.95,1.25];
    let horiz=[(Vec3::new(-RING_SIZE*0.5,0.0,-RING_SIZE*0.5),Vec3::new(RING_SIZE*0.5,0.0,-RING_SIZE*0.5)), (Vec3::new(-RING_SIZE*0.5,0.0,RING_SIZE*0.5),Vec3::new(RING_SIZE*0.5,0.0,RING_SIZE*0.5))];
    let vert=[(Vec3::new(-RING_SIZE*0.5,0.0,-RING_SIZE*0.5),Vec3::new(-RING_SIZE*0.5,0.0,RING_SIZE*0.5)), (Vec3::new(RING_SIZE*0.5,0.0,-RING_SIZE*0.5),Vec3::new(RING_SIZE*0.5,0.0,RING_SIZE*0.5))];
    for h in heights {
        for (a,b) in horiz { let len=(b.x-a.x).abs(); commands.spawn(PbrBundle{
            mesh:meshes.add(Mesh::from(bevy::math::primitives::Cylinder{radius:0.04, half_height:len*0.5})), material:rope_mat.clone(),
            transform:Transform{ translation:Vec3::new(0.0,h,a.z), rotation:Quat::from_rotation_z(std::f32::consts::FRAC_PI_2), ..Default::default()}, ..Default::default()}); }
        for (a,b) in vert { let len=(b.z-a.z).abs(); commands.spawn(PbrBundle{
            mesh:meshes.add(Mesh::from(bevy::math::primitives::Cylinder{radius:0.04, half_height:len*0.5})), material:rope_mat.clone(),
            transform:Transform{ translation:Vec3::new(a.x,h,0.0), rotation:Quat::from_rotation_x(std::f32::consts::FRAC_PI_2), ..Default::default()}, ..Default::default()}); }
    }
    let red=materials.add(StandardMaterial{ base_color:Color::srgb(0.9,0.2,0.2), ..Default::default()});
    let blue=materials.add(StandardMaterial{ base_color:Color::srgb(0.2,0.5,0.9), ..Default::default()});
    commands.spawn((PbrBundle{ mesh:meshes.add(Mesh::from(bevy::math::primitives::Cuboid{half_size:Vec3::splat(0.4)})), material:red, transform:Transform::from_translation(Vec3::new(-2.0,0.4,0.0)), ..Default::default()}, Boxer{id:0}, PunchState::default(), Health{current:100.0,max:100.0}, Guard(false)));
    commands.spawn((PbrBundle{ mesh:meshes.add(Mesh::from(bevy::math::primitives::Cuboid{half_size:Vec3::splat(0.4)})), material:blue, transform:Transform::from_translation(Vec3::new(2.0,0.4,0.0)), ..Default::default()}, Boxer{id:1}, PunchState::default(), Health{current:100.0,max:100.0}, Guard(false)));
}

fn setup_ui(mut commands:Commands, asset_server:Res<AssetServer>){
    commands.spawn(NodeBundle{ style:Style{ width:Val::Percent(100.0), height:Val::Percent(100.0), justify_content:JustifyContent::SpaceBetween, align_items:AlignItems::FlexStart, ..Default::default()}, ..Default::default()})
    .with_children(|root|{
        root.spawn(NodeBundle{ style:Style{ margin:UiRect::all(Val::Px(12.0)), ..Default::default()}, ..Default::default()})
        .with_children(|left|{
            left.spawn(TextBundle::from_section("Player One", TextStyle{ font:asset_server.load("fonts/FiraSans-Bold.ttf"), font_size:24.0, color:Color::WHITE }));
            left.spawn(NodeBundle{ style:Style{ width:Val::Px(260.0), height:Val::Px(16.0), margin:UiRect{ top:Val::Px(6.0), ..UiRect::DEFAULT }, ..Default::default()}, background_color:Color::srgb(0.2,0.2,0.25).into(), ..Default::default()})
            .with_children(|bar|{ bar.spawn((NodeBundle{ style:Style{ width:Val::Px(260.0), height:Val::Px(16.0), ..Default::default() }, background_color:Color::srgb(0.0,0.8,0.2).into(), ..Default::default()}, UiLeftBar)); });
        });
        root.spawn(NodeBundle{ style:Style{ align_self:AlignSelf::FlexStart, ..Default::default()}, ..Default::default()})
        .with_children(|mid|{ mid.spawn((TextBundle::from_section("3:00", TextStyle{ font:asset_server.load("fonts/FiraSans-Bold.ttf"), font_size:28.0, color:Color::WHITE }), UiTimer)); });
        root.spawn(NodeBundle{ style:Style{ margin:UiRect::all(Val::Px(12.0)), ..Default::default()}, ..Default::default()})
        .with_children(|right|{
            right.spawn(TextBundle::from_section("Player Two", TextStyle{ font:asset_server.load("fonts/FiraSans-Bold.ttf"), font_size:24.0, color:Color::WHITE }));
            right.spawn(NodeBundle{ style:Style{ width:Val::Px(260.0), height:Val::Px(16.0), margin:UiRect{ top:Val::Px(6.0), ..UiRect::DEFAULT }, ..Default::default()}, background_color:Color::srgb(0.2,0.2,0.25).into(), ..Default::default()})
            .with_children(|bar|{ bar.spawn((NodeBundle{ style:Style{ width:Val::Px(260.0), height:Val::Px(16.0), ..Default::default() }, background_color:Color::srgb(0.0,0.8,0.2).into(), ..Default::default()}, UiRightBar)); });
        });
    });
}

fn setup_status(mut commands:Commands, assets:Res<AssetServer>, host:Res<IsHost>){
    let who=if host.0 { "HOST" } else { "CLIENT (waiting if no host yet)" };
    commands.spawn(TextBundle{
        text:Text::from_section(format!("{}  —  F1: Editor", who), TextStyle{ font:assets.load("fonts/FiraSans-Bold.ttf"), font_size:18.0, color:Color::WHITE }),
        style:Style{ position_type:PositionType::Absolute, left:Val::Px(12.0), bottom:Val::Px(12.0), ..Default::default() },
        ..Default::default()
    }).insert(UiStatus);
}

fn toggle_editor(kb:Res<ButtonInput<KeyCode>>, mut next:ResMut<NextState<AppMode>>, state:Res<State<AppMode>>){
    if kb.just_pressed(KeyCode::F1){ let to=if state.get()==&AppMode::Game { AppMode::Editor } else { AppMode::Game }; next.set(to); }
}

fn side_camera_follow(mut q_cam:Query<&mut Transform,With<Camera>>, q_p:Query<&Transform,With<Boxer>>){
    let mut cam=q_cam.single_mut(); let mut center=Vec3::ZERO; let mut count=0.0;
    for t in &q_p { center+=t.translation; count+=1.0; }
    if count>0.0 { center/=count; }
    let target=Vec3::new(0.0,8.5,18.0); cam.translation=cam.translation.lerp(target,0.1); cam.look_at(center+Vec3::Y*1.2,Vec3::Y);
}

fn auto_face(q_pos:Query<(&Transform,&Boxer)>, mut q_set:Query<(&mut Transform,&Boxer)>){
    let mut x0=None; let mut x1=None;
    for (t,b) in &q_pos { if b.id==0 { x0=Some(t.translation.x); } if b.id==1 { x1=Some(t.translation.x); } }
    if let (Some(a),Some(bx))=(x0,x1){ let dir=(bx-a).signum(); for (mut t,b) in &mut q_set {
        if b.id==0 { t.rotation=Quat::from_rotation_y(if dir>=0.0 {0.0} else {std::f32::consts::PI}); }
        else { t.rotation=Quat::from_rotation_y(if dir>=0.0 {std::f32::consts::PI} else {0.0}); }
    }}
}

fn local_input_system(kb:Res<ButtonInput<KeyCode>>, time:Res<Time>, net:Res<NetChannels>){
    let frame=(time.elapsed_seconds_f64()*120.0) as u64;
    let input=InputMsg{ frame, left:kb.pressed(KeyCode::KeyA), right:kb.pressed(KeyCode::KeyD), up:kb.pressed(KeyCode::KeyW), down:kb.pressed(KeyCode::KeyS),
        guard:kb.pressed(KeyCode::ShiftLeft)||kb.pressed(KeyCode::ShiftRight), punch_left:kb.just_pressed(KeyCode::KeyJ), punch_right:kb.just_pressed(KeyCode::KeyK) };
    let _=net.to_net.send(ToNet(input));
}

fn apply_motion(time:Res<Time>, kb:Res<ButtonInput<KeyCode>>, host:Res<IsHost>, mut q:Query<(&mut Transform,&mut PunchState,&mut Guard,&Boxer)>){
    let dt=time.delta_seconds();
    for (mut t,mut punch,mut guard,boxer) in &mut q {
        let mut vx=0.0; let mut vz=0.0; let i_control_this=if host.0 { boxer.id==0 } else { boxer.id==1 };
        if i_control_this {
            if kb.pressed(KeyCode::KeyA){vx-=MOVE_SPEED;} if kb.pressed(KeyCode::KeyD){vx+=MOVE_SPEED;}
            if kb.pressed(KeyCode::KeyW){vz-=MOVE_SPEED;} if kb.pressed(KeyCode::KeyS){vz+=MOVE_SPEED;}
            if kb.just_pressed(KeyCode::KeyJ){punch.left_time=PUNCH_TIME;} if kb.just_pressed(KeyCode::KeyK){punch.right_time=PUNCH_TIME;}
            guard.0=kb.pressed(KeyCode::ShiftLeft)||kb.pressed(KeyCode::ShiftRight);
        }
        t.translation.x+=vx*dt; t.translation.z+=vz*dt;
        let base=0.8; let sx=base+(if punch.left_time>0.0{0.25}else{0.0})+(if punch.right_time>0.0{0.25}else{0.0}); t.scale=Vec3::splat(sx);
    }
}

fn ring_bounds(mut q:Query<&mut Transform,With<Boxer>>){
    for mut t in &mut q {
        t.translation.x=t.translation.x.clamp(-RING_SIZE*0.45,RING_SIZE*0.45);
        t.translation.z=t.translation.z.clamp(-RING_SIZE*0.45,RING_SIZE*0.45);
        if t.translation.y<0.4{ t.translation.y=0.4; }
    }
}

fn punch_decay(time:Res<Time>, mut q:Query<&mut PunchState,With<Boxer>>){
    let dt=time.delta_seconds(); for mut p in &mut q { p.left_time=(p.left_time-dt).max(0.0); p.right_time=(p.right_time-dt).max(0.0); }
}

fn net_receive_system(net:Res<NetChannels>, mut q:Query<(&mut Transform,&mut PunchState,&mut Health,&mut Guard,&Boxer)>){
    while let Ok(FromNet(snap))=net.from_net.try_recv(){
        for (mut t,mut p,mut h,mut g,b) in &mut q {
            let sp=if b.id==0{snap.p0}else{snap.p1};
            t.translation=Vec3::new(sp.x,sp.y,sp.z); p.left_time=sp.punch_l; p.right_time=sp.punch_r; h.current=sp.health; g.0=sp.guard;
        }
    }
}

fn ui_update(_names:Res<Names>, clock:Res<RoundClock>, mut left:Query<&mut Style,With<UiLeftBar>>, mut right:Query<&mut Style,With<UiRightBar>>, mut timer:Query<&mut Text,With<UiTimer>>, q:Query<(&Health,&Boxer)>){
    let mut lhealth=100.0; let mut rhealth=100.0;
    for (h,b) in &q { if b.id==0 { lhealth=(h.current/h.max*260.0).clamp(0.0,260.0);} else { rhealth=(h.current/h.max*260.0).clamp(0.0,260.0);} }
    if let Ok(mut s)=left.get_single_mut(){ s.width=Val::Px(lhealth); }
    if let Ok(mut s)=right.get_single_mut(){ s.width=Val::Px(rhealth); }
    if let Ok(mut t)=timer.get_single_mut(){ let total=clock.time_left.max(0.0) as i32; let m=total/60; let s=total%60; t.sections[0].value=format!("{:01}:{:02}",m,s); }
}

fn round_timer(time:Res<Time>, mut clock:ResMut<RoundClock>){ clock.time_left-=time.delta_seconds(); }

fn editor_ui(mut ctx:EguiContexts, mut cfg:ResMut<AnimConfig>, mut next:ResMut<NextState<AppMode>>){
    egui::Window::new("Developer Editor").show(ctx.ctx_mut(), |ui|{
        ui.label("Map your animation clip names to gameplay actions."); ui.separator();
        macro_rules! field{ ($label:literal,$f:ident)=>{{
            let mut v=cfg.map.$f.clone(); ui.horizontal(|ui|{ ui.label($label); if ui.text_edit_singleline(&mut v).changed(){ cfg.map.$f=v; cfg.dirty=true; } });
        }};}
        field!("Idle",idle); field!("Guard Idle",guard_idle); ui.separator();
        field!("Step Forward",step_forward); field!("Step Back",step_back); field!("Step Left",step_left); field!("Step Right",step_right); ui.separator();
        field!("Jab",jab); field!("Cross",cross); field!("Weave Left",weave_left); field!("Weave Right",weave_right);
        if ui.button("Save").clicked(){ cfg.save_if_dirty(); }
        if ui.button("Back to Game (F1)").clicked(){ next.set(AppMode::Game); }
        ui.separator(); ui.label("Drop `character.glb` into assets/ to replace the cubes.");
    });
}

fn host_thread(addr:String, to_net_rx:Receiver<ToNet>, from_net_tx:Sender<FromNet>){
    let listener = TcpListener::bind(addr.as_str()).expect("Failed to bind host port");
    listener.set_nonblocking(true).ok();
    println!("[HOST] Waiting for client on {} ...", addr);
    let mut client:Option<TcpStream>=None;
    let mut last_accept=Instant::now();

    let mut p0=SimPlayer{x:-2.0,y:0.4,z:0.0,health:100.0,punch_l:0.0,punch_r:0.0,guard:false};
    let mut p1=SimPlayer{x:2.0,y:0.4,z:0.0,health:100.0,punch_l:0.0,punch_r:0.0,guard:false};
    let tick=Duration::from_micros((1_000_000.0/120.0) as u64);
    let snap_tick=Duration::from_micros((1_000_000.0/SNAPSHOT_HZ) as u64);
    let mut last_snap=Instant::now(); let mut frame:u64=0;

    loop{
        if client.is_none() && last_accept.elapsed()>Duration::from_millis(100){
            if let Ok((stream,_addr))=listener.accept(){ stream.set_nonblocking(true).ok(); client=Some(stream); println!("[HOST] Client connected"); }
            last_accept=Instant::now();
        }
        // host local input
        let mut latest:Option<InputMsg>=None; while let Ok(ToNet(inp))=to_net_rx.try_recv(){ latest=Some(inp); }
        let dt=1.0/120.0;
        if let Some(inp)=latest{
            p0.x += (inp.right as i32 - inp.left as i32) as f32 * MOVE_SPEED * dt;
            p0.z += (inp.down as i32 - inp.up as i32) as f32 * MOVE_SPEED * dt;
            if inp.punch_left { p0.punch_l=PUNCH_TIME; } if inp.punch_right { p0.punch_r=PUNCH_TIME; }
            p0.guard=inp.guard;
        }
        // client input
        if let Some(stream)=client.as_mut(){
            let mut len_buf=[0u8;4];
            if stream.read_exact(&mut len_buf).is_ok(){
                let len=u32::from_le_bytes(len_buf) as usize; let mut buf=vec![0u8;len];
                if stream.read_exact(&mut buf).is_ok(){
                    if let Ok(inp)=bincode::deserialize::<InputMsg>(&buf){
                        p1.x += (inp.right as i32 - inp.left as i32) as f32 * MOVE_SPEED * dt;
                        p1.z += (inp.down as i32 - inp.up as i32) as f32 * MOVE_SPEED * dt;
                        if inp.punch_left { p1.punch_l=PUNCH_TIME; } if inp.punch_right { p1.punch_r=PUNCH_TIME; }
                        p1.guard=inp.guard;
                    }
                }
            }
        }
        // decay & clamp
        p0.punch_l=(p0.punch_l-dt).max(0.0); p0.punch_r=(p0.punch_r-dt).max(0.0);
        p1.punch_l=(p1.punch_l-dt).max(0.0); p1.punch_r=(p1.punch_r-dt).max(0.0);
        for p in [&mut p0,&mut p1]{ p.x=p.x.clamp(-RING_SIZE*0.45,RING_SIZE*0.45); p.z=p.z.clamp(-RING_SIZE*0.45,RING_SIZE*0.45); }
        if last_snap.elapsed() >= snap_tick {
            let snap=SnapshotMsg{frame,p0,p1};
            let _=from_net_tx.send(FromNet(snap.clone()));
            if let Some(stream)=client.as_mut(){
                let payload=bincode::serialize(&snap).unwrap();
                let len=(payload.len() as u32).to_le_bytes();
                let _=stream.write_all(&len); let _=stream.write_all(&payload);
            }
            last_snap=Instant::now();
        }
        frame+=1; std::thread::sleep(tick);
    }
}

fn client_thread(addr:String, to_net_rx:Receiver<ToNet>, from_net_tx:Sender<FromNet>){
    let mut stream=loop{
        match TcpStream::connect(addr.as_str()){
            Ok(s)=>{ let _=s.set_nonblocking(true); println!("[CLIENT] Connected to {}",addr); break s; },
            Err(_)=>{ println!("[CLIENT] Waiting for host..."); std::thread::sleep(Duration::from_millis(500)); }
        }
    };
    let tick_send=Duration::from_micros((1_000_000.0/60.0) as u64); let mut last_send=Instant::now();
    loop{
        if last_send.elapsed() >= tick_send {
            let mut latest:Option<InputMsg>=None; while let Ok(ToNet(inp))=to_net_rx.try_recv(){ latest=Some(inp); }
            if let Some(inp)=latest{
                let payload=bincode::serialize(&inp).unwrap(); let len=(payload.len() as u32).to_le_bytes();
                let _=stream.write_all(&len); let _=stream.write_all(&payload); last_send=Instant::now();
            }
        }
        let mut len_buf=[0u8;4];
        if stream.read_exact(&mut len_buf).is_ok(){
            let len=u32::from_le_bytes(len_buf) as usize; let mut buf=vec![0u8;len];
            if stream.read_exact(&mut buf).is_ok(){ if let Ok(snap)=bincode::deserialize::<SnapshotMsg>(&buf){ let _=from_net_tx.send(FromNet(snap)); } }
        }
        std::thread::sleep(Duration::from_millis(2));
    }
}
