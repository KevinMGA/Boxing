# Boxing Ring + Editor (Bevy 0.14)

**Run host:** `cargo run --release -- --host 0.0.0.0:5000`  
**Run client:** `cargo run --release -- --join 127.0.0.1:5000`  
Toggle **Editor**: press **F1** (bevy_egui panel).

- Windows console is hidden in **release** builds.
- Top UI: player names + health bars + center round timer.
- Ring: floor, posts, and 3-level ropes (static).

## FBX note
Bevy does **not** load FBX directly. Convert your `character.fbx` → `character.glb` (glTF 2.0). Use Blender:
1. Open Blender → File → Import → FBX → select your file.
2. File → Export → glTF 2.0 → select **glb** (binary), +Animation.
3. Save to `assets/character.glb`.

(You can script this with Blender's `--background --python convert_fbx_to_gltf.py`.)

## Editor
Press **F1** to open the panel and map animation clip names (from your GLB) to actions. The mapping is saved at `assets/config/anim_map.json`.

## Controls
- Move: WASD
- Guard: Shift (visual state only for now)
- Punches: J (left) / K (right)
"# Boxing" 
