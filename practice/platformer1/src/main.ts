import kaplay from "kaplay";

// This defines all function as globals
// Use k. prefix anyway to get better autocomplete
const k = kaplay();

import { pickAnim } from "./util";
import { saveTime, loadGame, fastSaveGame } from "./save";

k.setGravity(1000);
const [CW, CH] = [100, 210];
const [TW, TH] = [100, 100];
const [PW, PH] = [CW, CH];

k.loadRoot("./"); // A good idea for Itch.io publishing later
k.loadSprite("bean", "sprites/bean.png");
k.loadSpriteAtlas("sprites/map.png", {
  floor: { x: 0, y: 0, width: TW, height: TH },
  block: { x: TW, y: 0, width: TW, height: TH },
});

k.loadSpriteAtlas("sprites/sama.png", {
  sama: {
    x: 0,
    y: 0,
    width: CW * 20,
    height: CH,
    sliceX: 20,
    anims: {
      idle: { from: 0, to: 0 },
      move: { from: 0, to: 2 },
      jump: { from: 3, to: 4 },
      idle_roll: { from: 8, to: 8 },
      jump_roll: { from: 8, to: 11 },
      move_roll: { from: 8, to: 11 },
    },
  },
});

const map = k.addLevel(
  [
    "5                                                     5",
    "5                                                     5",
    "5   111                  111                  111     5",
    "5        111                                          5",
    "5                                   111               5",
    "5   111              111                              5",
    "5             111                                     5",
    "5111111                      111           111        5",
    "5155555                                               5",
    "5155555   111                                         5",
    "5155551111111111111111111111111111111111111111111111115",
    "5155555555555555555555555555555555555555555555555555115",
  ],
  {
    tileWidth: TW,
    tileHeight: TH,
    tiles: {
      1: () => [
        "solid",
        k.sprite("floor"),
        k.area(),
        k.body({ isStatic: true }),
      ],
      5: () => [
        "solid",
        k.sprite("block"),
        k.area(),
        k.body({ isStatic: true }),
        { health: 100 },
      ],
    },
  },
);

const txt = k.add([
  "text",
  k.pos(15, 40),
  k.fixed(),
  k.text("", { size: 30 }),
  {
    lastUpdate: 0,
    update: () => {
      if (k.time() - txt.lastUpdate > 1) {
        const fps = Math.floor(10 / k.dt()) / 10;
        txt.lastUpdate = k.time();
        txt.text = `FPS: ${fps}`;
      }
    },
  },
]);

const player = k.add([
  "mover",
  "player",
  k.sprite("sama"),
  k.area(),
  // TODO anchor point stays static when rolling
  k.anchor("center"),
  k.body(),
  k.doubleJump(),
  k.pos(),
  {
    initPos: k.vec2(300, 80),
    speed: 200,
    jump_force: 400,
    direction: "right",
    isRolling: false,
    mineSpeed: 20,
    shape: new k.Rect(k.vec2(0, (PH * (1 - 0.7)) / 2), PW / 2, PH * 0.7),
    rollShape: new k.Rect(k.vec2(0, (PH * (1 - 0.25)) / 2), PW / 2, PH * 0.25),
    roll: (v: boolean) => {
      // TODO Avoid collision when standing up
      player.isRolling = v;
      player.area.shape = player.isRolling ? player.rollShape : player.shape;
      pickAnim(player);
    },
    canMine: () => player.isRolling,
    init: () => {
      player.area.shape = player.shape;
      player.moveTo(player.initPos);
      pickAnim(player);
    },
    toSaveFile: () => ({
      pos: { x: player.pos.x, y: player.pos.y },
      flipX: player.flipX,
      isRolling: player.isRolling,
    }),
    fromSaveFile: (p) => {
      player.moveTo(k.vec2(p.pos.x, p.pos.y));
      player.flipX = p.flipX;
      player.roll(p.isRolling);
    },
  },
]);

// Initialization
map.use(k.scale(0.78));
k.onLoad(() => {
  player.init();
  k.setCamScale(1.3);
  loadGame(player);
});

k.onKeyPress("up", () => {
  player.doubleJump(player.jump_force);
  pickAnim(player);
});

// TODO do not standup if no vertical space
k.onKeyRelease("down", () => player.roll(!player.isRolling));

k.onKeyPress("q", () => {
  player.moveTo(player.initPos);
});

k.onKeyDown("right", () => {
  pickAnim(player, "move");
  player.flipX = false;
  player.move(player.speed, 0);
});

k.onKeyDown("left", () => {
  pickAnim(player, "move");
  player.flipX = true;
  player.move(-player.speed, 0);
});

k.onKeyRelease("right", () => pickAnim(player));
k.onKeyRelease("left", () => pickAnim(player));

k.onMouseRelease(() => {
  console.log(k.mousePos().dist(txt.pos));
  if (k.mousePos().dist(txt.pos) < txt.width)
    k.debug.inspect = !k.debug.inspect;
});

k.onCollide("mover", "solid", (a, b) => {
  // Stop jump animation if on ground
  pickAnim(player);
});

k.onCollideUpdate("player", "solid", (a, b, col) => {
  const canMine = a.canMine() && b.health !== undefined && col?.normal.y === 0;
  if (canMine && b.health > 0) {
    b.health -= a.mineSpeed * k.dt();
    k.shake(0.1);
  }
  if (canMine && b.health <= 0) {
    b.destroy();
    k.shake(30);
  }
});

k.onUpdate(() => {
  txt.update();
  k.setCamPos(player.pos.x, player.pos.y);
  // Save every 1 second
  if (k.time() - saveTime() > 1) {
    saveTime(k.time());
    fastSaveGame(player);
  }
});
