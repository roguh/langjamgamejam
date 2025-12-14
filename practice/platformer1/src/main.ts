import kaplay from "kaplay";

// This defines all function as globals
// Use k. prefix anyway to get better autocomplete
const k = kaplay();

import { pickAnim } from "./util";
import { saveTime, loadGame, fastSaveGame } from "./save";
import { makeTextButton } from "kaplay-ui/inputs";

k.setGravity(1000);
const [CW, CH] = [150, 210]; // Size for humanoid sprites
const [TW, TH] = [100, 100]; // Size for terrain tile sprites
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
    width: CW * 11,
    height: CH,
    sliceX: 11,
    anims: {
      idle: { from: 0, to: 0 },
      move: { from: 1, to: 8, loop: true, pingpong: false, speed: 12 },
      jump: { from: 9, to: 9 },
      fall: { from: 10, to: 10 },
    },
  },
  sama_sneak: {
    x: 0,
    y: CH,
    width: CW * 11,
    height: CH,
    sliceX: 11,
    anims: {
      idle: { from: 0, to: 0 },
      move: { from: 0, to: 5, loop: true, pingpong: false, speed: 12 },
    },
  },
  sama_roll: {
    x: 0,
    y: CH * 2,
    width: CW * 11,
    height: CH,
    sliceX: 11,
    anims: {
      idle: { from: 0, to: 0 },
      move: { from: 0, to: 11, loop: true, pingpong: false, speed: 15 },
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
        const fps = (1 / k.dt()).toFixed(1);
        txt.text = `FPS: ${fps} ${player.pos.x.toFixed(1)},${player.pos.y.toFixed(1)}`;
        txt.lastUpdate = k.time();
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
    jump_force: 700,
    direction: "right",
    isRolling: false,
    mineSpeed: 20,
    baseSprite: "sama",
    rollSprite: "sama_sneak",
    shape: new k.Rect(k.vec2(0, (PH * (1 - 0.7)) / 2), PW / 2, PH * 0.7),
    rollShape: new k.Rect(k.vec2(0, (PH * (1 - 0.45)) / 2), PW / 2, PH * 0.45),
    roll: (v: boolean) => {
      // TODO Avoid collision when standing up
      player.isRolling = v;
      player.area.shape = player.isRolling ? player.rollShape : player.shape;
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
k.onLoad(() => {
  map.use(k.scale(0.95));
  player.init();
  k.setCamScale(1.5);
  loadGame(player);
});

const actions = {
  lastDown: 0,
  lastUp: 0,
  reset: () => player.moveTo(player.initPos),
  up: () => {
    console.debug("action.up");
    // TODO allow holding the up button to double jump the maximum height...
    player.doubleJump(player.jump_force);
    pickAnim(player);
    actions.lastUp = k.time();
  },
  left: () => {
    console.debug("action.left");
    player.move(-player.speed, 0);
    pickAnim(player, "move");
    player.flipX = true;
  },
  leftStop: () => pickAnim(player),
  downStart: () => {
    console.debug("action.downStart");
    player.roll(!player.isRolling);
    pickAnim(player);
    actions.lastDown = k.time();
  },
  right: () => {
    console.debug("action.right");
    player.move(player.speed, 0);
    pickAnim(player, "move");
    player.flipX = false;
  },
  rightStop: () => pickAnim(player),
  clickOrTouchStart: (pos) => {
    console.debug("action.clickOrTouchStart");
    if (pos.dist(txt.pos) < txt.width) {
      k.debug.inspect = !k.debug.inspect;
    }
  },
  clickOrTouch: (pos) => {
    console.debug("action.clickOrTouch");
    if (pos.dist(buttons.up.pos) < buttons.up.width) {
      if (k.time() - actions?.lastUp > 0.25) actions.up();
    }
    if (pos.dist(buttons.left.pos) < buttons.left.width) {
      actions.left();
    }
    if (pos.dist(buttons.down.pos) < buttons.down.width / 2) {
      if (k.time() - actions?.lastDown > 1) actions.downStart();
    }
    if (pos.dist(buttons.right.pos) < buttons.right.width) {
      actions.right();
    }
  },
};

k.onKeyPress("0", actions.reset);
k.onKeyPress("up", actions.up);
k.onKeyDown("left", actions.left);
k.onKeyPress("down", actions.downStart);
k.onKeyDown("right", actions.right);
k.onKeyPress("w", actions.up);
k.onKeyDown("a", actions.left);
k.onKeyPress("s", actions.downStart);
k.onKeyDown("d", actions.right);
k.onKeyRelease("a", actions.leftStop);
k.onKeyRelease("d", actions.rightStop);
k.onKeyRelease("left", actions.leftStop);
k.onKeyRelease("right", actions.rightStop);
k.onMousePress(() => actions.clickOrTouchStart(k.mousePos()));
k.onMouseDown(() => actions.clickOrTouch(k.mousePos()));
if (k.isTouchscreen()) {
  k.onTouchStart(actions.clickOrTouchStart);
  k.onTouchMove(actions.clickOrTouch);
}

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

// Mobile touch controls
const bD = 150;
const [SW, SH] = [2.5 * bD, window.innerHeight - 3 * bD];
const buttons = {
  left: k.add(makeTextButton("<", SW - 2 * bD, SH, bD, bD)),
  right: k.add(makeTextButton(">", SW, SH, bD, bD)),
  up: k.add(makeTextButton("^", SW - bD, SH - bD, bD, bD)),
  down: k.add(makeTextButton("v", SW - bD, SH + bD, bD, bD)),
};
// Each button is in a fixed position
Object.values(buttons).map((b) => {
  b.use(k.fixed());
  if (!k.isTouchscreen()) b.hidden = true;
});

k.onUpdate(() => {
  txt.update();
  k.setCamPos(player.pos.x, player.pos.y);

  // Fall off map?
  if (player.pos.y > 10000) {
    actions.reset();
  }
  // Save every 1 second
  if (k.time() - saveTime() > 1) {
    saveTime(k.time());
    fastSaveGame(player);
  }
});
