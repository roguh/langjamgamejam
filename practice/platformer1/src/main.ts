import kaplay from "kaplay";

// This defines all function as globals
// Use k. prefix anyway to get better autocomplete
const k = kaplay();

import { pickAnim, tileMaker } from "./util";
import { saveTime, loadGame, fastSaveGame } from "./save";
import { makeTextButton } from "kaplay-ui/inputs";

k.setGravity(1000);
const [CW, CH] = [150, 210]; // Size for humanoid sprites
const [TW, TH] = [100, 100]; // Size for terrain tile sprites
const [PW, PH] = [CW, CH];

k.loadRoot("./"); // A good idea for Itch.io publishing later
k.loadSprite("bean", "sprites/bean.png");
k.loadSpriteAtlas("sprites/map.png", {
  floor_rightdown: { x: 0, y: 0, width: TW, height: TH },
  floor_rightup: { x: TW, y: 0, width: TW, height: TH },
  floor: { x: TW * 2, y: 0, width: TW, height: TH },
  floor_vert: { x: TW * 3, y: 0, width: TW, height: TH },
  flowy: { x: 0, y: TH, width: TW, height: TH },
  flowy_denser: { x: TW, y: TH, width: TW, height: TH },
  flowy_densest: { x: TW * 2, y: TH, width: TW, height: TH },
  coin: {
    x: 0,
    y: TH * 3,
    sliceX: 4,
    width: TW * 4,
    height: TH,
    anims: {
      shine: {
        from: 0,
        to: 3,
      },
    },
  },
});

const [SQW, SQH] = [80, 48];
k.loadSpriteAtlas("sprites/AzdnerSquirrelIdle.png", {
  squirrel: {
    x: 0,
    y: 0,
    width: SQW * 11,
    height: SQH,
    sliceX: 11,
    anims: { idle: { from: 0, to: 10 } },
  },
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
      move: { from: 1, to: 8, loop: true, pingpong: false, speed: 15 },
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
      move: { from: 0, to: 5, loop: true, pingpong: false, speed: 6 },
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
      move: { from: 0, to: 11, loop: true, pingpong: false, speed: 6 },
    },
  },
});

const map = k.addLevel(
  [
    "C                                                     w",
    "w                  5                  5                  W                w",
    "w            5                                                            w",
    "w    5                   W                    5                           w",
    "w                                                           5             w",
    "w                 5                 5                                     w",
    "w                                                 5      5                w",
    "w                                                                         w",
    "w       5                        5                              5         w",
    "w                         5                                               w",
    "w                                        5                                w",
    "w                    5                                     5              w",
    "w                                                                         w",
    "w                         C              5                          C     w",
    "w          C     5                                                        w",
    "w                                                         C               w",
    "w   012                  01111111112     5     2                  01111111w",
    "w        012          C                                                   w",
    "w    C                                                 0111112            w11111111111112",
    "w   012             01112                                                 w             11112",
    "w             012                 5                             C         w                 11111111112",
    "w111111                                         01112        0111112      w",
    "w1wwwww         C                      5                                  w",
    "w1wCWww   111                                    C                        w",
    "w1wWWw11111111111111111111111111111111111111111111111111111111111111111111w",
    "w1wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww11w",
  ],
  {
    tileWidth: TW,
    tileHeight: TH,
    tiles: {
      0: tileMaker("floor_rightup"),
      1: tileMaker("floor"),
      2: tileMaker("floor_rightdown"),
      5: tileMaker("floor_vert"),
      w: tileMaker("flowy", 100),
      W: tileMaker("flowy_densest", 100),
      C: tileMaker("coin", 1, "shine"),
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
      if (k.time() - txt.lastUpdate > 0.1) {
        const fps = (1 / k.dt()).toFixed(1);
        txt.text = `FPS: ${fps} ${player.pos.x.toFixed(1)},${player.pos.y.toFixed(1)}`;
        txt.lastUpdate = k.time();
      }
    },
  },
]);

const squirrel = k.add([
  "mover",
  k.sprite("squirrel"),
  k.area({ shape: new k.Rect(k.vec2(0, SQH / 2), SQW * 0.5, SQW * 0.5) }),
  k.anchor("center"),
  k.body(),
  k.pos(240, 300),
]);

const player = k.add([
  "mover",
  "player",
  k.sprite("sama"),
  k.area(),
  // TODO anchor point stays static when rolling
  k.anchor("center"),
  k.body(),
  k.pos(),
  {
    initPos: k.vec2(100, 700),
    speed: 350,
    rollSpeed: 300,
    runSpeed: 450,
    lastGrounded: 0,
    jump_force: 800,
    direction: "right",
    isRolling: false,
    mineSpeed: 20,
    baseSprite: "sama",
    rollSprite: "sama_sneak",
    shape: new k.Rect(k.vec2(0, (PH * (1 - 0.6)) / 2), PW / 4, PH * 0.6),
    rollShape: new k.Rect(k.vec2(0, (PH * (1 - 0.4)) / 2), PW / 4, PH * 0.4),
    roll: (v: boolean) => {
      // TODO Avoid collision when standing up
      player.isRolling = v;
      player.speed = player.isRolling ? player.rollSpeed : player.runSpeed;
      player.area.shape = player.isRolling ? player.rollShape : player.shape;
    },
    canMine: () => player.isRolling,
    init: () => {
      player.area.shape = player.shape;
      player.moveTo(player.initPos);
      pickAnim(player, "idle");
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
  map.use(k.scale(0.45));
  player.init();
  squirrel.play("idle");
  k.debug.log(squirrel.pos);
  k.setCamScale(1.5);
  loadGame(player);
  // k.debug.inspect = true;
  k.debug.showLog = true;
});

// TODO: add action buffer for attacks and jumps
// when action is ready, play the buffered sequence
const actions = {
  lastDown: 0,
  lastUp: 0,
  reset: () => {
    player.vel.y = 0;
    player.moveTo(player.initPos);
  },
  up: () => {
    console.debug("action.up");
    // Allow coyote time late jumps
    if (player.isGrounded() || k.time() - player.lastGrounded < 0.5)
      player.jump(player.jump_force);
    pickAnim(player, "jump");
    actions.lastUp = k.time();
  },
  upStop: () => {
    // Allow finer jump controls by stopping on release
    if (player.vel.y < 0) player.vel.y = 0;
  },
  left: () => {
    console.debug("action.left");
    player.move(-player.speed, 0);
    pickAnim(player, "move");
    player.flipX = true;
  },
  leftStop: () => pickAnim(player, "idle"),
  // TODO: add action buffer for attacks and jumpos
  // when action is ready, play the buffered sequence
  downStart: () => {
    console.debug("action.downStart");
    player.roll(!player.isRolling);
    pickAnim(player, "idle");
    actions.lastDown = k.time();
  },
  right: () => {
    console.debug("action.right");
    player.move(player.speed, 0);
    pickAnim(player, "move");
    player.flipX = false;
  },
  rightStop: () => pickAnim(player, "idle"),
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

// TODO completeness
k.onKeyPress("0", actions.reset);
k.onKeyPress("up", actions.up);
k.onKeyDown("left", actions.left);
k.onKeyPress("down", actions.downStart);
k.onKeyDown("right", actions.right);
k.onKeyPress("w", actions.up);
k.onKeyDown("a", actions.left);
k.onKeyPress("s", actions.downStart);
k.onKeyDown("d", actions.right);
k.onKeyRelease("w", actions.upStop);
k.onKeyRelease("a", actions.leftStop);
k.onKeyRelease("d", actions.rightStop);
k.onKeyRelease("up", actions.upStop);
k.onKeyRelease("left", actions.leftStop);
k.onKeyRelease("right", actions.rightStop);
k.onMousePress(() => actions.clickOrTouchStart(k.mousePos()));
k.onMouseDown(() => actions.clickOrTouch(k.mousePos()));
if (k.isTouchscreen()) {
  k.onTouchStart(actions.clickOrTouchStart);
  k.onTouchMove(actions.clickOrTouch);
}

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

  // Destroy random tile, starting near x=0
  const maxX = k.time() / 10 + 1;
  if (k.time() % 1 < 0.1 && k.time() < 5)
    map.getSpatialMap().map((row, y) =>
      row.map((tile, x) => {
        // Destroy random tile, starting near x=0 edge of map
        if (x < maxX && Math.random() < 0.01) tile.destroy();
      }),
    );

  // Fall off map?
  if (player.pos.y > 10000) {
    actions.reset();
  }
  if (player.isGrounded()) {
    player.lastGrounded = k.time();
  }
  if (player.isFalling()) {
    player.gravityScale = 2.5;
  } else {
    player.gravityScale = 1;
  }
  // Save every 1 second
  if (k.time() - saveTime() > 1) {
    saveTime(k.time());
    fastSaveGame(player);
    k.debug.log(squirrel.pos);
  }
});
