import { Scene, Input } from "phaser";
import Phaser from "phaser";
import { wrap } from "../utils";
import {
  compile_or_null,
  compile_error,
  needs_continue,
  needs_choice_arr,
  continue$,
  set_var_bool,
  get_var,
  saying,
  jump_to_node,
  current_node,
  choose,
  // goto_node,
} from "../../gleamjunk/glisten48/lang/yarn/runner";

export const DEBUG = false;
const debugLoc = [-270, 1957];
const heartEmoji = "❤︎";
const [W, H] = [1024, 768];
const TILE_OFFSET_Y = 256; //yikes

const [platW, platH] = [400, 32];
const [spikeW, spikeH] = [200, 68];

const DEV_MODE = DEBUG;
//const DEV_MODE = false;
const PHI = 0.5 + 5 ** 0.5 * 0.5;
// const MAX_DIALOGUE = ".......................".length; // for width 300*PHI
const MAX_DIALOGUE = ((W / 10) * 0.95) | 0; // for width 1024
const MAX_CHOICES = 5;

let game = null;

const playerConst = {
  // Speed in px/sec
  horiz: 360,
  dashHoriz: 550,
  vert: 500,
  // Durations in milliseconds
  dashDuration: 2000,
  restDuration: 3500,
  // Distance in px
  dialogueDistance: 80,
  ////  dialogueDistance: 700, // useful for testing
  itemDistance: 75,
  interactMinDuration: 1000,
};

function gleamList(a: any): string[] {
  if (a?.head) {
    return ["" + a.head].concat(gleamList(a?.tail));
  }
  return [];
}

export class Game extends Scene {
  public camera: Phaser.Cameras.Scene2D.Camera;
  public platforms: Phaser.Physics.Arcade.StaticGroup;
  public spikes: Phaser.Physics.Arcade.StaticGroup;
  public bombs: Phaser.Physics.Arcade.Group;
  public dialogueElements: (
    | Phaser.GameObjects.GameObject
    | Phaser.GameObjects.Text
  )[] = [];
  public dialogueChoiceText: Phaser.GameObjects.Text[][] = [];
  public dialogueChoiceElements: Phaser.GameObjects.GameObject[][] = [];
  public dialogueBox: Phaser.GameObjects.GameObject;
  public dialogueText: Phaser.GameObjects.Text;
  public dialogueHow: Phaser.GameObjects.Text;
  public statusText: Phaser.GameObjects.Text;
  public statusElements: (
    | Phaser.GameObjects.GameObject
    | Phaser.GameObjects.Text
  )[] = [];
  public dashBar: Phaser.GameObjects.GameObject;
  public canInteractText: Phaser.GameObjects.Text;
  public descText: Phaser.GameObjects.Text;
  public descBack: Phaser.GameObjects.GameObject[];
  public gameover: boolean = false;
  public gameoverText: Phaser.GameObjects.Text;
  public gameoverBack: Phaser.GameObjects.GameObject;
  public isInteracting: boolean = false;
  public canInteract: boolean = false;
  public player: Phaser.Physics.Arcade.Sprite;
  public foundSupplies: boolean = false;
  public chests: Phaser.Physics.Arcade.Sprite[];
  public ship: Phaser.Physics.Arcade.Sprite;
  public tree: Phaser.Physics.Arcade.Sprite;
  public squirrels: Phaser.Physics.Arcade.Group;
  public playerTimes = {
    grounded: 0,
    releaseLeft: 0,
    releaseRight: 0,
    dash: 0,
    dialogueStart: 0,
  };
  public coyoteTime: number = 200; // milliseconds
  public cursors?: Phaser.Types.Input.Keyboard.CursorKeys;
  public wasd?: object;
  public nums: Record<
    // This int is passed to Yarn dialogue VM as the user's conversation choice
    number,
    Phaser.Input.Keyboard.Key | undefined
  >;
  public conversationalists: Record<
    // This string is passed to the Yarn dialogue VM as the Node name (character/item)
    string,
    Phaser.Physics.Arcade.Sprite
  >;
  // TODO: Use Yarn VM to get dialogue and use object's ID as the Node name
  public items: { text: string; obj: Phaser.GameObjects.GameObject }[] = [];
  // This is the dialogue VM
  public dialogue_vm: object; //TODO Gleam types
  // This is a fresh dialogue VM useful during game resets
  public init_vm: object;

  constructor() {
    super("Game");
    game = this;
  }

  preload() {
    this.load.setPath("assets");

    this.load.text("mainstory", "dialog/main/heat_from_fire.yarn");
    this.load.audio("intro", [
      "music/Silk Abbess (Promo Video) - Eymbr.ogg",
      "music/Silk Abbess (Promo Video) - Eymbr.mp3",
    ]);
    this.load.audio("cave", [
      "music/Eymbr - Eymbr VVytch- Black Tragedy - 06 Honor Of The Knight.ogg",
      "music/Eymbr - Eymbr VVytch- Black Tragedy - 06 Honor Of The Knight.mp3",
    ]);
    this.load.image("background", "bg.png");

    this.load.image("maptiles", "sprites/maptiles.png");
    this.load.tilemapTiledJSON("map", "map.json");
    // 400px,32px
    this.load.image("ground", "sprites/grassy_platform.png");
    this.load.image("spikes", "sprites/spikes.png");
    this.load.image("halfspikes", "sprites/halfspikes.png");
    this.load.image("star", "star.png");
    this.load.image("bomb", "bomb.png");
    this.load.image("chest", "sprites/chest.png");
    this.load.image("tree", "sprites/tree.png");
    this.load.spritesheet("ship", "sprites/ship.png", {
      frameWidth: 675 / 2,
      frameHeight: 168,
    });
    this.load.spritesheet("vera", "sprites/sama.png", {
      frameWidth: 150,
      frameHeight: 144,
    });
    this.load.spritesheet("thedude", "dude.png", {
      frameWidth: 32,
      frameHeight: 48,
    });
    this.load.spritesheet("squirrel", "AzdnerSquirrelIdle.png", {
      frameWidth: 80,
      frameHeight: 48,
    });
    // TODO drone, laser
    // TODO dead drone
    // TODO open chest
    // TODO ship lore
    // TODO squirrel tutorial
    // TODO squirrel priest directions
  }

  introText() {
    const viewTime = DEV_MODE ? 0.3 : 1;
    const transTime = DEV_MODE ? 0.2 : 4;
    const text = [
      this.add
        .text(W / 2, H / 2, "Heat from Fire, Fire from Heat", {
          fontFamily: "Arial Black",
          fontSize: 38,
          color: "#ff1964",
          align: "center",
        })
        .setOrigin(0.5)
        .setDepth(100),
      this.add
        .text(W / 2, H / 2 + 70, "A narrative platformer.", {
          fontFamily: "Arial Black",
          fontSize: 27,
          color: "#ff1964",
          align: "center",
        })
        .setOrigin(0.5)
        .setDepth(100),
      this.add.rectangle(W / 2, H / 2, W, H, 0x000000).setDepth(99),
    ];
    text.forEach((txt) => {
      txt.setScrollFactor(0);
      this.time.addEvent({
        delay: viewTime * 1000,
        callback: () => {
          this.tweens.add({
            targets: txt,
            y: -H,
            ease: "EaseIn",
            duration: transTime * 1000,
            yoyo: false,
            repeat: 0,
          });
          this.tweens.add({
            targets: txt,
            alpha: 0,
            ease: "Linear",
            duration: transTime * 1000,
            yoyo: false,
            repeat: 0,
          });
        },
      });
    });
  }

  setupIntroLevel() {
    this.squirrels = this.physics.add.group();
    this.spikes = this.physics.add.staticGroup();
    this.platforms = this.physics.add.staticGroup();

    this.ship = this.physics.add
      .staticSprite(-600 - 100, 568 - 168 + 48 + TILE_OFFSET_Y, "ship")
      .setScale(2);
    this.conversationalists.Ship = this.ship;

    // ShipBookshelves
    // -596, 709
    // -512,709  seat
    // -704,709
    this.items.push({
      obj: this.ship,
      text: saying(continue$(jump_to_node(this.dialogue_vm, "Ship"))),
    });
    this.items.push({
      obj: this.add.rectangle(-596, 709, 10, 10, 0xff0000).setAlpha(0.01),
      text: saying(
        continue$(jump_to_node(this.dialogue_vm, "Ship_Bookshelves")),
      ),
    });

    // Squirrels
    const rescueSquirrel = this.squirrels
      .create(-600 - platW - 60, 350, "squirrel")
      .setBounce(0.2)
      .setFlipX(true)
      .setCollideWorldBounds(true)
      .setScale(1.5)
      .setSize(40, 48);
    // TODO tutorial squirrel
    this.conversationalists.Secret_Squirrel = rescueSquirrel;
    this.squirrels
      .create(400, 350, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(1.5)
      .setSize(40, 48);
    this.squirrels
      .create(100, 200, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(1.5);
    this.squirrels
      .create(150, 200, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(1.5);
    const mainSquirrel = this.squirrels
      .create(600, 350, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(1.5)
      .refreshBody();
    mainSquirrel.flipX = true;
    this.conversationalists.Friendly_Squirrel = mainSquirrel;
    this.squirrels.children.iterate((obj) => {
      obj.y += TILE_OFFSET_Y;
      if (obj === mainSquirrel) return null;
      this.items.push({
        obj,
        // TODO get Node text from VM?
        text: Phaser.Math.RND.pick([
          "A squirrel sits watching another squirrel attentively",
          "**chitter chitter**",
          "It's just a squirrel",
        ]),
      });
      return null;
    });

    const lastLoc = [-1570, 6277];
    this.chests = [
      this.physics.add
        .staticSprite(680, 150 + TILE_OFFSET_Y, "chest")
        .setOrigin(0)
        .refreshBody(),
      this.physics.add
        .staticSprite(lastLoc[0], lastLoc[1] - 40, "chest")
        .setScale(1.2)
        .setFlipX(true)
        .setOrigin(0)
        .refreshBody(),
    ];
    this.items.push({
      obj: this.chests[0],
      text: "This chest contains the right materials to repair your engine.",
    });
    this.items.push({
      obj: this.chests[1],
      text: "This chest contains the remaining materials you need.",
    });
  }

  setupAnims() {
    this.anims.create({
      key: "ship-interior",
      frames: this.anims.generateFrameNumbers("ship", { start: 1, end: 1 }),
    });
    this.anims.create({
      key: "ship-exterior",
      frames: this.anims.generateFrameNumbers("ship", { start: 0, end: 0 }),
    });
    this.anims.create({
      key: "idle",
      frames: this.anims.generateFrameNumbers("vera", { start: 0, end: 0 }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "idleDodge",
      frames: this.anims.generateFrameNumbers("vera", {
        start: 11,
        end: 11,
      }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "jump",
      frames: this.anims.generateFrameNumbers("vera", { start: 9, end: 9 }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "fall",
      frames: this.anims.generateFrameNumbers("vera", { start: 10, end: 10 }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "right",
      frames: this.anims.generateFrameNumbers("vera", { start: 1, end: 8 }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "rightDodge",
      frames: this.anims.generateFrameNumbers("vera", {
        start: 11,
        end: 11 + 6,
      }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "squirrel-idle",
      frames: this.anims.generateFrameNumbers("squirrel", {
        start: 0,
        end: 10,
      }),
      frameRate: 5,
      repeat: -1,
    });
  }

  setupUI() {
    this.statusText = this.add
      .text(32 * 1.5, 32 * 1.5, heartEmoji.repeat(5), {
        fontSize: "22px",
        color: "#ff1964",
      })
      .setOrigin(0)
      .setDepth(96);
    this.dashBar = this.add
      .rectangle(32 * 1.5, 32 * 1.5 + 12 * 2, 0, 12, 0x101010)
      .setOrigin(0)
      .setDepth(95);
    this.statusElements = [this.statusText, this.dashBar];
    this.statusElements.forEach((e) => (e || e?.body).setScrollFactor(0));
    this.descBack = [
      this.add
        .rectangle(W / 2, H / 2, W / PHI, H / PHI, 0x000000)
        .setDepth(95)
        .setScrollFactor(0),
    ];
    this.descText = this.add
      .text(W / 2, H / 2, "", {
        fontSize: "22px",
        color: "#ffffff",
      })
      .setDepth(96)
      .setOrigin(0.5)
      .setScrollFactor(0);
    this.canInteractText = this.add
      .text(W / 2, H / 2 - 110 + 32, "", {
        fontSize: "22px",
        color: "#000",
      })
      .setOrigin(0.5)
      .setScrollFactor(0);
    this.gameoverBack = this.add
      .rectangle(W / 2, H / 2, W, H / PHI / 5, 0x000000)
      .setDepth(95)
      .setAlpha(0)
      .setScrollFactor(0);
    this.gameoverText = this.add
      .text(W / 2, H / 2, "", {
        fontSize: "40px",
        color: "#ff1964",
      })
      .setDepth(96)
      .setOrigin(0.5)
      .setScrollFactor(0);

    const dialogueW = W;
    const dialogueH = (400 * PHI) / 4;
    this.dialogueBox = this.add
      .rectangle(0, H - dialogueH, dialogueW, dialogueH, 0xff1964)
      .setOrigin(0)
      .setDepth(95);
    const dialogueHowBox = this.add
      .rectangle(32, H - dialogueH - 32, dialogueW / PHI, 32, 0x000)
      .setOrigin(0)
      .setDepth(95);
    this.dialogueHow = this.add
      .text(32 * PHI, H - dialogueH - 32, "", {
        fontSize: "24px",
        color: "#ff1964",
        stroke: "#ff1964",
        strokeThickness: 1,
      })
      .setOrigin(0)
      .setDepth(96);
    this.dialogueText = this.add
      .text(32 * 1.5, H - dialogueH + 32 / PHI, "", {
        fontSize: "16px",
        color: "#000",
        stroke: "#333",
        strokeThickness: 2,
      })
      .setOrigin(0)
      .setDepth(96);
    this.dialogueChoiceText = new Array(MAX_CHOICES)
      .fill(undefined)
      .map((_, i) => [
        this.add
          .text(
            96 * PHI,
            H - dialogueH - 64 - 32 * i - 12,
            `Dialogue option ${i + 1}`,
            {
              fontSize: "24px",
              color: "#000",
              stroke: "#000",
              strokeThickness: 1,
            },
          )
          .setOrigin(0)
          .setDepth(96),
        this.add
          .text(32 * PHI, H - dialogueH - 64 - 32 * i - 12, `> ${i + 1}`, {
            fontSize: "24px",
            color: "#ff1964",
            stroke: "#ff1964",
            strokeThickness: 1,
          })
          .setOrigin(0)
          .setDepth(96),
      ]);
    this.dialogueChoiceElements = new Array(MAX_CHOICES)
      .fill(undefined)
      .map((_, i) => [
        this.add
          .rectangle(
            32 * PHI - 16,
            H - dialogueH - 64 - 32 * i - 16,
            32 * PHI + 32,
            32 * 0.98,
            0x000000,
          )
          .setOrigin(0)
          .setDepth(95),
        this.add
          .rectangle(
            64 * PHI + 32,
            H - dialogueH - 64 - 32 * i - 16,
            W,
            32 * 0.98,
            0xff1964,
          )
          .setOrigin(0)
          .setDepth(94),
      ]);
    this.dialogueElements = [
      this.dialogueBox,
      this.dialogueHow,
      dialogueHowBox,
      this.dialogueText,
      ...this.dialogueChoiceElements.flat(),
      ...this.dialogueChoiceText.flat(),
    ];
    this.dialogueElements.forEach((e) => e.setScrollFactor(0));
  }

  setupLowerLevel() {
    this.tree = this.physics.add
      .sprite(-W / 2, H * 2, "tree")
      .setScale(0.5)
      .refreshBody();
    this.conversationalists.Tree = this.tree;

    this.squirrels
      .create(W * 0.4, H * 2.5 + TILE_OFFSET_Y, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(2)
      .refreshBody();
    this.squirrels
      .create(W * 0.5, H * 2.5 + TILE_OFFSET_Y, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(2)
      .refreshBody();
    const squirrel2 = this.squirrels
      .create(W * 0.7, H * 2.5 + TILE_OFFSET_Y, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(4)
      .refreshBody();
    squirrel2.flipX = true;

    this.conversationalists.Big_Squirrel = squirrel2;
    this.squirrels.children.iterate((obj) => {
      if (obj === squirrel2 || this.items.map(({ obj }) => obj).includes(obj))
        return null;
      this.items.push({
        obj,
        text: "A squirrel sits watching another squirrel attentively",
      });
      return null;
    });
  }

  create() {
    this.items = [];
    this.conversationalists = {};

    // idk why this
    this.sound.unlock();

    // 512,384 is the center of the screen
    this.add
      .tileSprite(512, 384 - H + TILE_OFFSET_Y, W * 9, H, "background")
      .setFlipY(true);
    this.add.tileSprite(512, 384 + TILE_OFFSET_Y, W * 9, H, "background");
    this.add
      .rectangle(-W * 4, H + TILE_OFFSET_Y, W * 8, H * 3, 0x205030)
      .setOrigin(0);

    const map = this.make.tilemap({ key: "map" });
    const tiles = map.addTilesetImage("maptiles", "maptiles");
    const tileLayer = map.createLayer(0, tiles, -W - 100 * 10, 75);
    tileLayer?.setCollisionByProperty({ collides: true });

    this.introText();
    this.physics.world.setBounds(-W * 2, 0, tileLayer.width, tileLayer.height);

    // for funny compile-error game over
    this.setupUI();

    const error: string = compile_error(this.cache.text.get("mainstory"));
    // TODO log errors
    if (error) {
      console.log("Error compiling", error);
      this.gameOver("compilation error.", 5000);
      return;
    }
    this.init_vm = compile_or_null(this.cache.text.get("mainstory"));
    this.dialogue_vm = this.init_vm;
    this.dialogue_vm = set_var_bool(this.dialogue_vm, "$test_from_js", true);
    console.log("get test", get_var(this.dialogue_vm, "$test_from_js"));
    console.log(
      "get test dne",
      get_var(this.dialogue_vm, "$$$$$var_not_exists_test"),
    );

    this.setupIntroLevel();
    this.setupLowerLevel();
    // The Dude abides
    this.player = this.physics.add
      .sprite(DEBUG ? debugLoc[0] : 100, DEBUG ? debugLoc[1] - 20 : 760, "vera")
      //.sprite(-1300, 450, "vera")
      .setBounce(0.2)
      .setScale(0.73)
      .setAlpha(0.95)
      .setCollideWorldBounds(true)
      .refreshBody();
    this.player.name = "player vera orion (sama)";
    this.player.setSize(
      (this.player.body?.width || 0) / 2,
      this.player?.body.height,
    );

    // 2nd level before tree
    this.turrets = [
      this.add.rectangle(480, 1861, 50, 50, 0xff0000),
      this.add.rectangle(480 + 98, 1861, 50, 50, 0xff0000),
    ];

    this.turretguns = this.turrets.map((t) =>
      this.add.rectangle(t.x, t.y - 15, 90, 18, 0xa04ff0),
    );
    const shoot = () =>
      this.turrets.map((t) => {
        const aim = Phaser.Math.Angle.Between(
          this.player.x,
          this.player.y - Phaser.Math.FloatBetween(20, 40),
          t.x,
          t.y,
        );

        if (
          Phaser.Math.Distance.Between(this.player.x, this.player.y, t.x, t.y) >
          W * 0.9
        ) {
          return;
        }
        this.bombs
          .create(t.x, t.y - 35, "bomb")
          .setBounce(6)
          .setVelocity(-Math.cos(aim) * 400, -Math.sin(aim) * 400);
      });
    this.time.addEvent({
      delay: Phaser.Math.Between(900, 1300),
      loop: true,
      callbackContext: this,
      callback: shoot,
    });

    this.camera = this.cameras.main;
    this.setupAnims();

    this.bombs = this.physics.add.group();

    this.cursors = this.input.keyboard?.createCursorKeys();
    this.wasd = this.input.keyboard?.addKeys("W,S,A,D");
    this.nums = {
      1: this.input.keyboard?.addKey(Phaser.Input.Keyboard.KeyCodes.ONE),
      2: this.input.keyboard?.addKey(Phaser.Input.Keyboard.KeyCodes.TWO),
      3: this.input.keyboard?.addKey(Phaser.Input.Keyboard.KeyCodes.THREE),
      4: this.input.keyboard?.addKey(Phaser.Input.Keyboard.KeyCodes.FOUR),
      5: this.input.keyboard?.addKey(Phaser.Input.Keyboard.KeyCodes.FIVE),
    };

    // Run these after all other init
    this.physics.add.collider(this.player, this.platforms);
    this.physics.add.collider(
      this.player,
      this.bombs,
      this.hitBomb,
      null,
      this,
    );
    this.physics.add.collider(
      this.player,
      this.spikes,
      this.hitSpike,
      null,
      this,
    );
    if (DEBUG) {
      const debugGraphics = this.add.graphics().setAlpha(0.5);
      tileLayer.renderDebug(debugGraphics, {
        tileColor: null, // Color of non-colliding tiles
        collidingTileColor: new Phaser.Display.Color(243, 134, 48, 255), // Color of colliding tiles
        faceColor: new Phaser.Display.Color(40, 39, 37, 255), // Color of colliding face edges
      });
    }
    [this.player, this.squirrels, this.tree, this.bombs].map((sprite) => {
      this.physics.add.collider(sprite, this.platforms);
      tileLayer &&
        this.physics.add.collider(
          sprite,
          tileLayer,
          this.onTileCollision,
          null,
          this,
        );
    });

    this.sound.pauseOnBlur = true;
    const s = (this.intromusic = this.sound.add("intro", {
      loop: true,
      volume: 0.25,
    }));
    this.intromusic.play();
    this.actionmusic = this.sound.add("cave", { loop: true, volume: 0.25 });
    this.actionmusic.play();
    this.actionmusic.pause();
    console.log(this.actionmusic.isPaused, "ispaus");
  }

  updateDialogue(
    continueJustReleased: boolean,
    choices: Record<number, boolean>, // pass straight to VM
    yarnNodeName: string,
    obj: Phaser.GameObjects.GameObject,
  ) {
    let next;

    if (current_node(this.dialogue_vm) != yarnNodeName) {
      console.log("going to", yarnNodeName);
      this.dialogue_vm = jump_to_node(this.dialogue_vm, yarnNodeName);
    }

    this.dialogueElements.map((e) => (e.alpha = 1));
    this.dialogueChoiceElements.forEach((l) =>
      l.forEach((e) => (e.alpha = 0.65)),
    );
    const presentingOptions = needs_choice_arr(this.dialogue_vm);
    if (presentingOptions.length < MAX_CHOICES) {
      this.dialogueChoiceText.forEach((l, ix) =>
        l.forEach((e) => {
          e.alpha = ix >= presentingOptions.length ? 0 : 1;
          if (ix < presentingOptions.length) {
            if (e.x > 32 * PHI) e.setText(presentingOptions[ix]);
          }
        }),
      );
      this.dialogueChoiceElements.forEach((l, ix) =>
        l.forEach((e) => (e.alpha = ix >= presentingOptions.length ? 0 : 0.65)),
      );
    }
    this.dialogueBox.alpha = 0.65;
    const name = (yarnNodeName || "").replace(/_/g, " ");

    if (continueJustReleased) {
      console.log("SPACE");
      this.dialogue_vm = continue$(this.dialogue_vm);
    }
    const currentDialogue =
      presentingOptions.length === 0 ? saying(this.dialogue_vm) : "";

    const justChose = Object.entries(choices)
      .filter(([ix, val]) => val)
      .map(([ix, _]) => ix);
    if (justChose.length > 0) {
      this.dialogue_vm = choose(this.dialogue_vm, Number(justChose[0]) - 1);
    }

    // If running, animate at obj location
    const ix = (1 + (this.time.now - this.playerTimes.dialogueStart) / 60) | 0;
    const sub = currentDialogue.slice(0, ix);
    this.dialogueText.setText(wrap(sub, MAX_DIALOGUE));
    let t = "";
    if (needs_continue(this.dialogue_vm)) {
      t = "Press SPACE to continue";
    } else if (presentingOptions.length > 0) {
      t = "Press NUMBER to select";
    }
    this.dialogueHow.setText(name + ": " + t);
  }

  closeDialogue() {
    // Hide UI
    this.dialogueElements.forEach((e) => (e.alpha = 0));
    // Reset animation, if any
    this.playerTimes.dialogueStart = this.time.now;
  }

  updateInventory(text: string, obj: Phaser.GameObjects.GameObject) {
    const ox = obj.body?.position.x || 0;
    const oy = (obj.body?.position.y || 0) - 32;
    this.descBack.forEach((e) => {
      e.alpha = 0.55;
    });
    this.descText.alpha = 1;
    this.descText.setText(wrap(text, 45));

    // hack
    if (obj === this.chests[0]) {
      // set VM variable
      this.foundSupplies = true;
      this.dialogue_vm = set_var_bool(this.dialogue_vm, "$foundSupplies", true);
      console.log(
        "Found supplies?",
        get_var(this.dialogue_vm, "$foundSupplies")[0],
      );
    }
    if (obj === this.chests[1]) {
      // set VM variable
      this.foundSupplies = true;
      this.dialogue_vm = set_var_bool(
        this.dialogue_vm,
        "$foundSupplies2",
        true,
      );
      console.log(
        "Found more supplies?",
        get_var(this.dialogue_vm, "$foundSupplies2")[0],
      );
    }
  }
  closeInventory() {
    this.descBack.forEach((e) => (e.alpha = 0));
    this.descText.alpha = 0;
  }

  pad(checker: (pad: Phaser.Input.Gamepad.Gamepad) => boolean) {
    return (this.input?.gamepad?.gamepads.filter(checker) || []).length > 0;
  }
  updatePlayer() {
    if (get_var(this.dialogue_vm, "$ending_vera_dies")[0] === true) {
      this.gameOver("The tree has consumed you.");
    }
    if (get_var(this.dialogue_vm, "$ending_vera_enlightened")[0] === true) {
      this.gameOver("enlightenment found.");
    }
    const onGround =
      this.player.body?.touching.down || this.player?.tileCollide;

    this.turretguns?.map((t, index) => {
      const aim = Phaser.Math.Angle.Between(
        this.player.x,
        this.player.y,
        this.turrets[index].x,
        this.turrets[index].y,
      );
      t?.setRotation(aim);
    });

    // WASD
    const lDown =
      this.cursors?.left.isDown ||
      this.wasd?.A?.isDown ||
      this.pad((p) => p.left);
    const rDown =
      this.cursors?.right.isDown ||
      this.wasd?.D?.isDown ||
      this.pad((p) => p.right);
    const dDown =
      this.cursors?.down.isDown ||
      this.wasd?.S?.isDown ||
      this.pad((p) => p.down);
    const uDown =
      this.cursors?.up.isDown ||
      this.wasd?.W?.isDown ||
      this.pad((p) => p.up || p.B || p.L1 > 0.5 || p.L2 > 0.5);
    const uJustUp =
      (this.cursors?.up && Input.Keyboard.JustUp(this.cursors?.up)) ||
      (this.wasd?.W && Input.Keyboard.JustUp(this.wasd?.W));
    const sDown =
      this.cursors?.shift.isDown ||
      this.pad((p) => p.R1 > 0.5 || p.R2 > 0.5 || p.A);

    if (lDown || rDown || uDown || dDown) {
      this.isInteracting = false;
    }

    // Camera follow
    this.camera.centerOnX(this.player.x);
    if (this.player.y % H) {
      this.camera.centerOnY(this.player.y);
    }

    if (this.player.y > H * 1.3) {
      // Night mode
      this.dashBar.fillColor = 0xffffff;
      this.canInteractText.setColor("#fff");
      this.intromusic.pause();
      this.actionmusic.isPaused &&
        this.time.addEvent({
          delay: 2000,
          callbackScope: this,
          callback: function () {
            this.actionmusic.isPaused && this.actionmusic.play();
          },
        });
    } else {
      this.dashBar.fillColor = 0x000000;
      this.canInteractText.setColor("#ff1964");
      //this.actionmusic.pause();
      //this.intromusic.isPaused && this.intromusic.play();
    }

    if (
      Phaser.Math.Distance.Between(
        this.player.x,
        this.player.y,
        this.ship.x,
        this.ship.y,
      ) < 290
    )
      this.ship.anims.play("ship-interior", true);
    else this.ship.anims.play("ship-exterior", true);

    // Basic movement controls set velocity directly
    const canDash =
      this.time.now - this.playerTimes.dash > playerConst.restDuration;
    const isDashing =
      this.player.body?.touching.down &&
      this.time.now - this.playerTimes.dash < playerConst.dashDuration;

    if (isDashing) {
      // TODO DASHING IS BROKEN DUE TO BAD TILE COLLISION
      this.dashBar.alpha = 0;
      // TODO cleanup, use tweens?
      this.dashBar.width =
        ((1 -
          (this.time.now - this.playerTimes.dash) / playerConst.dashDuration) *
          W) /
        PHI /
        2;
    } else if (!canDash) {
      // TODO DASHING IS BROKEN DUE TO BAD TILE COLLISION
      this.dashBar.alpha = 0;
      this.dashBar.width =
        (2 *
          (-0.5 +
            (this.time.now - this.playerTimes.dash) /
              playerConst.restDuration) *
          W) /
        PHI /
        2;
      if (this.dashBar.width < 0) this.dashBar.width = 0;
    } else {
      this.dashBar.alpha = 0;
    }

    const horizSpeed = isDashing ? playerConst.dashHoriz : playerConst.horiz;
    const vertSpeed = playerConst.vert;
    if (lDown) {
      // 160 px/sec
      this.player.setVelocityX(-horizSpeed);
      // TODO smaller hitbox
      // TODO move slower, slower dash too
    } else if (rDown) {
      this.player.setVelocityX(horizSpeed);
    } else {
      this.player.setVelocityX(0);
    }
    if (!onGround) {
      if ((this.player.body?.velocity.y || 0) > 0)
        this.player.anims.play("fall", true);
      else this.player.anims.play("jump", true);
    } else if (this.player.body?.velocity.x == 0) {
      this.player.anims.play(dDown ? "idleDodge" : "idle", true);
    } else if (lDown) {
      this.player.anims.play(dDown ? "rightDodge" : "right", true);
    } else if (rDown) {
      this.player.anims.play(dDown ? "rightDodge" : "right", true);
    }
    if (lDown) this.player.flipX = true;
    if (rDown) this.player.flipX = false;

    if (sDown && (lDown || rDown) && canDash) {
      this.playerTimes.dash = this.time.now;
    }

    // Coyote Time: allow jumping even after falling off a platform
    if (
      uDown &&
      (onGround || this.time.now - this.playerTimes.grounded < this.coyoteTime)
    ) {
      this.player.setVelocityY(-vertSpeed);
    }
    if (this.player.body?.touching.down) {
      this.playerTimes.grounded = this.time.now;
    }

    // Fast falling: less wasted time falling
    if (
      !this.player.body?.touching.down &&
      this.player.body?.velocity.y &&
      this.player.body?.velocity.y > 0
    ) {
      this.player.setGravityY(700);
    } else {
      this.player.setGravityY(300);
    }
    // Allow fine-grained jump height control by releasing up key early
    if (uJustUp) {
      if ((this.player.body?.velocity.y || 0) < 0) {
        this.player.setVelocityY(0);
      }
    }
    this.player.tileCollide = false;
  }

  update() {
    if (this.gameover) return;
    const spaceJustUp =
      (this.cursors?.space && Input.Keyboard.JustUp(this.cursors?.space)) ||
      this.pad((p) => p.X || p.Y);
    false;

    const choices = {
      1: false,
      2: false,
      3: false,
      4: false,
      5: false,
    };
    Object.entries(this.nums).forEach(([key, value]) => {
      if (value && Input.Keyboard.JustUp(value)) {
        choices[key as keyof typeof choices] = true;
      }
    });

    this.squirrels.children.iterate((s) =>
      s.anims.play(
        { key: "squirrel-idle", frameRate: Phaser.Math.Between(3, 8) },
        true,
      ),
    );
    const talkers = Object.entries(this.conversationalists).filter(
      ([_, obj]) =>
        Phaser.Math.Distance.Between(
          this.player.x,
          this.player.y,
          obj.x,
          obj.y,
        ) <
        (obj.width && obj.width / 2 > playerConst.dialogueDistance
          ? obj.width / 2
          : playerConst.dialogueDistance),
    );
    if (talkers.length > 0 && talkers.length > 0 && this.isInteracting) {
      this.lastNode = talkers[0][0];
      this.updateDialogue(spaceJustUp, choices, ...talkers[0]);
    } else {
      this.closeDialogue();
    }
    let nearbyItems = this.items.filter(({ obj }) => {
      const [x, y] = [
        obj.body?.position.x ?? obj?.getCenter().x ?? -Infinity,
        obj.body?.position.y ?? obj?.getCenter().y ?? -Infinity,
      ];
      return (
        Phaser.Math.Distance.Between(this.player.x, this.player.y, x, y) <
        (obj.width && obj.width > playerConst.itemDistance
          ? obj.width
          : playerConst.itemDistance)
      );
    });

    // Messy UI code!
    if (nearbyItems.length > 0 && talkers.length === 0 && this.isInteracting) {
      this.updateInventory(nearbyItems[0].text, nearbyItems[0].obj);
    } else {
      this.closeInventory();
    }
    const nearInteractive = talkers.length > 0 || nearbyItems.length > 0;
    this.canInteract = nearInteractive && !this.isInteracting;
    if (this.canInteract) {
      this.canInteractText.setText(talkers.length > 0 ? "➥Listen" : "➦View");
      if (spaceJustUp) {
        this.isInteracting = true;
      }
    } else {
      this.canInteractText.setText("");
    }
    if (!nearInteractive) {
      if (this.lastNode && current_node(this.dialogue_vm) !== this.lastNode)
        // restart dialogue after leaving
        this.dialogue_vm = jump_to_node(this.dialogue_vm, this.lastNode);
      this.isInteracting = false;
    }

    // Although we've added a lot of code it should all be pretty readable.
    this.updatePlayer();
  }

  restart() {
    this.dialogue_vm = this.init_vm;
    this.gameover = false;
    this.anims.resumeAll();
    this.scene.restart();
  }

  gameOver(text: string, pauseDelay: number = 2000) {
    this.intromusic?.stop();
    this.actionmusic?.stop();
    this.anims.pauseAll();
    this.gameoverText.setText("Game Over: " + text);
    this.gameoverBack.alpha = 0.65;
    this.gameover = true;
    this.time.addEvent({
      delay: 40,
      callback: () => this.physics?.pause(),
    });
    this.time.addEvent({
      delay: pauseDelay,
      callback: () => this.restart(),
    });
  }

  hitBomb(_bomb: any, player: Phaser.GameObjects.Sprite) {
    _bomb.alpha = 0;
    this.gameOver("shot by bomb.");
  }

  hitSpike(_a, _b) {
    this.gameOver("impaled on a spike.");
  }

  onTileCollision(obj, tile) {
    if (this.bombs.children.contains(obj)) {
      obj.destroy();
    }
    if (obj === this.player) {
      if (obj.y >= this.player.y) {
        this.player.tileCollide = true;
        if (tile?.properties?.jumpy === true) {
          this.player.setVelocityY(-3800);
        }
      }
    }
    if (obj === this.player && tile?.properties?.kill === true)
      this.gameOver("landed on a spike.");
  }
}
