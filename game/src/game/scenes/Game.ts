import { Math, Scene, Input } from "phaser";
import { wrap } from "../utils";
import {
  compile_or_null,
  needs_continue,
  continue$,
  set_var_bool,
  get_var,
  saying,
  // goto_node,
} from "../../gleamjunk/glisten48/lang/yarn/runner";

const heartEmoji = "❤︎";
const [W, H] = [1024, 768];

const [platW, platH] = [400, 32];
const [spikeW, spikeH] = [200, 68];

const DEV_MODE = true;
//const DEV_MODE = false;
const PHI = 0.5 + 5 ** 0.5 * 0.5;
// const MAX_DIALOGUE = ".......................".length; // for width 300*PHI
// const MAX_DIALOGUE = "................................".length; // for width 400*PHI
const MAX_DIALOGUE = ((W / 10) * 0.95) | 0; // for width 1024
const MAX_CHOICES = 5;
const EMPTY_DIALOGUE = null;
const INIT_DIALOGUE = "";

let game = null;

const playerConst = {
  // Speed in px/sec
  horiz: 260,
  dashHoriz: 550,
  vert: 370,
  // Durations in milliseconds
  dashDuration: 2000,
  restDuration: 3500,
  // Distance in px
  dialogueDistance: 80,
  ////  dialogueDistance: 700, // useful for testing
  itemDistance: 75,
  interactMinDuration: 1000,
};

type DialogueExecutionState =
  | "Stopped"
  | "WaitingOnOptionSelection"
  | "WaitingOnContinue"
  | "Running";

function gleamList(a: any): string[] {
  if (a?.head) {
    return ["" + a.head].concat(gleamList(a?.tail));
  }
  return [];
}

export class Game extends Scene {
  public camera: Phaser.Cameras.Scene2D.Camera;
  public currentDialogue: string;
  public dialogueState: DialogueExecutionState;
  public dialogueVM: string[];
  public platforms: Phaser.Physics.Arcade.StaticGroup;
  public jumpPlatform: Phaser.Physics.Arcade.StaticGroup;
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
  public chest: Phaser.Physics.Arcade.Sprite;
  public ship: Phaser.Physics.Arcade.Sprite;
  public tree: Phaser.Physics.Arcade.Sprite;
  public squirrels: Phaser.Physics.Arcade.Group;
  public conversationalists: Record<string, Phaser.Physics.Arcade.Sprite>;
  public items: { text: string; obj: Phaser.GameObjects.GameObject }[] = [];
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
  public vm: object;
  public init_vm: object;

  constructor() {
    super("Game");
    game = this;
    this.dialogueState = "WaitingOnContinue";
    this.dialogueVM = [
      "Hello, fair traveler!",
      "The Tree is found deeper into the woods towards the bottom-right side of your screen :)",
    ];
    this.currentDialogue = INIT_DIALOGUE;
    this.items = [];
    this.conversationalists = {};

    this.init_vm = this.vm = compile_or_null("title: Test\n---\na b c===\n");
    console.log(needs_continue(this.vm));
    this.vm = continue$(this.vm);
    console.log(gleamList(saying(this.vm)).join("\n"));
    this.vm = set_var_bool(this.vm, "$jstest", true);
    console.log(get_var(this.vm, "$$$$$notexists"));
    console.log(get_var(this.vm, "$jstest")[0]);
  }

  preload() {
    this.load.setPath("assets");

    this.load.image("background", "bg.png");
    // 400px,32px
    this.load.image("ground", "sprites/grassy_platform.png");
    this.load.image("spikes", "sprites/spikes.png");
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
    const transTime = DEV_MODE ? 1 : 4;
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
        .text(
          W / 2,
          H / 2 + 70,
          "A narrative platformer made in the Mountain States\nNo generative AI",
          {
            fontFamily: "Arial Black",
            fontSize: 27,
            color: "#ff1964",
            align: "center",
          },
        )
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

    // Refresh required after scaling
    this.platforms.create(600, platW, "ground");
    this.platforms.create(50, 250, "ground");
    this.platforms.create(750, 220, "ground");
    this.platforms.create(400, 568, "ground").setScale(2).refreshBody();
    this.platforms.create(1300, 568, "ground").setScale(2).refreshBody();
    this.spikes.create(1300 - platW - spikeW / 4, 568 + spikeH / 2, "spikes");
    this.platforms
      .create(-600 - platW * 2, 568, "ground")
      .setScale(3.2, 2)
      .refreshBody();
    const shipPlat = this.platforms
      .create(-600, 568, "ground")
      .setScale(2)
      .refreshBody();
    this.spikes.create(-600 + platW + spikeW / 2, 568 + spikeH / 2, "spikes");
    const shipfloor = this.platforms.create(-600, 568 - 67, "ground");
    shipfloor.alpha = 0;
    const _shipwall = this.platforms.add(
      this.add
        .rectangle(-600 - 390, 568 - 67 - 125, 180, 250, 0x000000)
        .setAlpha(0)
        .setOrigin(0),
    );
    this.platforms.children.iterate((p) =>
      // Fake 3D effect, top of platform is above player's feet
      p.setSize(p.body?.width, (p?.body.height || 100) * 0.6),
    );

    this.ship = this.physics.add
      .staticSprite(shipPlat.x - 100, shipPlat.y - 168 + 48, "ship")
      .setScale(2);

    // Squirrels
    const rescueSquirrel = this.squirrels
      .create(-600 - platW - 60, 350, "squirrel")
      .setBounce(0.2)
      .setFlipX(true)
      .setCollideWorldBounds(true)
      .setScale(1.5)
      .setSize(40, 48);
    this.conversationalists.RescueSquirrel = rescueSquirrel;
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
    this.chest = this.physics.add
      .staticSprite(680, 150, "chest")
      .setOrigin(0)
      .refreshBody();
    this.items.push({
      obj: this.chest,
      text: "This chest contains the right materials to repair your engine.",
    });

    this.conversationalists.SquirrelPriest = mainSquirrel;
    this.squirrels.children.iterate((obj) => {
      if (obj === mainSquirrel) return null;
      this.items.push({
        obj,
        // TODO get Node text from VM?
        text: Math.RND.pick([
          "A squirrel sits watching another squirrel attentively",
          "**chitter chitter**",
          "It's just a squirrel",
        ]),
      });
      return null;
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
        color: "#000",
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
      .text(W / 2, H / 2 - this.player.displayHeight + 32, "", {
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
        color: "#ffffff",
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
            W / PHI,
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
    this.add.rectangle(-W * 4, H, W * 8, H * 3, 0x205030).setOrigin(0);
    this.platforms.add(
      this.add
        .rectangle(-W * 2, H * 2.9, W * 4, H * 0.2, 0x201410)
        .setOrigin(0),
    );
    this.platforms.add(
      this.add
        .rectangle(-W * 2, H * 2.9, W * 4, H * 0.05, 0x102010)
        .setOrigin(0),
    );
    this.platforms.add(
      this.add.rectangle(-W * 2, H * 3.1, W * 4, H, 0x040602).setOrigin(0),
    );
    this.tree = this.physics.add.sprite(-W, H * 2, "tree");
    this.tree.body.setSize(
      this.tree.body?.width,
      (this.tree?.body.height || 100) * 0.7,
    );
    this.conversationalists.Tree = this.tree;
    this.jumpPlatform = this.physics.add.staticGroup();
    this.jumpPlatform.add(
      this.add
        .rectangle(W * 0.8, H * 2.8, W / 2, H * 0.05, 0x882211)
        .setOrigin(0),
    );

    this.squirrels
      .create(W * 0.4, H * 2.5, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(2)
      .refreshBody();
    this.squirrels
      .create(W * 0.5, H * 2.5, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(2)
      .refreshBody();
    const squirrel2 = this.squirrels
      .create(W * 0.7, H * 2.5, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(4)
      .refreshBody();
    squirrel2.flipX = true;

    this.conversationalists.SquirrelLowerLevel = squirrel2;
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
    // 512,384 is the center of the screen
    this.add.tileSprite(512, 384 - H, W * 9, H, "background").setFlipY(true);
    this.add.tileSprite(512, 384, W * 9, H, "background");

    this.introText();
    this.physics.world.setBounds(-W * 2, 0, W * 4, H * 3);

    this.setupIntroLevel();
    this.setupLowerLevel();
    // The Dude abides
    this.player = this.physics.add
      .sprite(100, 450, "vera")
      .setBounce(0.2)
      .setScale(0.73)
      .setAlpha(0.95)
      .setCollideWorldBounds(true)
      .refreshBody();
    this.player.setSize(
      (this.player.body?.width || 0) / 2,
      this.player?.body.height,
    );

    this.camera = this.cameras.main;
    this.setupAnims();

    this.bombs = this.physics.add.group();

    this.bombs
      .create(400, 300, "bomb")
      .setBounce(1)
      .setVelocity(Math.Between(-200, 200), 20);

    this.setupUI();

    this.cursors = this.input.keyboard?.createCursorKeys();
    this.wasd = this.input.keyboard?.addKeys("W,S,A,D");

    // Run these after all other init
    this.physics.add.collider(this.player, this.platforms);
    this.physics.add.collider(
      this.player,
      this.jumpPlatform,
      this.onJumpPlatform,
    );
    this.physics.add.collider(this.bombs, this.platforms);
    this.physics.add.collider(this.tree, this.platforms);
    this.physics.add.collider(this.squirrels, this.platforms);
    this.physics.add.collider(this.player, this.bombs, this.hitBomb);
    this.physics.add.collider(this.player, this.spikes, this.hitSpike);
    this.physics.add.collider(this.player, this.chest, this.hitChest);
  }

  onJumpPlatform(
    player: Phaser.Physics.Arcade.Sprite,
    _jumpPlatform: Phaser.Physics.Arcade.Sprite,
  ) {
    if (player.body?.blocked.down || player.body?.touching.down)
      player.setVelocityY(-1500);
  }

  continueDialogue(): string | null {
    console.log(this.dialogueState, this.currentDialogue);
    // Return the next line of dialogue, if any

    // Temporary: show choices at end
    if (this.dialogueVM.length === 0) {
      this.dialogueState = "WaitingOnOptionSelection";
      return EMPTY_DIALOGUE;
    }

    // No more lines!
    if (this.dialogueState === "Stopped") return EMPTY_DIALOGUE;
    if (this.dialogueVM.length === 0) {
      this.dialogueState = "Stopped";
      return EMPTY_DIALOGUE;
    }
    if (this.dialogueState === "Running") {
      // Skip to end?
      this.dialogueState = "WaitingOnContinue";
      this.playerTimes.dialogueStart = -100000000;
      return EMPTY_DIALOGUE;
    }
    const res = this.dialogueVM[0];
    this.playerTimes.dialogueStart = this.time.now;
    this.dialogueVM = this.dialogueVM.slice(1);
    if (this.dialogueState === "WaitingOnContinue") {
      // Start new line
      this.dialogueState = "Running";
    }
    return res;
  }

  updateDialogue(
    continueJustReleased: boolean,
    yarnNodeName: string,
    obj: Phaser.GameObjects.GameObject,
  ) {
    let next;
    this.dialogueElements.map((e) => (e.alpha = 1));
    this.dialogueChoiceElements.forEach((l) =>
      l.forEach((e) => (e.alpha = 0.65)),
    );
    if (this.dialogueState !== "WaitingOnOptionSelection") {
      this.dialogueChoiceText.forEach((l) => l.forEach((e) => (e.alpha = 0)));
      this.dialogueChoiceElements.forEach((l) =>
        l.forEach((e) => (e.alpha = 0)),
      );
    }
    this.dialogueBox.alpha = 0.65;
    const name = yarnNodeName;

    if (continueJustReleased) {
      if ((next = this.continueDialogue())) this.currentDialogue = next;
    }
    if (this.dialogueState === "Running") {
      // If running, animate at obj location
      const ix =
        (1 + (this.time.now - this.playerTimes.dialogueStart) / 60) | 0;
      const sub = this.currentDialogue.slice(0, ix);
      this.dialogueText.setText(wrap(sub, MAX_DIALOGUE));
      if (ix == this.currentDialogue.length) {
        // If animation is done, switch states
        if ((next = this.continueDialogue())) this.currentDialogue = next;
      }
      this.dialogueHow.setText(name + ": " + "");
    } else {
      this.dialogueText.setText(wrap(this.currentDialogue, MAX_DIALOGUE));
      let t = "";
      if (this.dialogueState === "WaitingOnContinue") {
        t = "Press SPACE to continue";
      } else if (this.dialogueState === "WaitingOnOptionSelection") {
        t = "Press NUMBER to select";
      }
      this.dialogueHow.setText(name + ": " + t);
    }
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
    if (obj === this.chest) {
      // set VM variable
      this.foundSupplies = true;
      this.vm = set_var_bool(this.vm, "$foundSupplies", true);
      console.log("Found supplies?", get_var(this.vm, "$foundSupplies")[0]);
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
      this.statusText.setColor("#fff");
      this.dashBar.fillColor = 0xffffff;
      this.canInteractText.setColor("#fff");
    } else {
      this.statusText.setColor("#000000");
      this.dashBar.fillColor = 0x000000;
      this.canInteractText.setColor("#000000");
    }

    if (this.player.y > H * 6) {
      // Reset
      this.player.setPosition(100, 450);
    }

    if (
      Math.Distance.Between(
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
      this.dashBar.alpha = 1;
      // TODO cleanup, use tweens?
      this.dashBar.width =
        ((1 -
          (this.time.now - this.playerTimes.dash) / playerConst.dashDuration) *
          W) /
        PHI /
        2;
    } else if (!canDash) {
      this.dashBar.alpha = 1;
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
    if (!this.player.body?.touching.down) {
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
      (this.player.body?.touching.down ||
        this.time.now - this.playerTimes.grounded < this.coyoteTime)
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
  }

  update() {
    if (this.gameover) return;
    const spaceJustUp =
      (this.cursors?.space && Input.Keyboard.JustUp(this.cursors?.space)) ||
      this.pad((p) => p.X || p.Y);
    false;

    this.squirrels.children.iterate((s) =>
      s.anims.play(
        { key: "squirrel-idle", frameRate: Math.Between(3, 8) },
        true,
      ),
    );
    const talkers = Object.entries(this.conversationalists).filter(
      ([yarnNode, obj]) =>
        Math.Distance.Between(this.player.x, this.player.y, obj.x, obj.y) <
        (obj.width > playerConst.dialogueDistance
          ? obj.width / 2
          : playerConst.dialogueDistance),
    );
    if (talkers.length > 0 && talkers.length > 0) {
      this.updateDialogue(spaceJustUp, ...talkers[0]);
    } else {
      this.closeDialogue();
    }
    let nearbyItems = this.items.filter(
      ({ obj }) =>
        Math.Distance.Between(
          this.player.x,
          this.player.y,
          obj.body?.position.x ?? -Infinity,
          obj.body?.position.y ?? -Infinity,
        ) < playerConst.itemDistance,
    );

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
      this.isInteracting = false;
    }

    // Although we've added a lot of code it should all be pretty readable.
    this.updatePlayer();
  }

  restart() {
    this.vm = this.init_vm;
    this.gameover = false;
    this.anims.resumeAll();
    this.scene.restart();
  }

  gameOver(text: string) {
    this.anims.pauseAll();
    this.gameoverText.setText("Game Over: " + text);
    this.gameoverBack.alpha = 1;
    this.gameover = true;
    this.time.addEvent({
      delay: 40,
      callback: () => this.physics?.pause(),
    });
    this.time.addEvent({
      delay: 2000,
      callback: () => this.restart(),
    });
  }

  hitBomb(_bomb: any, player: Phaser.GameObjects.Sprite) {
    _bomb.alpha = 0;
    game.gameOver("You hit a bomb.");
  }

  hitSpike(_a, _b) {
    game.gameOver("You landed on a spike.");
  }

  hitChest(player, chest) {}
}
