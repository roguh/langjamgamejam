import { Math, Scene, Input } from "phaser";

const [W, H] = [1024, 768];

const DEV_MODE = true;
const PHI = 0.5 + 5 ** 0.5 * 0.5;
// const MAX_DIALOGUE = ".......................".length; // for width 300*PHI
// const MAX_DIALOGUE = "................................".length; // for width 400*PHI
const MAX_DIALOGUE = ((W / 10) * 0.95) | 0; // for width 1024
const MAX_CHOICES = 3;
const EMPTY_DIALOGUE = null;
const INIT_DIALOGUE = "";

const playerConst = {
  horiz: 160,
  dashHoriz: 400,
  vert: 430,
  dashActivation: 750, // milliseconds between button  mashes
  dashDuration: 1000,
  dialogueDistance: 150,
  itemDistance: 150,
  ////  dialogueDistance: 700,
};

type DialogueExecutionState =
  | "Stopped"
  | "WaitingOnOptionSelection"
  | "WaitingOnContinue"
  | "Running";

const wrap = (str: string, maxLen: number): string[] => {
  // implement wrap logic here
  const words = str.split(" ");
  const lines = [];
  let currentLine = "";

  for (const word of words) {
    if (currentLine.length + word.length + 1 > maxLen) {
      lines.push(currentLine);
      currentLine = word;
    } else {
      currentLine += (currentLine.length > 0 ? " " : "") + word;
    }
  }

  if (currentLine.length > 0) {
    lines.push(currentLine);
  }

  return lines;
};

export class Game extends Scene {
  public currentDialogue: string;
  public dialogueState: DialogueExecutionState;
  public dialogueVM: string[];
  public platforms: Phaser.Physics.Arcade.StaticGroup;
  public bombs: Phaser.Physics.Arcade.Group;
  public dialogueElements: (
    | Phaser.GameObjects.GameObject
    | Phaser.GameObjects.Text
  )[] = [];
  public dialogueChoiceElements: (
    | Phaser.GameObjects.GameObject
    | Phaser.GameObjects.Text
  )[] = [];
  public dialogueChoices: Phaser.GameObjects.Text[];
  public dialogueBox: Phaser.GameObjects.GameObject;
  public dialogueText: Phaser.GameObjects.Text;
  public dialogueHow: Phaser.GameObjects.Text;
  public statusText: Phaser.GameObjects.Text;
  public itemText: Phaser.GameObjects.Text;
  public itemBack: Phaser.GameObjects.GameObject[];
  public player: Phaser.Physics.Arcade.Sprite;
  public squirrel: Phaser.Physics.Arcade.Sprite;
  public squirrels: Phaser.Physics.Arcade.Group;
  public conversationalists: Record<string, Phaser.Physics.Arcade.Sprite>;
  public items: Record<string, Phaser.GameObjects.GameObject>;
  public playerTimes = {
    grounded: 0,
    releaseLeft: 0,
    releaseRight: 0,
    dash: 0,
    dialogueStart: 0,
  };
  public coyoteTime: number = 200; // milliseconds
  public cursors?: Phaser.Types.Input.Keyboard.CursorKeys;

  constructor() {
    super("Game");
    this.dialogueState = "WaitingOnContinue";
    this.dialogueVM = [
      "...",
      "Hello, fair traveler!",
      "The Tree is found deeper into the woods towards the bottom-right side of your screen :)",
    ];
    this.currentDialogue = INIT_DIALOGUE;
  }

  preload() {
    this.load.setPath("assets");

    this.load.image("background", "bg.png");
    // 400px,32px
    this.load.image("ground", "platform.png");
    this.load.image("star", "star.png");
    this.load.image("bomb", "bomb.png");
    this.load.spritesheet("thedude", "dude.png", {
      frameWidth: 32,
      frameHeight: 48,
    });
    this.load.spritesheet("squirrel", "AzdnerSquirrelIdle.png", {
      frameWidth: 80,
      frameHeight: 48,
    });
    // TODO Vera Orion
    // TODO tree
    // TODO drone
  }

  create() {
    // 512,384 is the center of the screen
    this.add.image(512, 384, "background");
    const text = [
      this.add
        .text(W / 2, H / 2, "Axelloni: A narrative platformer.", {
          fontFamily: "Arial Black",
          fontSize: 38,
          color: "#ffffff",
          stroke: "#000000",
          strokeThickness: 8,
          align: "center",
        })
        .setOrigin(0.5)
        .setDepth(100),
      this.add
        .text(
          W / 2,
          H / 2 + 70,
          "Made by your favorite game studio in the Mountain States of the U.S.A.\nNo generative AI.",
          {
            fontFamily: "Arial Black",
            fontSize: 27,
            color: "#ffffff",
            stroke: "#000000",
            strokeThickness: 3,
            align: "center",
          },
        )
        .setOrigin(0.5)
        .setDepth(100),
      this.add.rectangle(W / 2, H / 2, W, H, 0xff1964).setDepth(99),
    ];
    const viewTime = DEV_MODE ? 0.01 : 1;
    const transTime = DEV_MODE ? 1 : 4;
    text.forEach((txt) =>
      this.time.addEvent({
        delay: viewTime * 1000,
        callback: () => {
          this.tweens.add({
            targets: txt,
            y: -H / 2,
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
      }),
    );

    this.platforms = this.physics.add.staticGroup();
    // Refresh required after scaling
    this.platforms.create(400, 568, "ground").setScale(2).refreshBody();
    this.platforms.create(600, 400, "ground");
    this.platforms.create(50, 250, "ground");
    this.platforms.create(750, 220, "ground");

    // The Dude abides
    this.player = this.physics.add
      .sprite(100, 450, "thedude")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(1.5)
      .refreshBody();
    /// This allows you to define a single animation once and apply it to as many Game Objects as you require.
    this.anims.create({
      key: "left",
      frames: this.anims.generateFrameNumbers("thedude", { start: 0, end: 3 }),
      frameRate: 14,
      //  The 'repeat -1' value tells the animation to loop.
      repeat: -1,
    });
    this.anims.create({
      key: "right",
      frames: this.anims.generateFrameNumbers("thedude", { start: 5, end: 8 }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "leftDodge",
      frames: this.anims.generateFrameNumbers("thedude", {
        start: 11,
        end: 12,
      }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "rightDodge",
      frames: this.anims.generateFrameNumbers("thedude", { start: 9, end: 10 }),
      frameRate: 14,
      repeat: -1,
    });
    this.anims.create({
      key: "turn",
      frames: [{ key: "thedude", frame: 4 }],
      frameRate: 20,
    });

    this.squirrels = this.physics.add.group();
    this.squirrel = this.squirrels
      .create(600, 350, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(1.5)
      .refreshBody();
    this.squirrels
      .create(400, 350, "squirrel")
      .setBounce(0.2)
      .setCollideWorldBounds(true)
      .setScale(1.5);
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
    this.squirrel.flipX = true;
    this.anims.create({
      key: "squirrel-idle",
      frames: this.anims.generateFrameNumbers("squirrel", {
        start: 0,
        end: 10,
      }),
      frameRate: 5,
      repeat: -1,
    });

    this.bombs = this.physics.add.group();

    this.cursors = this.input.keyboard?.createCursorKeys();
    this.physics.add.collider(this.player, this.platforms);
    this.physics.add.collider(this.bombs, this.platforms);
    this.physics.add.collider(this.squirrels, this.platforms);
    this.physics.add.collider(this.player, this.bombs, this.hitBomb);

    this.conversationalists = {
      SquirrelYarn: this.squirrel,
    };
    this.items = {};
    this.squirrels.children.iterate((child) => {
      this.items[`A squirrel sits watching another squirrel attentively.`] =
        child;
    });

    this.bombs
      .create(400, 300, "bomb")
      .setBounce(1)
      .setVelocity(Math.Between(-200, 200), 20);

    this.statusText = this.add
      .text(32 * 1.5, 32 * 1.5, "Health: +++++\nFuel: 0", {
        fontSize: "22px",
        color: "#000",
        stroke: "#333",
        strokeThickness: 1,
      })
      .setOrigin(0)
      .setDepth(96);

    this.itemBack = [
      this.add
        .rectangle(W - 32 * 10.5, 32, 32 * 10, (32 * 10) / PHI, 0xff1964)
        .setOrigin(0)
        .setDepth(95),
    ];
    this.itemText = this.add
      .text(W - 32 * 10, 32 * 1.5, "", {
        fontSize: "22px",
        color: "#000",
        stroke: "#333",
        strokeThickness: 1,
      })
      .setOrigin(0)
      .setDepth(96);

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
    this.dialogueChoices = new Array(MAX_CHOICES).fill(undefined).map((_, i) =>
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
    );
    const dialogueChoiceBack = [
      ...this.dialogueChoices.map((_, i) =>
        this.add
          .text(32 * PHI, H - dialogueH - 64 - 32 * i - 12, `> ${i + 1}`, {
            fontSize: "24px",
            color: "#ff1964",
            stroke: "#ff1964",
            strokeThickness: 1,
          })
          .setOrigin(0)
          .setDepth(96),
      ),
      ...this.dialogueChoices.map((_, i) =>
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
      ),
      ...this.dialogueChoices.map((_, i) =>
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
      ),
    ];
    this.dialogueChoiceElements = [
      ...this.dialogueChoices,
      ...dialogueChoiceBack,
    ];
    this.dialogueElements = [
      this.dialogueBox,
      this.dialogueHow,
      dialogueHowBox,
      this.dialogueText,
      ...this.dialogueChoices,
      ...dialogueChoiceBack,
    ];
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

  updateDialogue(_yarnNodeName: string, obj: Phaser.GameObjects.GameObject) {
    let next;
    this.dialogueElements.map((e) => (e.alpha = 1));
    if (this.dialogueState !== "WaitingOnOptionSelection") {
      this.dialogueChoiceElements.forEach((e) => (e.alpha = 0));
    }
    this.dialogueBox.alpha = 0.65;
    const name = "Squirrel Priest";

    if (this.cursors?.space && Input.Keyboard.JustUp(this.cursors?.space)) {
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

  updateInventory(desc: string, obj) {
    this.itemBack.forEach((e) => (e.alpha = 0.65));
    this.itemText.alpha = 1;
    this.itemText.setText(wrap(desc, "A squirrel sites watc".length));
  }
  closeInventory() {
    this.itemBack.forEach((e) => (e.alpha = 0));
    this.itemText.alpha = 0;
  }

  update() {
    this.squirrels.children.iterate((s) =>
      s.anims.play(
        { key: "squirrel-idle", frameRate: Math.Between(3, 8) },
        true,
      ),
    );
    const talkers = Object.entries(this.conversationalists).filter(
      ([yarnNode, obj]) =>
        Math.Distance.Between(this.player.x, this.player.y, obj.x, obj.y) <
        playerConst.dialogueDistance,
    );
    if (talkers.length > 0) {
      this.updateDialogue(...talkers[0]);
    } else {
      this.closeDialogue();
    }
    let nearbyItems = Object.entries(this.items).filter(
      ([key, item]) =>
        Math.Distance.Between(
          this.player.x,
          this.player.y,
          item.body?.position.x ?? -Infinity,
          item.body?.position.y ?? -Infinity,
        ) < playerConst.itemDistance,
    );
    if (nearbyItems.length > 0) {
      this.updateInventory(...nearbyItems[0]);
    } else {
      this.closeInventory();
    }

    // Although we've added a lot of code it should all be pretty readable.

    // Basic movement controls set velocity directly
    const isDashing =
      this.time.now - this.playerTimes.dash < playerConst.dashDuration;
    const horizSpeed = isDashing ? playerConst.dashHoriz : playerConst.horiz;
    if (this.cursors?.left.isDown) {
      // 160 px/sec
      this.player.setVelocityX(-horizSpeed);
      // TODO smaller hitbox
      // TODO move slower, slower dash too
      this.player.anims.play(
        this.cursors?.down.isDown ? "leftDodge" : "left",
        true,
      );
    } else if (this.cursors?.right.isDown) {
      this.player.setVelocityX(horizSpeed);
      this.player.anims.play(
        this.cursors?.down.isDown ? "rightDodge" : "right",
        true,
      );
    } else {
      this.player.setVelocityX(0);
      this.player.anims.play(this.cursors?.down.isDown ? "rightDodge" : "turn");
    }

    // MESSSS!!
    if (
      this.cursors?.shift.isDown &&
      (this.cursors?.left.isDown || this.cursors?.right.isDown)
    ) {
      if (this.time.now - this.playerTimes.dash > 2 * playerConst.dashDuration)
        this.playerTimes.dash = this.time.now;
    }

    // Coyote Time: allow jumping even after falling off a platform
    if (
      this.cursors?.up.isDown &&
      (this.player.body?.touching.down ||
        this.time.now - this.playerTimes.grounded < this.coyoteTime)
    ) {
      this.player.setVelocityY(-430);
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
      this.player.setGravityY(600);
    } else {
      this.player.setGravityY(300);
    }
    // Allow fine-grained jump height control by releasing up key early
    if (this.cursors?.up && Input.Keyboard.JustUp(this.cursors?.up)) {
      if ((this.player.body?.velocity.y || 0) < 0) {
        this.player.setVelocityY(0);
      }
    }
  }

  hitBomb(_a: any, _b: any) {
    this.physics.pause();
    this.player.setTint(0xff0000);
    this.player.anims.play("turn");
  }
}
