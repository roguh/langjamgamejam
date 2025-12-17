import { Math, Scene, Input } from "phaser";

const DEV_MODE = true;
const PHI = 0.5 + 5 ** 0.5 * 0.5;
// const MAX_DIALOGUE = ".......................".length; // for width 300*PHI
const MAX_DIALOGUE = "................................".length; // for width 400*PHI
const EMPTY_DIALOGUE = null;
const INIT_DIALOGUE = "";

const playerConst = {
  horiz: 160,
  dashHoriz: 400,
  vert: 430,
  dashActivation: 750, // milliseconds between button  mashes
  dashDuration: 1000,
  dialogueDistance: 150,
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
  public dialogueBox: Phaser.GameObjects.GameObject;
  public dialogueText: Phaser.GameObjects.Text;
  public dialogueHow: Phaser.GameObjects.Text;
  public player: Phaser.Physics.Arcade.Sprite;
  public squirrel: Phaser.Physics.Arcade.Sprite;
  public squirrels: Phaser.Physics.Arcade.Group;
  public conversationalists: Record<string, Phaser.Physics.Arcade.Sprite>;
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
        .text(512, 384, "Axelloni: A narrative platformer.", {
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
          512,
          384 + 70,
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
      this.add.rectangle(512, 384, 1024, 768, 0xff1964).setDepth(99),
    ];
    const viewTime = DEV_MODE ? 0.01 : 1;
    const transTime = DEV_MODE ? 1 : 4;
    text.forEach((txt) =>
      this.time.addEvent({
        delay: viewTime * 1000,
        callback: () => {
          this.tweens.add({
            targets: txt,
            y: -384,
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

    this.bombs
      .create(400, 300, "bomb")
      .setBounce(1)
      .setVelocity(Math.Between(-200, 200), 20);

    this.dialogueBox = this.add
      .rectangle(32, 32, 400 * PHI, (400 * PHI) / 4, 0xff1964)
      .setOrigin(0)
      .setDepth(95);
    this.dialogueBox.alpha = 0.8;
    this.dialogueHow = this.add
      .text(this.dialogueBox.body?.position.x ?? 0 + 32 * 1.5, 0, "", {
        fontSize: "24px",
        color: "#000",
        stroke: "#333",
        strokeThickness: 2,
      })
      .setOrigin(0)
      .setDepth(95);
    this.dialogueText = this.add
      .text(
        this.dialogueBox.body?.position.x ?? 0 + 32 * 1.5,
        this.dialogueBox.body?.position.y ?? 0 + 32 * 1.5,
        "",
        {
          fontSize: "32px",
          color: "#000",
          stroke: "#333",
          strokeThickness: 2,
        },
      )
      .setOrigin(0)
      .setDepth(96);
  }

  continueDialogue(): string | null {
    console.log(this.dialogueState, this.currentDialogue);
    // Return the next line of dialogue, if any

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
    this.dialogueBox.alpha = 0.8;
    this.dialogueHow.alpha = 1;
    this.dialogueText.alpha = 1;

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
      this.dialogueHow.setText("Squirrel: " + "");
    } else {
      this.dialogueText.setText(wrap(this.currentDialogue, MAX_DIALOGUE));
      this.dialogueHow.setText(
        "Squirrel: " +
          (this.dialogueState === "Stopped" ? "" : "Press SPACE to continue"),
      );
    }
  }

  closeDialogue() {
    // Hide box
    this.dialogueBox.alpha = 0;
    this.dialogueHow.alpha = 0;
    this.dialogueText.alpha = 0;
    // Reset animation, if any
    this.playerTimes.dialogueStart = this.time.now;
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
