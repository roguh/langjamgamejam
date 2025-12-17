import { Scene } from "phaser";

const DEV_MODE = true;

export class Game extends Scene {
  public platforms: Phaser.Physics.Arcade.StaticGroup;
  public player: Phaser.Physics.Arcade.Sprite;
  public cursors?: Phaser.Types.Input.Keyboard.CursorKeys;

  constructor() {
    super("Game");
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
          "Made by humans in the Mountain States of the U.S.A. No generative AI.",
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
    this.player = this.physics.add.sprite(100, 450, "thedude");
    this.player.setBounce(0.2);
    this.player.setCollideWorldBounds(true);
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
      key: "turn",
      frames: [{ key: "thedude", frame: 4 }],
      frameRate: 20,
    });

    this.cursors = this.input.keyboard?.createCursorKeys();
    this.physics.add.collider(this.player, this.platforms);
  }

  update() {
    // Although we've added a lot of code it should all be pretty readable.
    if (this.cursors?.left.isDown) {
      // 160 px/sec
      this.player.setVelocityX(-160);
      this.player.anims.play("left", true);
    } else if (this.cursors?.right.isDown) {
      this.player.setVelocityX(160);
      this.player.anims.play("right", true);
    } else {
      this.player.setVelocityX(0);
      this.player.anims.play("turn");
    }
    if (this.cursors?.up.isDown && this.player.body?.touching.down) {
      this.player.setVelocityY(-330);
    }
  }
}
